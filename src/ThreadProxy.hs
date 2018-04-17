module ThreadProxy where

import System.IO
import Network.BSD
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send, sendAll)

import Control.Concurrent
import Control.Concurrent.MVar
import System.Timeout (timeout)
import Control.Exception (try, SomeException)

import qualified Data.ByteString.Char8 as C

import Data.Map as Map hiding (drop)
import HttpParser
import Common
import Stats

type TidSocketInfo = Map.Map ThreadId (Maybe Socket, Maybe Socket)

data TerminalInfo = 
  TerminalInfo {
     buffer :: C.ByteString
   , socketD :: Socket
   , packetParserInfo :: HttpParserInfo
   , packetData :: HttpPacket
   , stillToProcess :: HttpData
   , isClosed :: Bool
   }
   deriving Show

processClient :: (Socket, SockAddr) -> MVar TidSocketInfo -> [String] -> MVar  RequestStat -> IO ()
processClient (conn, clientaddr) tidList args rstat = withSocketsDo $
  do
    -- | initialise data structures and populate the TidSocketInfo
    thisThreadId <- myThreadId
    tids <- takeMVar tidList
    let newTids = Map.insert thisThreadId (Just conn, Nothing) tids
    putMVar tidList newTids

    let clientInfo = emptyTerminal conn HttpRequestParser

    -- | recv the request from client and parse it completely
    (clientInfo', allGood) <- getRequestPacket clientInfo

    -- | check whether headers satisfy your criteria
    -- | in case of error send the appropriate error packet back to the client
    if allGood 
       then do
       flagHeaders <- checkHeaderErrors (packetData clientInfo') conn tidList args rstat
       case flagHeaders of
         False -> do
           closeSocket conn clientInfo' tidList
           return ()
         _ -> processRequest clientInfo' tidList clientaddr
      else
        return ()


processRequest cI tidList clientaddr = withSocketsDo $
  do
    let hP = packetData cI
        Just x = Map.lookup "host" (hHeaders hP)
        hostname = takeWhile (\x -> x /= ':') (snd x)
        port = getPort (drop (length hostname + 1) (snd x))
        getPort "" = "80"
        getPort x = x

    -- | print the request to the screen
    tids <- takeMVar tidList
    putStrLn (show clientaddr ++ " " ++ hMethod hP ++ " " ++ hUrl hP)
    putMVar tidList tids

    -- | make a socket to communicate to the destination server
    let hints = defaultHints {
            addrSocketType = Stream
          , addrFamily = AF_INET
        }
    addrInfo <- getAddrInfo (Just hints) (Just hostname) (Just port)
    let destAddr = head addrInfo
    dSock <- socket (addrFamily destAddr) (addrSocketType destAddr) (addrProtocol destAddr)
    x <- try (connect dSock (addrAddress destAddr)) :: IO (Either SomeException ())
    case x of
      Left e -> return ()
      Right x' -> do
        -- | contruct TerminalInfo for the destination server too
        let sI = emptyTerminal dSock HttpResponseParser

        case (hMethod hP) of
          "CONNECT" -> do
            let cI' = queue cI (C.pack connectEstKPacket)
            requestResponseCycle cI' sI tidList
          _ -> do
            let sI' = queue sI (C.pack (buildPacket hP ["proxy-connection","connection","keep-alive"] [("Connection","Close")]))
            requestResponseCycle cI sI' tidList


requestResponseCycle cI sI tidList = withSocketsDo $
  do
    termMVar <- newMVar (cI, sI)
    thisThreadId <- myThreadId
    tids <- takeMVar tidList
    putMVar tidList (updatingMap thisThreadId (Just (socketD cI), Nothing) tids)
    requestResponseCycle' termMVar tidList (socketD sI) (socketD cI)


requestResponseCycle' termMVar tidList ssock csock = withSocketsDo $
  do
    resp1 <- try (timeout timerCount (recv ssock 2)) :: IO (Either SomeException (Maybe C.ByteString))
    case resp1 of
      Left e -> return ()
      Right msg1 -> do
        case msg1 of
          Nothing -> return ()
          Just x -> do
            thisThreadId <- myThreadId
            tids <- takeMVar tidList
            tidServ <- forkIO $ handleRServer termMVar tidList ssock x
            let y = Map.lookup thisThreadId tids
            case y of
              Nothing -> do
                putMVar tidList tids
              Just (csock', ssock') -> do let newTids = Map.insert tidServ
                                                (Nothing, Just ssock) tids
                                          putMVar tidList newTids


    resp2 <- try (timeout timerCount (recv csock 2)) :: IO (Either SomeException (Maybe C.ByteString))
    case resp2 of
      Left e -> return ()
      Right msg2 -> do
        case msg2 of
          Nothing -> return ()
          Just x -> do
            forkIO $ handleRClient termMVar tidList csock x
            return ()

    -- | continue the write process in this thread only
    handleWList termMVar tidList ssock csock


handleRServer termMVar tidList sock initMsg = withSocketsDo $
  do
    if C.length initMsg == 0
       then do
         -- | update the socket status to closed
         (cI,sI) <- takeMVar termMVar
         (sI', tidList') <- closeSocket sock sI tidList
         putMVar termMVar (cI, sI')
       else do
         resp <- try (recv sock receiveSize) :: IO (Either SomeException C.ByteString)
         case resp of
           Left e -> do
             (cI,sI) <- takeMVar termMVar
             let cI' = queue cI initMsg
             (sI', tidList') <- closeSocket sock sI tidList  -- close the socket
             putMVar termMVar (cI', sI)
           Right msg -> do
             (cI,sI) <- takeMVar termMVar
             let cI' = queue cI (C.append initMsg msg)
             putMVar termMVar (cI', sI)
             if C.length msg == 0
                then do
                  (cI,sI) <- takeMVar termMVar
                  (sI', tidList') <- closeSocket sock sI tidList
                  putMVar termMVar (cI, sI')
                else do
                  -- remove this threads entry from MVarList
                  thisThreadId <- myThreadId
                  tids <- takeMVar tidList
                  putMVar tidList (delete thisThreadId tids)


handleRClient termMVar tidList sock initMsg = withSocketsDo $
  do
    if C.length initMsg == 0
       then do
         return ()
       else do
         resp <- try (recv sock receiveSize) :: IO (Either SomeException C.ByteString)
         case resp of
           Left e -> do
             (cI,sI) <- takeMVar termMVar
             (cI', tidList') <- closeSocket sock cI tidList  -- close the socket
             putMVar termMVar (cI', sI)
           Right msg -> do
             (cI,sI) <- takeMVar termMVar
             let sI' = queue sI (C.append initMsg msg)
             putMVar termMVar (cI, sI')


handleWList termMVar tidList ssock csock = withSocketsDo $
  do
    (cI, sI) <- takeMVar termMVar
    if isClosed cI
       then do
         putMVar termMVar (cI, sI)
         return ()
       else do
         (cI', tidList') <- handleWTerminal cI tidList
         putMVar termMVar (cI', sI)
         if isClosed cI'
            then
              return ()
            else do
              (cI'', sI') <- takeMVar termMVar
              (sI'', tidList'') <- handleWTerminal sI' tidList'
              putMVar termMVar (cI'', sI'')
              if isClosed sI'' && C.null (buffer cI'')
                 then do
                   -- | should use lock here ???
                   (cI''', tidList') <- closeSocket (socketD cI'') cI'' tidList
                   return ()
                   else
                   requestResponseCycle' termMVar tidList ssock csock


handleWTerminal tI tidList = withSocketsDo $
  do
    if not (isClosed tI || C.null (buffer tI))
       then flush tI tidList
       else return (tI, tidList)


getRequestPacket cI = withSocketsDo $
  do
    resp <- try (recv (socketD cI) receiveSize) :: IO (Either SomeException C.ByteString)
    case resp of
      Left e -> return (cI, False)
      Right msg -> do
        let hI = packetParserInfo cI
            hP = packetData cI 
            (leftover, hI', hP') = scanner (stillToProcess cI ++ C.unpack msg) hI hP
            cI' = updateInfoAndPacket cI leftover hI' hP'
        if hState hI' /= ReceivedBody
           then getRequestPacket cI'
           else return (cI', True)


checkHeaderErrors hP conn tidList args rstat = withSocketsDo $
  do
    let hasHostHeader = Map.member "host" (hHeaders hP)
    case hasHostHeader of
      False -> do
        sendAll conn (C.pack badRequestPacket)
        rstat' <- takeMVar rstat
        rstat'' <- updateStat rstat' "error"
        putMVar rstat rstat''
        return False
      _ -> case checkMethod (hMethod hP) of
             False -> do
               sendAll conn (C.pack methodNotAllowed)
               rstat' <- takeMVar rstat
               rstat'' <- updateStat rstat' "error"
               putMVar rstat rstat''
               return False
             _ ->  case checkVersion (hVersion hP) of
                     False -> do
                       sendAll conn (C.pack badRequestPacket)
                       rstat' <- takeMVar rstat
                       rstat'' <- updateStat rstat' "error"
                       putMVar rstat rstat''
                       return False
                     _ -> let Just x = Map.lookup "host" (hHeaders hP)
                           in case checkBlocked (snd x) args of
                                True -> do
                                  tids <- takeMVar tidList
                                  putStrLn (hMethod hP ++ " " ++ hUrl hP ++ " [FILTERED]")
                                  putMVar tidList tids
                                  sendAll conn (C.pack forbiddenPacket)
                                  rstat' <- takeMVar rstat
                                  rstat'' <- updateStat rstat' "filtered"
                                  putMVar rstat rstat''
                                  return False
                                _ -> do
                                  rstat' <- takeMVar rstat
                                  rstat'' <- updateStat rstat' "success"
                                  putMVar rstat rstat''
                                  return True


updatingMap k v m = update (\_ -> Just v) k m

checkMethod xs = xs `elem` allowedMethods
checkVersion xs = xs `elem` allowedVersion
checkBlocked y zs = checkBlocked' zs
  where checkBlocked' [] = False
        checkBlocked' (x:xs) | x == y = True                    -- make it more expressive
          | otherwise = checkBlocked' xs

emptyTerminal sock pType = TerminalInfo{
  buffer = C.empty
, socketD = sock
, packetParserInfo = emptyHttpParserInfo pType
, packetData = emptyHttpPacket
, stillToProcess = ""
, isClosed = False
}

updateInfoAndPacket cI xs hI hP = TerminalInfo{
  buffer = buffer cI
, socketD = socketD cI
, packetParserInfo = hI
, packetData = hP
, stillToProcess = xs
, isClosed = isClosed cI
}

queue tI newData = TerminalInfo{
  buffer = C.append (buffer tI) newData
, socketD = socketD tI
, packetParserInfo = packetParserInfo tI
, packetData = packetData tI
, stillToProcess = stillToProcess tI
, isClosed = isClosed tI
}

closeSocket conn tI tidList = withSocketsDo $
  do 
  thisThreadId <- myThreadId
  tids <- takeMVar tidList
  let entry = Map.lookup thisThreadId tids
  case entry of 
    Nothing -> do
      -- | This case will happen when an exception is raised 
      --   (in recv data from server) of a thread that does not 
      --   have an entry in MVar
      putMVar tidList tids
      return (tI, tidList)
    Just (fe, se) ->
      if fe == Just conn 
         then do
           close conn
           if se == Nothing
              then do
                putMVar tidList (delete thisThreadId tids)
              else do
                putMVar tidList (updatingMap thisThreadId (Nothing, se) tids)
           return (closeSocket' tI, tidList)
         else do
           close conn
           if fe == Nothing
              then do
                putMVar tidList (delete thisThreadId tids)
              else do
                putMVar tidList (updatingMap thisThreadId (fe, Nothing) tids)
           return (closeSocket' tI, tidList)


closeSocket' tI= TerminalInfo{
  buffer = buffer tI
, socketD = socketD tI
, packetParserInfo = packetParserInfo tI
, packetData = packetData tI
, stillToProcess = stillToProcess tI
, isClosed = True
}
  

flush tI tidList = withSocketsDo $
  do
    resp <- try (send (socketD tI) (buffer tI)) :: IO (Either SomeException Int)
    case resp of
      Left e -> do
        (tI', tidList') <- closeSocket (socketD tI) tI tidList
        return (tI', tidList')
      Right x -> return (TerminalInfo{
              buffer = C.drop x (buffer tI)
            , socketD = socketD tI
            , packetParserInfo = packetParserInfo tI
            , packetData = packetData tI
            , stillToProcess = stillToProcess tI
            , isClosed = isClosed tI
           }, tidList)

receiveSize = 8192
timerCount = 500
