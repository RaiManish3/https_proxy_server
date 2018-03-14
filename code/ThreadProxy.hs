module ThreadProxy where

import Data.Bits
import Network.Socket hiding (recv, send)
import Network.BSD
import System.IO
import Network.Socket.ByteString (recv, send, sendAll)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (unless, forever, void)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C

import Data.Map as Map hiding (drop)
import HttpParser
import Common

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

processClient :: (Socket, SockAddr) -> MVar TidSocketInfo -> IO ()
processClient (conn, clientaddr) tidList = withSocketsDo $
  do
    -- | initialise data structures and populate the TidSocketInfo
    thisThreadId <- myThreadId
    tids <- takeMVar tidList
    let newTids = Map.insert thisThreadId (Just conn, Nothing) tids
    putMVar tidList newTids

    let clientInfo = emptyTerminal conn HttpRequestParser

    -- | recv the request from client and parse it completely
    clientInfo' <- getRequestPacket clientInfo

    -- | check whether headers satisfy your criteria
    -- | in case of error send the appropriate error packet back to the client
    flagHeaders <- checkHeaderErrors (packetData clientInfo') conn tidList
    case flagHeaders of
      False -> do
        closeSocket conn clientInfo' tidList
        return ()
      _ -> processRequest clientInfo' tidList clientaddr

    tids <- takeMVar tidList
    putStrLn (show (Map.size tids))
    putMVar tidList tids


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
    connect dSock (addrAddress destAddr)

    -- | contruct TerminalInfo for the destination server too
    let sI = emptyTerminal dSock HttpResponseParser

    -- | implement CONNECT method
    case (hMethod hP) of
      "CONNECT" -> do
        let cI' = queue cI (C.pack connectEstKPacket)
        requestResponseCycle cI' sI tidList
      _ -> do
        let sI' = queue sI (C.pack (buildPacket hP ["proxy-connection","connection","keep-alive"] [("Connection","Close")]))
        requestResponseCycle cI sI' tidList


requestResponseCycle cI sI tidList = withSocketsDo $
  do
    -- | IDEA is to create single write thread for both sockets
    -- | and two seperate read threads for the server and client

    -- | make a lock which has cI and sI data
    termMVar <- newMVar (cI, sI)
    tidServ <- forkIO $ handleRServer termMVar tidList (socketD sI)
    forkIO $ handleRClient termMVar tidList (socketD cI)

    -- | update the tidList to account for the new threads
    thisThreadId <- myThreadId
    tids <- takeMVar tidList
    let x = Map.lookup thisThreadId tids
    case x of
      Nothing -> do
        putMVar tidList tids
      Just (csock, ssock) -> do let newTids = Map.insert tidServ (Nothing, ssock) 
                                        (updatingMap thisThreadId (csock, Nothing) tids)
                                putMVar tidList newTids

    -- | continue the write process in this thread only
    handleWList termMVar tidList


handleRServer termMVar tidList sock = withSocketsDo $
  do
    msg <- recv sock receiveSize

    if C.length msg == 0
       then do
         -- | update the socket status to closed
         (cI,sI) <- takeMVar termMVar
         (sI', tidList') <- closeSocket sock sI tidList
         putMVar termMVar (cI, sI')
       else do
         (cI,sI) <- takeMVar termMVar
         let cI' = queue cI msg
         putMVar termMVar (cI', sI)
         handleRServer termMVar tidList sock


handleRClient termMVar tidList sock = withSocketsDo $
  do
    msg <- recv sock receiveSize

    if C.length msg == 0
       then do
         (cI, sI) <- takeMVar termMVar
         putMVar termMVar (cI, sI)
       else do
         (cI, sI) <- takeMVar termMVar
         let sI' = queue sI msg
         putMVar termMVar (cI, sI')
         handleRClient termMVar tidList sock


handleWList termMVar tidList = withSocketsDo $
  do
    (cI, sI) <- takeMVar termMVar
    cI' <- handleWTerminal cI
    putMVar termMVar (cI', sI)

    (cI'', sI') <- takeMVar termMVar
    sI'' <- handleWTerminal sI'
    putMVar termMVar (cI'', sI'')

    if isClosed sI'' && C.null (buffer cI'')
       then do
         (cI''', tidList') <- closeSocket (socketD cI'') cI'' tidList
         return ()
       else do
         handleWList termMVar tidList


handleWTerminal tI = withSocketsDo $
  do
    if not (isClosed tI || C.null (buffer tI))
       then flush tI
       else return tI


-- | need to repair this function
getRequestPacket cI = withSocketsDo $
  do
    msg <- recv (socketD cI) receiveSize
    let hI = packetParserInfo cI
        hP = packetData cI 
        (leftover, hI', hP') = scanner (stillToProcess cI ++ C.unpack msg) hI hP
        cI' = updateInfoAndPacket cI leftover hI' hP'
    if hState hI' /= ReceivedBody then getRequestPacket cI'
                                  else return cI'


checkHeaderErrors hP conn tidList = withSocketsDo $
  do
    let hasHostHeader = Map.member "host" (hHeaders hP)
    case hasHostHeader of
      False -> do
        sendAll conn (C.pack badRequestPacket)
        return False
      _ -> case checkMethod (hMethod hP) of
             False -> do
               sendAll conn (C.pack methodNotAllowed)
               return False
             _ ->  case checkVersion (hVersion hP) of
                     False -> do
                       sendAll conn (C.pack badRequestPacket)
                       return False
                     _ -> let Just x = Map.lookup "host" (hHeaders hP)
                           in case checkBlocked (snd x) of
                                True -> do
                                  tids <- takeMVar tidList
                                  putStrLn (hMethod hP ++ hUrl hP ++ " [FILTERED]")
                                  putMVar tidList tids
                                  sendAll conn (C.pack forbiddenPacket)
                                  return False
                                _ -> return True


updatingMap k v m = update (\_ -> Just v) k m

checkMethod xs = xs `elem` allowedMethods
checkVersion xs = xs `elem` allowedVersion
checkBlocked y = checkBlocked' blockedDomain
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
  let Just entry = Map.lookup thisThreadId tids
      (fe, se) = entry
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
  

flush tI = withSocketsDo $
  do
    x <- send (socketD tI) (buffer tI)
    return TerminalInfo{
              buffer = C.drop x (buffer tI)
            , socketD = socketD tI
            , packetParserInfo = packetParserInfo tI
            , packetData = packetData tI
            , stillToProcess = stillToProcess tI
            , isClosed = isClosed tI
            }

receiveSize = 8192
