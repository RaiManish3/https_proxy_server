module ThreadProxy where

import Data.Bits
import Network.Socket hiding (recv, send)
import Network.BSD
import System.IO
import Network.Socket.ByteString (recv, send, sendAll)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (unless, forever, void)

import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C

import Data.Map as Map hiding (drop)
import HttpParser
import Common

type TidSocketInfo = Map.Map ThreadId (Maybe Socket, Maybe Socket)   
                    -- threadId : (clientsock, Socket)

data TerminalInfo = 
  TerminalInfo {
     buffer :: HttpData
   , socketD :: Socket
   , socketA :: SockAddr
   , packetParserInfo :: HttpParserInfo
   , packetData :: HttpPacket
   , stillToProcess :: HttpData
   , isClosed :: Bool
   }

processClient :: (Socket, SockAddr) -> MVar TidSocketInfo -> IO ()
processClient (conn, clientaddr) tidList = withSocketsDo $
  do
    -- | initialise data structures and populate the TidSocketInfo
    thisThreadId <- myThreadId
    tids <- takeMVar tidList
    let newTids = Map.insert thisThreadId (Just conn, Nothing) tids
    putMVar tidList newTids

    let clientInfo = emptyTerminal conn clientaddr HttpRequestParser

    -- | recv the request from client and parse it completely
    clientInfo' <- getRequestPacket clientInfo

    -- | check whether headers satisfy your criteria
    -- | in case of error send the appropriate error packet back to the client
    flagHeaders <- checkHeaderErrors (packetData clientInfo') conn tidList
    case flagHeaders of
      False -> do
        (clientInfo'', tidList') <- closeSocket conn clientInfo' tidList
        return ()
      _ -> processRequest clientInfo' tidList


processRequest cI tidList = withSocketsDo $
  do
    let hP = packetData cI
        Just x = Map.lookup "host" (hHeaders hP)
        hostname = takeWhile (\x -> x /= ':') (snd x)
        port = getPort (drop (length hostname + 1) (snd x))
        getPort "" = "80"
        getPort x = x

    -- | print the request to the screen
    tids <- takeMVar tidList
    putStrLn (hMethod hP ++ " " ++ hUrl hP)
    putMVar tidList tids

    -- | make a socket to communicate to the destination server
    let hints = defaultHints {
            addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
          , addrFamily = AF_INET
        }
    addrInfo <- getAddrInfo (Just hints) (Just hostname) (Just port)
    let destAddr = head addrInfo

    -- | print the request to the screen
    tids <- takeMVar tidList
    putStrLn (show destAddr)
    putMVar tidList tids

    dSock <- socket (addrFamily destAddr) (addrSocketType destAddr) (addrProtocol destAddr)
    connect dSock (addrAddress destAddr)

    -- | contruct TerminalInfo for the destination server too
    let sI = emptyTerminal dSock (addrAddress destAddr) HttpResponseParser

    -- | implement CONNECT method
    case (hMethod hP) of
      "CONNECT" -> do
        let cI' = queue cI connectEstKPacket
        requestResponseCycle cI' sI tidList
      _ -> do
        let sI' = queue sI (buildPacket hP ["proxy-connection","connection","keep-alive"] [("Connection","Close")])
        requestResponseCycle cI sI' tidList

    -- | TESTER
    tids <- takeMVar tidList
    putStrLn ("Exiting thread....")
    putMVar tidList tids


requestResponseCycle cI sI tidList = withSocketsDo $
  do
    -- | give write actions preference 
    (cI', sI') <- handleWList cI sI

    -- | receive from the server until it closes its side of connection
    (cI'', sI'', tidList') <- handleRList cI' sI' tidList


    -- | close client socket when server socket is closed and and its buffer is empty
    -- | ??? is above a good measure ??? || add time elapsed as a factor too
    if isClosed sI'' && buffer cI == "" then do
                                        (cI''', tidList'') <- closeSocket (socketD cI'') cI'' tidList
                                        return ()
                                        else requestResponseCycle cI'' sI'' tidList


handleWList cI sI = withSocketsDo $
  do
    cI' <- handleWTerminal cI
    sI' <- handleWTerminal sI
    return (cI', sI')

handleWTerminal tI = withSocketsDo $
  do
    if not (isClosed tI) && buffer tI /= "" then flush tI
                                            else return tI

-- | PROBLEM LIES HERE --------------------------------------------------------########
handleRList cI sI tidList = withSocketsDo $
  do
    (sI', cI', tidList') <- handleRTerminal sI cI tidList

    (cI'', sI'', tidList'') <- handleRTerminal cI sI tidList
    -- | print the request to the screen
    tids <- takeMVar tidList''
    putStrLn ("Server buffer is:\n" ++ buffer sI'')
    putStrLn ("Client buffer is:\n" ++ buffer cI'')
    putMVar tidList'' tids

    return (cI'', sI'', tidList'')


handleRTerminal rI sI tidList = withSocketsDo $
  do
    if not (isClosed sI) then do
                         msg <- recv (socketD sI) 1024
                         if C.length msg == 0 && (hType (packetParserInfo sI) /= HttpRequestParser)
                            then do
                              (sI', tidList') <- closeSocket (socketD sI) sI tidList
                              return (rI, sI', tidList')
                            else do
                              let rI' = queue rI (C.unpack msg)
                              return (rI', sI, tidList)
                         else return (rI, sI, tidList)


-- | need to repair this function
getRequestPacket cI = withSocketsDo $
  do
    msg <- recv (socketD cI) 1024
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

emptyTerminal sock addr pType = TerminalInfo{
  buffer = ""
, socketD = sock
, socketA = addr
, packetParserInfo = emptyHttpParserInfo pType
, packetData = emptyHttpPacket
, stillToProcess = ""
, isClosed = False
}

updateInfoAndPacket cI xs hI hP = TerminalInfo{
  buffer = buffer cI
, socketD = socketD cI
, socketA = socketA cI
, packetParserInfo = hI
, packetData = hP
, stillToProcess = xs
, isClosed = isClosed cI
}

queue tI newData = TerminalInfo{
  buffer = buffer tI ++ newData
, socketD = socketD tI
, socketA = socketA tI
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
      fe = fst entry
  if fe == Just conn then do
                     close conn
                     putMVar tidList (updatingMap thisThreadId (Nothing, snd entry) tids)
                     return (closeSocket' tI, tidList)
                     else do
                     close conn
                     putMVar tidList (updatingMap thisThreadId (fe, Nothing) tids)
                     return (closeSocket' tI, tidList)

closeSocket' tI= TerminalInfo{
  buffer = buffer tI
, socketD = socketD tI
, socketA = socketA tI
, packetParserInfo = packetParserInfo tI
, packetData = packetData tI
, stillToProcess = stillToProcess tI
, isClosed = True
}
  

flush tI = withSocketsDo $
  do
    x <- send (socketD tI) (C.pack (buffer tI))   -- should we use sendAll here
    return TerminalInfo{
              buffer = drop x (buffer tI)
            , socketD = socketD tI
            , socketA = socketA tI
            , packetParserInfo = packetParserInfo tI
            , packetData = packetData tI
            , stillToProcess = stillToProcess tI
            , isClosed = isClosed tI
            }
