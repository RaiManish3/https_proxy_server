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

    let bP = buildPacket (packetData clientInfo') [] []
    putStrLn (show bP)

    {- tids <- takeMVar tidList -}
    {- let x = parseRequest (C.unpack msg)  -- self made method -}
    {- putStrLn (show x) -}
    {- putMVar tidList tids -}
    
    {- -- build a client sock' to communicate with the destination server -}
    {- addrInfo <- getAddrInfo Nothing (Just "geeksforgeeks.org") (Just $ "80") -}
    {- let serverAddr = head addrInfo -}
    {- dSock <- socket AF_INET Stream defaultProtocol -}
    {- -- add dSock into TiDSocketInfo -}
    {- connect dSock (addrAddress serverAddr) -}

    {- sendAll dSock msg -}
    {- destmsg <- recv dSock 1024 -}
    {- {- putStrLn (C.unpack destmsg) -} -}


getRequestPacket cI = withSocketsDo $
  do
    msg <- recv (socketD cI) 1024
    let hI = packetParserInfo cI
        hP = packetData cI 
        (leftover, hI', hP') = scanner (stillToProcess cI ++ C.unpack msg) hI hP
        cI' = updateInfoAndPacket cI leftover hI' hP'
    if hState hI' /= ReceivedBody then getRequestPacket cI'
                                  else return cI'


updatingMap k v m = update (\_ -> Just v) k m

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
                     return (tidList, closeSocket' tI)
                     else do
                     close conn
                     putMVar tidList (updatingMap thisThreadId (fe, Nothing) tids)
                     return (tidList, closeSocket' tI)

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
