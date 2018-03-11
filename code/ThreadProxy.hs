module ThreadProxy where

import Data.Bits
import Network.Socket hiding (recv)
import Network.BSD
import System.IO
import Network.Socket.ByteString (recv, sendAll)

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

import Data.List

type TidSocketInfo = [(ThreadId, Socket)]

data ClientToServer = ClientToServer {
                                       clientSock :: Socket
                                     , serverSock :: Socket
                                    -- requestParser :: RequestParser
                                    -- responseParser :: ResponseParser
                                     }

processClient :: (Socket, SockAddr) -> MVar TidSocketInfo -> IO ()
processClient (conn, clientaddr) tidList = withSocketsDo $
  do
    -- add the thread and client sock into the list
    {- tids <- takeMVar tidList -}
    {- let newTids = (myThreadId, conn) -}
    {- putMVar tidList newTids -}

    -- accept request from the client
    msg <- recv conn 1024      -- msg is a bytestring
    putStrLn (show msg)

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
