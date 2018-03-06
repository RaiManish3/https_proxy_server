module ThreadProxy where

import Data.Bits
import Network.Socket hiding (recv)
import Network.BSD
import System.IO
import Network.Socket.ByteString (recv, sendAll)

import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.Char8 (unpack)


type TidSocketInfo = [(ThreadId, Socket)]

data ClientToServer = ClientToServer {
                                    clientSock :: Socket,
                                    serverSock :: Socket,
                                    -- requestParser :: RequestParser
                                    -- responseParser :: ResponseParser
                                     }

processClient :: (Socket, SockAddr) -> MVar TidSocketInfo -> IO ()
processClient (conn, clientaddr) tidList = withSocketsDo $
  do
    -- add the thread and client sock into the list
    tids <- takeMVar tidList
    let newTids = (myThreadId, conn)
    putMVar tidList newTids

    -- accept request from the client
    msg <- recv conn 1024 
    putStrLn (unpack msg)
    unless (S.null msg) $ do
      processClient (conn, clientaddr)

