module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import System.IO

import Control.Concurrent
import Control.Concurrent.MVar
import System.Posix.Process (getProcessID)

import System.Exit
import System.Environment (getArgs)
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigUSR1, sigUSR2)
import System.Posix.Process

import Data.Map as Map
import ThreadProxy


main :: IO ()
main = do
  args <- getArgs
  tidList <- newMVar Map.empty
  pid <- getProcessID
  maintid <- myThreadId
  putStrLn ("Server's Process ID: " ++ show pid)

  let port = "8888"
      backlog = 5
  hostSocket port backlog tidList maintid args


type Port = String
type BackLog = Int

hostSocket :: Port -> BackLog -> MVar TidSocketInfo -> ThreadId -> [String] -> IO ()
hostSocket port backlog tidList maintid args = withSocketsDo $
  do
    let hints = defaultHints {
            addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
        }

    addrinfos <- getAddrInfo (Just hints)
        Nothing (Just port)

    let serveraddr = head addrinfos
    
    sock <- socket (addrFamily serveraddr) (addrSocketType serveraddr) (addrProtocol serveraddr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)
    listen sock backlog

    putStrLn ("Server running at -> " ++ show (addrAddress serveraddr))

    installHandler sigUSR1 (Catch $ handlerSIGUSR1 tidList) Nothing
    installHandler sigUSR2 (CatchOnce $ handlerSIGUSR2 tidList sock) Nothing
    -- | TODO :: Disable sigInt later
    {- installHandler sigINT Ignore Nothing -}

    listenForClient sock tidList args


listenForClient :: Socket -> MVar TidSocketInfo -> [String] -> IO()
listenForClient sock tidList args = withSocketsDo $ do
  (conn , peer) <- accept sock
  tid <- forkIO $ processClient (conn, peer) tidList args
  listenForClient sock tidList args


handlerSIGUSR1 :: MVar TidSocketInfo -> IO ()
handlerSIGUSR1 tidList = do
  tids <- takeMVar tidList
  putStrLn "SIGUSR1 hit -- print statistics"
  -- | TODO
  putMVar tidList tids

handlerSIGUSR2 :: MVar TidSocketInfo -> Socket -> IO ()
handlerSIGUSR2 tidList mainSocket = do
  tids <- takeMVar tidList
  putStrLn "Encountered SIGUSR2 signal ..."
  putStrLn "Exiting gracefully"
  -- | close all the sockets and then exit
  closeSocketLoop (Map.elems tids)
  close mainSocket
  putMVar tidList Map.empty
  exitImmediately ExitSuccess


closeSocketLoop [] = return ()
closeSocketLoop (tid:tids) = do
  closeSocketLoop' (snd tid)
  closeSocketLoop' (fst tid)
  closeSocketLoop tids

closeSocketLoop' (Just x) = withSocketsDo $ do close x
closeSocketLoop' Nothing = return ()
