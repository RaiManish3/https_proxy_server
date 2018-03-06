module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import System.IO

import Control.Concurrent
{- import qualified Control.Concurrent.Thread as Thread -}
import Control.Concurrent.MVar
import Control.Monad (forever)
import System.Posix.Process (getProcessID)

import System.Exit
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigUSR1, sigUSR2)
import System.Posix.Process

import Data.List
import ThreadProxy


main :: IO ()
main = do
  tidList <- newMVar []
  pid <- getProcessID
  maintid <- myThreadId

  putStrLn (show pid)

  let port = "8888"
      backlog = 5

  installHandler sigUSR1 (Catch $ handlerSIGUSR1 tidList) Nothing
  installHandler sigUSR2 (CatchOnce $ handlerSIGUSR2 tidList) Nothing
  {- installHandler sigINT Ignore Nothing -}
  hostSocket port backlog tidList maintid


type Port = String
type BackLog = Int

hostSocket :: Port -> BackLog -> MVar TidSocketInfo -> ThreadId -> IO ()
hostSocket port backlog tidList maintid = withSocketsDo $
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

    tids <- takeMVar tidList
    let newTids = (maintid, sock):tids
    putMVar tidList newTids

    listenForClient sock tidList

    --  Should not reach here
    close sock


listenForClient :: Socket -> MVar TidSocketInfo -> IO()
listenForClient sock tidList = forever $ do
  -- add the thread and client sock into the list
  tids <- takeMVar tidList
  (conn , peer) <- accept sock
  putStrLn ("Connection from -> " ++ show peer)
  tid <- forkIO $ processClient (conn, peer) tidList
  putStrLn ("New Thread Created with id: " ++ show tid)
  let newTids = (tid, conn)
  putMVar tidList newTids


handlerSIGUSR1 :: MVar TidSocketInfo -> IO ()
handlerSIGUSR1 tidList = do
  tids <- takeMVar tidList
  putStrLn "SIGUSR1 hit -- print statistics"
  putMVar tidList tids

handlerSIGUSR2 :: MVar TidSocketInfo -> IO ()
handlerSIGUSR2 tidList = do
  putStrLn "Encountered SIGUSR2 signal ..."
  putStrLn "Exiting gracefully"
  tids <- takeMVar tidList
  closeSocketLoop tids
  putMVar tidList []
  exitImmediately ExitSuccess


closeSocketLoop :: MVar TidSocketInfo -> IO ()
closeSocketLoop [] = return ()
closeSocketLoop (tid:tids) = do
  -- wait for the thread to complete except possibly the main thread
  close (snd tid)
  closeSocketLoop tids
