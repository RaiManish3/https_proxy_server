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
import Stats
import ThreadProxy

main :: IO ()
main = do
  args <- getArgs
  tidList <- newMVar Map.empty
  pid <- getProcessID
  putStrLn ("Server's Process ID: " ++ show pid)

  let port = "8888"
      backlog = 5
  hostSocket port backlog tidList args


type Port = String

hostSocket :: Port -> Int -> MVar TidSocketInfo -> [String] -> IO ()
hostSocket port backlog tidList args = withSocketsDo $
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
    rs <- newMVar initialStat

    installHandler sigUSR1 (Catch $ handlerSIGUSR1 tidList rs) Nothing
    installHandler sigUSR2 (CatchOnce $ handlerSIGUSR2 tidList sock) Nothing
    -- | TODO :: Disable sigInt later
    installHandler sigINT Ignore Nothing

    listenForClient sock tidList args rs


listenForClient :: Socket -> MVar TidSocketInfo -> [String] -> MVar RequestStat -> IO()
listenForClient sock tidList args rs = withSocketsDo $ do
  (conn , peer) <- accept sock
  tid <- forkIO $ processClient (conn, peer) tidList args rs
  listenForClient sock tidList args rs


handlerSIGUSR1 :: MVar TidSocketInfo -> MVar RequestStat -> IO ()
handlerSIGUSR1 tidList rs = do
  tids <- takeMVar tidList
  rs' <- takeMVar rs
  putMVar rs rs'
  putStrLn "\n==================================================="
  putStrLn "SIGUSR1 hit -- printing statistics"
  putStrLn ("#Successful requests: " ++ show (tSuccess rs'))
  putStrLn ("#Filtered requests: " ++ show (tFiltered rs'))
  putStrLn ("#Error requests: " ++ show (tError rs'))
  putStrLn "===================================================\n"
  putMVar tidList tids

handlerSIGUSR2 :: MVar TidSocketInfo -> Socket -> IO ()
handlerSIGUSR2 tidList mainSocket = do
  tids <- takeMVar tidList
  putStrLn "\n==================================================="
  putStrLn "Encountered SIGUSR2 signal ..."
  putStrLn "Exiting gracefully"
  closeSocketLoop (Map.elems tids)
  putMVar tidList Map.empty
  putStrLn "===================================================\n"
  close mainSocket
  exitImmediately ExitSuccess


closeSocketLoop [] = return ()
closeSocketLoop (tid:tids) = do
  closeSocketLoop' (snd tid)
  closeSocketLoop' (fst tid)
  closeSocketLoop tids

closeSocketLoop' (Just x) = withSocketsDo $ do close x
closeSocketLoop' Nothing = return ()
