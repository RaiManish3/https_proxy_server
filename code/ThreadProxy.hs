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
import qualified Data.Map as Map


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


type HttpData = String

data HttpParserTypes = HttpRequestParser
                     | HttpResponseParser
                     deriving (Show, Eq)

data HttpParserChunked = Chunked
                       | Nothing
                       deriving (Show, Eq)

data HttpParserState = Initialised
                     | ReceivedFirstLine
                     | ReceivingHeaders
                     | ReceivedHeaders
                     | ReceivingBody
                     | ReceivedBody
                     deriving (Show, Eq)

data HttpParserInfo = 
  HttpParserInfo {
    hType :: HttpParserTypes
  , hState :: HttpParserState
  }
  deriving Show

data HttpPacket = 
  HttpPacket  {
    hMethod :: HttpData
  , hUrl :: HttpData
  , hVersion :: HttpData
  , hStatusCode :: HttpData
  , hStatus :: HttpData
  , hHeaders :: Map.Map HttpData (HttpData, HttpData)
  , hBody :: HttpData
  }
  deriving Show

allowedMethods :: [HttpData]
allowedMethods = ["GET", "POST", "HEAD", "CONNECT"]

buildParserInfo :: (HttpParserTypes, HttpParserState) -> HttpParserInfo
buildParserInfo (x, y) = 
  HttpParserInfo {
    hType = x
  , hState = y
  }


splitter :: String -> (String, String)
splitter xs = splitter' "" xs
  where splitter' a (x:y:ys) | (x:y:[]) == "\r\n" = (a, ys)
                             | otherwise          = splitter' (a++[x]) (y:ys)
        splitter' _ _ = ("", xs)




parseFirstLine xs hI hP | length xs == 0 = (hI, hP)
  | otherwise = let xs' = T.pack xs
                    x = T.splitOn (T.pack " ") xs'
                    httpType = hType hI
                 in case httpType of
                      HttpRequestParser -> (buildParserInfo (httpType, ReceivedFirstLine), parseFirstLine' x hP)
                      _ -> (buildParserInfo (httpType, ReceivedFirstLine), parseFirstLine'' x hP)


parseFirstLine' :: [T.Text] -> HttpPacket -> HttpPacket
parseFirstLine' xs hP = HttpPacket {
     hMethod = textStripper (xs !! 0)
   , hUrl = textStripper (xs !! 1)
   , hVersion = textStripper (xs !! 2)
   , hStatusCode = ""
   , hStatus = ""
   , hHeaders = hHeaders hP
   , hBody = hBody hP
   }

parseFirstLine'' :: [T.Text] -> HttpPacket -> HttpPacket
parseFirstLine'' xs hP = HttpPacket {
     hMethod = ""
   , hUrl = ""
   , hVersion = textStripper (xs !! 0)
   , hStatusCode = textStripper (xs !! 1)
   , hStatus = textStripper (T.concat (drop 2 xs))
   , hHeaders = hHeaders hP
   , hBody = hBody hP
   }



parseHeaders xs hI hP | length xs == 0 = (buildParserInfo (hType hI, f (hState hI)), hP)
  | otherwise = (buildParserInfo (hType hI, ReceivingHeaders), parseHeaders' ys hP)
                      where ys' = T.pack xs
                            ys = T.splitOn (T.pack ":") ys'
                            f ReceivedFirstLine = ReceivingHeaders
                            f ReceivingHeaders = ReceivedHeaders
                            f _ = error "parse Headers at the wrong place"


parseHeaders' (x:xs) hP = HttpPacket {
     hMethod = hMethod hP
   , hUrl = hUrl hP
   , hVersion = hUrl hP
   , hStatusCode = hStatusCode hP
   , hStatus = hStatus hP
   , hHeaders = Map.insert (textStripper . T.toLower $ x) (textStripper x, textStripper . T.concat $ xs) (hHeaders hP)
   , hBody = hBody hP
   } 

parseHeaders' _ hP = error "Error in parsing headers"


parseBody "" hI hP | hState hI == ReceivedHeaders = (buildParserInfo (hType hI, ReceivingBody), hP)
  | hState hI == ReceivingBody = (buildParserInfo (hType hI, ReceivedBody), hP)
parseBody xs hI hP = ( buildParserInfo (hType hI, ReceivingBody), parseBody' xs hP)

parseBody' xs hP = HttpPacket {
     hMethod = hMethod hP
   , hUrl = hUrl hP
   , hVersion = hUrl hP
   , hStatusCode = hStatusCode hP
   , hStatus = hStatus hP
   , hHeaders = hHeaders hP
   , hBody = hBody hP ++ xs
   }


textStripper :: T.Text -> String
textStripper = T.unpack . T.strip


mainParser xs hI hP = let (y, ys) = splitter xs
                       in case hState hI of
                            Initialised -> let (hI', hP') = parseFirstLine y hI hP
                                            in (ys, hI', hP') 
                            ReceivedFirstLine -> let (hI', hP') = parseHeaders y hI hP
                                                  in (ys, hI', hP') 
                            ReceivingHeaders -> let (hI', hP') = parseHeaders y hI hP
                                                 in (ys, hI', hP') 
                            _ -> let (hI', hP') = parseBody xs hI hP
                                  in ("", hI', hP')


initialHttpParserInfo :: HttpParserTypes -> HttpParserInfo
initialHttpParserInfo hT = buildParserInfo (hT, Initialised)

initialHttpPacket = HttpPacket {
     hMethod = ""
   , hUrl = ""
   , hVersion = ""
   , hStatusCode = ""
   , hStatus = ""
   , hHeaders = Map.empty
   , hBody = ""
   }



xcvb "" hI hP = (hI, hP)
xcvb xs hI hP = let (ys, hI', hP') = mainParser xs hI hP
                 in xcvb ys hI' hP'

xxx = xcvb "hey guys there\r\nContent: manish\r\nWow: 3\r\n\r\n" (initialHttpParserInfo HttpRequestParser) initialHttpPacket
