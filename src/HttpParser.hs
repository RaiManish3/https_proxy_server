module HttpParser where

import qualified Data.ByteString as S
import qualified Data.Text as T

import Data.List
import qualified Data.Map as Map
import Hex (unhex)


type HttpData = String

-- |  data types ----------------------------------------------------
data HttpParserTypes = HttpRequestParser
                     | HttpResponseParser
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
  , hBodySize :: Int
  , hChunked :: Bool
  , hChunker :: ChunkerInfo
  }
  deriving Show

data ChunkerState = WaitingForSize
                  | WaitingForData
                  | Complete
                  deriving (Show, Eq)

data ChunkerInfo =
  ChunkerInfo{
    cState :: ChunkerState
  , cChunk :: HttpData
  , cRemain :: HttpData
  , cSize :: Int
  }
  deriving Show
-- |  data types ----------------------------------------------------



-- | helper functions -----------------------------------------------
buildParserInfo :: (HttpParserTypes, HttpParserState) -> HttpParserInfo
buildParserInfo (x, y) = 
  HttpParserInfo {
    hType = x
  , hState = y
  }


splitter :: String -> (Maybe String, String)
splitter xs = splitter' "" xs
  where splitter' a (x:y:ys) | [x,y] == "\r\n" = (Just a, ys)
                             | otherwise          = splitter' (a++[x]) (y:ys)
        splitter' _ _ = (Nothing, xs)


textStripper :: T.Text -> String
textStripper = T.unpack . T.strip
-- | helper functions -----------------------------------------------


emptyHttpParserInfo hT = buildParserInfo (hT, Initialised)

emptyHttpPacket = HttpPacket {
     hMethod = ""
   , hUrl = ""
   , hVersion = ""
   , hStatusCode = ""
   , hStatus = ""
   , hHeaders = Map.empty
   , hBody = ""
   , hBodySize = 0
   , hChunked = False
   , hChunker = emptyChunker
   }

emptyChunker = ChunkerInfo{
    cState = WaitingForSize
  , cChunk = ""
  , cRemain = ""
  , cSize = 0
}


-- | Parse First Line -----------------------------------------------
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
   , hBodySize = hBodySize hP
   , hChunked = hChunked hP
   , hChunker = hChunker hP
   }

parseFirstLine'' :: [T.Text] -> HttpPacket -> HttpPacket
parseFirstLine'' xs hP = HttpPacket {
     hMethod = ""
   , hUrl = ""
   , hVersion = textStripper (xs !! 0)
   , hStatusCode = textStripper (xs !! 1)
   , hStatus = textStripper . T.intercalate (T.pack " ") $ (drop 2 xs)
   , hHeaders = hHeaders hP
   , hBody = hBody hP
   , hBodySize = hBodySize hP
   , hChunked = hChunked hP
   , hChunker = hChunker hP
   }
-- | Parse First Line -----------------------------------------------


-- | Parse Headers --------------------------------------------------
parseHeaders xs hI hP | length xs == 0 = parseHeaders'' hI hP
  | otherwise = (buildParserInfo (hType hI, ReceivingHeaders), parseHeaders' ys hP)
                      where ys' = T.pack xs
                            ys = T.splitOn (T.pack ":") ys'


parseHeaders' (x:xs) hP = HttpPacket {
     hMethod = hMethod hP
   , hUrl = hUrl hP
   , hVersion = hVersion hP
   , hStatusCode = hStatusCode hP
   , hStatus = hStatus hP
   , hHeaders = Map.insert (textStripper . T.toLower $ x) (textStripper x, textStripper . T.intercalate (T.pack ":") $ xs) (hHeaders hP)
   , hBody = hBody hP
   , hBodySize = hBodySize hP
   , hChunked = hChunked hP
   , hChunker = hChunker hP
   } 

parseHeaders'' hI hP =
  let x = Map.lookup "content-length" (hHeaders hP)
   in case x of
        Just y -> (buildParserInfo (hType hI, ReceivedHeaders), parseHeaders''' (read (snd y) :: Int) hP)
        Nothing -> let y = Map.lookup "transfer-encoding" (hHeaders hP)
                    in case y of
                         Nothing -> (buildParserInfo (hType hI, ReceivedBody), hP)
                         Just z -> if (T.toLower . T.pack $ (snd z)) == T.pack "chunked"
                                      then
                                      (buildParserInfo (hType hI, ReceivedHeaders), parseHeaders'''' hP)
                                      else
                                      (buildParserInfo (hType hI, ReceivedBody), hP)

parseHeaders''' x hP = HttpPacket{
     hMethod = hMethod hP
   , hUrl = hUrl hP
   , hVersion = hVersion hP
   , hStatusCode = hStatusCode hP
   , hStatus = hStatus hP
   , hHeaders = hHeaders hP
   , hBody = hBody hP
   , hBodySize = x
   , hChunked = hChunked hP
   , hChunker = hChunker hP
   } 

parseHeaders'''' hP = HttpPacket{
     hMethod = hMethod hP
   , hUrl = hUrl hP
   , hVersion = hVersion hP
   , hStatusCode = hStatusCode hP
   , hStatus = hStatus hP
   , hHeaders = hHeaders hP
   , hBody = hBody hP
   , hBodySize = hBodySize hP
   , hChunked = True
   , hChunker = hChunker hP
  }
-- | Parse Headers --------------------------------------------------


-- | Parse Body    --------------------------------------------------
parseBody xs hI hP 
  | hChunked hP = let hP' = processChunk xs hP
                   in if cState (hChunker hP') == Complete
                         then (buildParserInfo (hType hI, ReceivedBody), hP')
                         else (buildParserInfo (hType hI, ReceivingBody), hP')
  | hBodySize hP == 0 = (buildParserInfo (hType hI, ReceivedBody), hP)
  | otherwise = (buildParserInfo (hType hI, ReceivingBody), parseBody' xs hP)

parseBody' xs hP = HttpPacket {
     hMethod = hMethod hP
   , hUrl = hUrl hP
   , hVersion = hVersion hP
   , hStatusCode = hStatusCode hP
   , hStatus = hStatus hP
   , hHeaders = hHeaders hP
   , hBody = hBody hP ++ xs
   , hBodySize = hBodySize hP - length xs
   , hChunked = hChunked hP
   , hChunker = hChunker hP
   }
-- | Parse Body    --------------------------------------------------


rootParser xs hI hP = let (y, ys) = splitter xs
                       in case (y, hState hI) of
                            (_ , ReceivedHeaders) -> let (hI', hP') = rootParser' xs hI hP
                                                      in if xs == "" then (Nothing, "", hI, hP)
                                                                     else (Just "", "", hI', hP')
                            (_ , ReceivingBody) -> let (hI', hP') = rootParser' xs hI hP
                                                    in (Just "", "", hI', hP')
                            (Just yy, _ ) -> let (hI', hP') = rootParser' yy hI hP
                                              in (Just "", ys, hI', hP')
                            (Nothing, _) -> (Nothing, xs, hI, hP)


rootParser' xs hI hP = case hState hI of
                         Initialised -> let (hI', hP') = parseFirstLine xs hI hP
                                         in (hI', hP') 
                         ReceivedFirstLine -> let (hI', hP') = parseHeaders xs hI hP
                                               in (hI', hP') 
                         ReceivingHeaders -> let (hI', hP') = parseHeaders xs hI hP
                                              in (hI', hP') 
                         _ -> let (hI', hP') = parseBody xs hI hP
                               in (hI', hP')



{- buildUrl url | "http" /= (T.unpack . T.toLower . T.pack $ (drop 4 url)) = url -}
{-   | otherwise = f -}
{-   where f = let x = dropWhile (/= '/') $ drop 7 url -}
{-              in if x == "" then "/" -}
{-                            else x -}

buildUrl url = url

buildHeader [] _ [] = ""
buildHeader [] _ addList = buildHeader addList [] []
buildHeader (x:xs) delList addList | elemInDel x = buildHeader xs delList addList
  | otherwise = fst x ++ ": " ++ snd x ++ "\r\n" ++ buildHeader xs delList addList
  where elemInDel y = (T.unpack . T.toLower . T.pack . fst $ x) `elem` delList

-- make sense only for the request packet
buildPacket hP delList addList = let fl = hMethod hP ++ " " ++ buildUrl (hUrl hP) ++ " " ++ hVersion hP
                                     hds = buildHeader (Map.elems (hHeaders hP)) delList addList
                                  in fl ++ "\r\n" ++ hds ++ "\r\n" ++ hBody hP


scanner xs hI hP | hState hI == ReceivedBody = (xs, hI, hP)
  | y == Nothing = (ys, hI, hP)
  | otherwise = scanner ys hI' hP'
  where (y, ys, hI', hP') = rootParser xs hI hP


-- |  Chunk Parsing  ----------------------------------------------------
processChunk xs hP | cState cI == WaitingForSize = processChunkSize xs hP
  | otherwise = processChunkData xs hP
  where cI = hChunker hP

processChunkSize xs hP = 
  let cI = hChunker hP
      (y, ys) = splitter (cRemain cI ++ xs)
   in case y of
        Nothing -> modifyChunker hP WaitingForSize "" (cRemain cI ++ xs) 0 ""
        Just yy -> modifyChunker hP WaitingForData "" ys (unhex yy) ""


processChunkData xs hP =
  let cI = hChunker hP
      prevchunk = cChunk cI
      tChunkSize = cSize cI - length prevchunk
      cData = take tChunkSize xs
      leftOver = drop tChunkSize xs
   in if length cData == tChunkSize
         then
         if cSize cI /= 0
            then modifyChunker hP WaitingForSize "" (drop 2 leftOver) 0 (prevchunk ++ cData)
            else modifyChunker hP Complete "" "" 0  (prevchunk ++ cData)
         else
            modifyChunker hP WaitingForData (prevchunk ++ cData) leftOver (cSize cI) ""


modifyChunker hP st ch rem size bd = HttpPacket {
   hMethod = hMethod hP
 , hUrl = hUrl hP
 , hVersion = hVersion hP
 , hStatusCode = hStatusCode hP
 , hStatus = hStatus hP
 , hHeaders = hHeaders hP
 , hBody = hBody hP ++ bd
 , hBodySize = hBodySize hP
 , hChunked = hChunked hP
 , hChunker = modifyChunker' (hChunker hP) st ch rem size
 }

modifyChunker' cI st ch rem size = ChunkerInfo{
    cState = st
  , cChunk = ch
  , cRemain = rem
  , cSize = size
  }
-- |  Chunk Parsing  ----------------------------------------------------
