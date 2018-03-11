module HttpParser where

import qualified Data.ByteString as S
import qualified Data.Text as T

import Data.List
import qualified Data.Map as Map


type HttpData = String
allowedMethods :: [HttpData]
allowedMethods = ["GET", "POST", "HEAD", "CONNECT"]

{- TODO :: ChunkParser and allowedMethods and Host Check-}

-- |  data types ----------------------------------------------------
--
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
  }
  deriving Show

data ChunkerState = WaitingForSize
                  | WaitingForData
                  | Complete
                  deriving (Show, Eq)

data ChunkerInfo =
  ChunkerInfo{
    cState :: ChunkerState
  , cBody :: HttpData
  , cChunk :: HttpData
  , cSize :: Int
  }
  deriving Show

-- |  data types ----------------------------------------------------



-- | helper functions -----------------------------------------------
--
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


-- | Parse First Line -----------------------------------------------
--
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
   }

-- | Parse First Line -----------------------------------------------


-- | Parse Headers --------------------------------------------------
--
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
   } 

parseHeaders'' hI hP = let x = Map.lookup "content-length" (hHeaders hP)
                       in case x of
                            Just y -> (buildParserInfo (hType hI, ReceivedHeaders), parseHeaders''' (read (snd y) :: Int) hP)
                            Nothing -> (buildParserInfo (hType hI, ReceivedBody), hP)

parseHeaders''' x hP = HttpPacket{
     hMethod = hMethod hP
   , hUrl = hUrl hP
   , hVersion = hVersion hP
   , hStatusCode = hStatusCode hP
   , hStatus = hStatus hP
   , hHeaders = hHeaders hP
   , hBody = hBody hP
   , hBodySize = x
   } 

-- | Parse Headers --------------------------------------------------

-- | Parse Body    --------------------------------------------------
--
parseBody xs hI hP | hBodySize hP == 0 = (buildParserInfo (hType hI, ReceivedBody), hP)
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
   }

emptyChunker = ChunkerInfo{
    cState = WaitingForSize
  , cBody = ""
  , cChunk = ""
  , cSize = 0
}


buildHeader [] _ [] = ""
buildHeader [] _ addList = buildHeader addList [] []
buildHeader (x:xs) delList addList | elemInDel x = buildHeader xs delList addList
  | otherwise = fst x ++ ": " ++ snd x ++ "\r\n" ++ buildHeader xs delList addList
  where elemInDel y = (T.unpack . T.toLower . T.pack . fst $ x) `elem` delList

-- make sense only for the request packet
buildPacket hP delList addList = let fl = tail . foldr (\x a -> " " ++ x hP ++ a) "" $ [hMethod, hUrl, hVersion]
                                     hds = buildHeader (Map.elems (hHeaders hP)) delList addList
                                  in fl ++ "\r\n" ++ hds ++ "\r\n" ++ hBody hP


scanner xs hI hP | hState hI == ReceivedBody = (xs, hI, hP)
  | y == Nothing = (ys, hI, hP)
  | otherwise = scanner ys hI' hP'
  where (y, ys, hI', hP') = rootParser xs hI hP


{- processChunk xs cI | cState cI == WaitingForSize = processChunkSize xs cI -}
{-   | otherwise = processChunkData xs cI -}

{- processChunkSize xs cI = let (y, ys) = splitter xs -}
{-                              f n = C. -}
{-                           in case y of -}
{-                                Nothing -> (xs, cI) -}
{-                                Just yy -> (ys, f yy) -}
