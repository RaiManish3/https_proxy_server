module Stats where

data RequestStat = 
  RequestStat{
    tSuccess :: Int
  , tFiltered :: Int
  , tError :: Int
}

initialStat :: RequestStat
initialStat = RequestStat{
                  tSuccess = 0
                , tFiltered = 0
                , tError = 0
               }

updateStat :: RequestStat -> String -> IO RequestStat
updateStat rs ss = do
  case ss of
    "success" -> return RequestStat {
                   tSuccess = tSuccess rs + 1
                 , tFiltered = tFiltered rs
                 , tError = tError rs
                 }
    "filtered" -> return RequestStat {
                   tSuccess = tSuccess rs
                 , tFiltered = tFiltered rs + 1
                 , tError = tError rs
                 }
    "error"   -> return RequestStat {
                   tSuccess = tSuccess rs
                 , tFiltered = tFiltered rs + 1
                 , tError = tError rs
                 }
    _ -> return rs
