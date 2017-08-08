import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.List

getNames :: IO [String]
getNames = do
  conn <- connectSqlite3 "test.db"
  st <- prepare conn "select name from people"
  execute st []
  rows <- fetchAllRows st
  return $ map (\row -> fromSql $ row !! 0) rows

main :: IO ()
main = do
  names <- getNames
  forM_ names putStrLn
