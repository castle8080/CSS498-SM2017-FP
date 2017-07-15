import System.IO
import Data.List
import qualified WordSet as WordSet

main = do
  words <- WordSet.load "enable1.txt"
  bc <- return $ WordSet.beginWith "cat" words
  putStrLn $ (intercalate [','] bc)
