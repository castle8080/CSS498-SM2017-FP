import qualified Suduko.Board as Board

main :: IO ()
main = do
  board <- Board.readBoardFromFile "puzzles/puzzle1.txt"
  putStrLn $ "Running suduko solver: " ++ (show $ Board.getValues board)
