{-
  Name: Bryan Castillo
  Exercises: Chapter 8 (Declaring types and classes) "Programming in Haskell"
-}
import Data.List (all)
import Data.Char

all_true xs = all id xs

main :: IO ()
main = do
  putStrLn "Running chapter 8 tests:"
