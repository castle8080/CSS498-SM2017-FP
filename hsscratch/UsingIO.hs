import Data.List
import Text.Read
import Text.Printf

import System.IO
import Data.Time.Clock


import Network.HTTP.Client

getReversedLine :: IO String
getReversedLine =
  getLine >>= (\line -> return $ reverse line)

getInt :: IO Int
getInt =
  getLine >>= (\line -> return $ read line)



promptForNumber :: Int -> Int -> IO Int
promptForNumber start end = do
  printf "Choose between %d and %d: " start end
  line <- getLine
  case readMaybe line of
    Just x | x >= start && x <= end ->
      return x
    _ -> do
      putStrLn "That doesn't look right!"
      promptForNumber start end

promptForNumber2 :: Int -> Int -> IO Int
promptForNumber2 start end =
  printf "Choose between %d and %d: " start end >>
  getLine >>= \line ->
  case readMaybe line of
    Just x | x >= start && x <= end ->
      return x
    _ ->
      putStrLn "That doesn't look right!" >>
      promptForNumber2 start end

readLines :: String -> IO [String]
readLines filePath = do
  content <- readFile filePath
  return $ lines content

runIt :: String -> IO ()
runIt logFile =
  withFile logFile WriteMode $ \out -> do
    hPutStrLn out $ "Starting....."
    -- Calculate something big!
    hPutStrLn out $ "Done........."

getURL :: String -> IO String
getURL url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  return $ show $ responseBody response
