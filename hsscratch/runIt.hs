import System.IO

runIt :: String -> IO ()
runIt logFile =
  withFile logFile WriteMode $ \out -> do
    hPutStrLn out $ "Starting....."
    -- Calculate something big!
    hPutStrLn out $ "Done........."
