import Data.IORef
import Control.Monad

process1 :: IORef Int -> Int -> IO()
process1 countRef x =
  forM_ [1..x] $ \n -> do
    putStrLn "Do process 1"
    modifyIORef countRef (+1)

main :: IO ()
main = do
  countRef <- newIORef 0
  count <- readIORef countRef
  putStrLn $ "Count at start: " ++ (show count)
  process1 countRef 3
  count <- readIORef countRef
  putStrLn $ "Count at end: " ++ (show count)
