readLines :: String -> IO [String]
readLines filePath = do
  content <- readFile filePath
  return $ lines content
