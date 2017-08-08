import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

getURL :: String -> IO String
getURL url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  withResponse request manager $ \response -> do
    bytes <- brConsume $ responseBody response
    return $ UTF8.toString $ BS.concat bytes
