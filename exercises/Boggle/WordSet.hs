module WordSet (load, beginWith) where

import System.IO
import Data.Maybe
import qualified Data.Set as Set

type WordSet = Set.Set String

load :: FilePath -> IO WordSet
load fileName = do
  fContent <- readFile fileName
  return (Set.fromList $ lines fContent)

beginWith :: String -> WordSet -> [String]
beginWith subs words =
  let
    subse = (init subs) ++ ([succ $ subs !! (length subs - 1)])
    si = Set.lookupGE subs words >>= (\w -> return $ Set.findIndex w words)
    ei = Set.lookupLT subse words >>= (\w -> return $ Set.findIndex w words)
    in
      case (si, ei) of
        (Just si, Just ei) -> [Set.elemAt i words | i <- [si..ei]]
        (Nothing, Just ei) -> [Set.elemAt i words | i <- [0..ei]]
        (Just si, Nothing) -> [Set.elemAt i words | i <- [si..]]
        _                  -> []
