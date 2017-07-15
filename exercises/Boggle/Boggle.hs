module Boggle () where

import System.Random

newBoard :: RandomGen g => g -> Int -> ([[Int]], g)
newBoard rg size =
  
