module Suduko.BoardCell (
  BoardCell(BoardCell),
  value,
  possibilities,
  create,
  hasValue,
  set
) where

import qualified Data.Vector as V
import qualified Data.HashSet as HS

data BoardCell = BoardCell {
  value :: Int,
  possibilities :: HS.HashSet Int
} deriving (Show,Eq)

hasValue :: BoardCell -> Bool
hasValue cell =
  (value cell) /= 0

create :: Int -> BoardCell
create v =
  let isSet = v >= 1 && v <= 9 in
  BoardCell {
    value =
      if isSet then v else 0,
    possibilities =
      if isSet then HS.empty else HS.fromList [1..9]
  }

set :: Int -> BoardCell -> BoardCell
set v cell =
  cell { value = v, possibilities = HS.empty }

removePossibility :: Int -> BoardCell -> BoardCell
removePossibility v cell =
  let newP = HS.delete v $ possibilities cell in
  if (HS.size newP) == 1 then
    BoardCell {
      value =
        head $ HS.toList newP,
      possibilities =
        HS.empty
    }
  else
    cell { possibilities = newP }
