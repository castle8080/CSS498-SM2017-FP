module Suduko.Board (
  Board(Board),
  cells,
  empty,
  readBoard,
  readBoardFromFile,
  getValues
) where

import qualified Suduko.BoardCell as Cell
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Char as Char

data Board = Board {
  cells :: V.Vector (V.Vector Cell.BoardCell)
} deriving (Show, Eq)

empty :: Board
empty =
  Board {
    cells = V.replicate 9 (V.replicate 9 $ Cell.create 0)
  }

getValues :: Board -> [[Int]]
getValues board =
  map (\row -> map Cell.value $ V.toList row) $ V.toList (cells board)

setValue :: Int -> Int -> Int -> Board -> Board
setValue val rowNo colNo board =
  let
    _cells   = (cells board)
    row      = _cells V.! rowNo
    cell     = row V.! colNo
    newCell  = Cell.set val cell
    newRow   = row V.// [(colNo, newCell)]
    newCells = _cells V.// [(rowNo, newRow)]
    newBoard = board { cells = newCells }
  in
    newBoard

readBoard :: String -> Board
readBoard content =
  let
    cLines = lines content
    rowLines = L.zip cLines [0 .. (length cLines - 1)]
    getRowValues rowText =
      map (\c -> if c == ' ' then 0 else Char.digitToInt c) rowText
    updateRow board (rowText, rowNo) =
      let
        rawValues = getRowValues rowText
        colValues = L.zip rawValues [0 .. (length rawValues - 1)]
        updateCell board (v, colNo) =
          setValue v rowNo colNo board
      in
        L.foldl updateCell board colValues
  in
    L.foldl updateRow empty rowLines

readBoardFromFile :: String -> IO Board
readBoardFromFile boardFile = do
  content <- readFile boardFile
  return $ readBoard content
