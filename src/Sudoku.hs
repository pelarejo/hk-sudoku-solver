module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List (intercalate, transpose)

data Cell
  = Cell Int
  | Empty

instance Show Cell where
  show (Cell c) = show c
  show Empty = "_"

instance Eq Cell where
  (Cell c1) == (Cell c2) = c1 == c2
  Empty == Empty = True
  _ == _ = False

newtype Grid = Grid [[Cell]]
  deriving (Show, Eq)

rowToText :: [Cell] -> String
rowToText [] = ""
rowToText [x] = show x
rowToText (x : xs) = show x ++ " " ++ rowToText xs

gridToText :: Grid -> String
gridToText (Grid []) = ""
gridToText (Grid (x : xs)) = rowToText x ++ "\n" ++ gridToText (Grid xs)

getRows :: Grid -> Grid
getRows grid = grid

getColumns :: Grid -> Grid
getColumns (Grid cells) = Grid $ transpose cells

getSubGrid :: Grid -> Grid
getSubGrid (Grid cells) = Grid $ undoSubgrid $ map transpose $ makeSubGrid cells
  where
    -- A transposable sub-section
    -- This is required to make this reversable -> getSubGrid $ getSubGrid grid == grid
    makeSubGrid = myChunksOf 3 . map (myChunksOf 3)
    undoSubgrid = map concat . concat

-- Similar function as Data.List.Split (chunkOf), did not want to include the package.
myChunksOf :: Int -> [a] -> [[a]]
myChunksOf _ [] = []
myChunksOf size xs = fst split : myChunksOf size (snd split)
  where split = splitAt size xs
