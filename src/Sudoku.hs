module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List (intercalate, transpose)
import Data.Bool (Bool(False))

data Cell
  = Cell Int
  | Empty [Int]

instance Show Cell where
  show (Cell c) = show c
  show (Empty _) = "_"

instance Eq Cell where
  (Cell c1) == (Cell c2) = c1 == c2
  (Empty _) == (Empty _) = True
  _ == _ = False

type Row = [Cell]

type Grid = [Row]
  -- deriving (Show, Eq)

gridToText :: Grid -> String
gridToText [] = ""
gridToText (x : xs) = rowToText x ++ "\n" ++ gridToText xs
  where
    rowToText [] = ""
    rowToText [x] = show x
    rowToText (x : xs) = show x ++ " " ++ rowToText xs

gridToTextChoices :: Grid -> String
gridToTextChoices [] = ""
gridToTextChoices (x : xs) = rowToText x ++ "\n" ++ gridToTextChoices xs
  where
    showChoices (Cell c) = replicate 17 ' ' ++ show c
    showChoices (Empty xs) = replicate space_size ' ' ++ show xs
      where
        space_size = 17 - length xs * 2

    rowToText [] = ""
    rowToText [x] = showChoices x
    rowToText (x : xs) = showChoices x ++ " " ++ rowToText xs

getRows :: Grid -> Grid
getRows grid = grid

getColumns :: Grid -> Grid
getColumns = transpose

getSubGrid :: Grid -> Grid
getSubGrid rows = undoSubgrid $ map transpose $ makeSubGrid rows
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

repeated :: Eq a => [a] -> Bool
repeated [] = False
repeated [_] = False
repeated (x : xs) = elem x xs || repeated xs

reduceChoices :: Row -> Row
reduceChoices row = map reduceChoices row
  where
    isCell (Cell _) = True
    isCell (Empty _) = False
    validCells = filter isCell row

    reduceChoices (Empty choices) = Empty $ filter (\p -> Cell p `notElem` validCells) choices
    reduceChoices (Cell c) = Cell c

reduceGridChoices :: Grid -> Grid
reduceGridChoices = reduceRows getRows . reduceRows getColumns . reduceRows getSubGrid
  where
    reduceRows f = f . map reduceChoices . f
