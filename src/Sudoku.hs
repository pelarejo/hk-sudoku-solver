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

reduceChoices :: Row -> Maybe Row
reduceChoices row = mapM reduce row
  where
    isCell (Cell _) = True
    isCell (Empty _) = False
    validCells = filter isCell row

    reduce (Cell c) = Just (Cell c)
    reduce (Empty choices) = case filter (\p -> Cell p `notElem` validCells) choices of
      [] -> Nothing
      [x] -> Just (Cell x)
      xs -> Just (Empty xs)



reduceGridChoices :: Grid -> Maybe Grid
reduceGridChoices grid = Just grid >>= reduceDimension getRows >>= reduceDimension getColumns >>= reduceDimension getSubGrid
  where
    reduceDimension f = fmap f . mapM reduceChoices . f

runReduceGridChoices :: Grid -> Maybe Grid
runReduceGridChoices grid = case grid' of
  Just g -> if grid == g then Just grid else runReduceGridChoices g
  Nothing -> Nothing
  where grid' = reduceGridChoices grid
