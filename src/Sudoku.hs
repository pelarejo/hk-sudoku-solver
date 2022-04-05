module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List (intercalate, transpose, elemIndices)
import Data.Bool (Bool(False))
import Control.Applicative ((<|>))

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

isCell :: Cell -> Bool
isCell (Cell _) = True
isCell (Empty _) = False

isEmpty :: Cell -> Bool
isEmpty = not . isCell

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
    showChoices (Cell c) = replicate 18 ' ' ++ show c
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

newGrid :: Grid -> (Grid, Grid)
newGrid grid = (replace2D (fst choices) (fst emptyCell) grid, replace2D (snd choices) (fst emptyCell) grid)
  where
    emptyCells = findEmpty grid
    -- warning: no handling if not found
    emptyCell = head emptyCells
    choices = splitChoice $ snd emptyCell

    -- return the coordonates of the first empty cell + the empty cell
    findEmpty g = [ ((fst x,y), snd x) | (y,row) <- zip [0..] g, x <- filter (isEmpty . snd) (zip [0..] row)]

    -- Make a Cell out of an Empty choice to fill the first grid with
    -- Return the rest of the choices in a smaller Empty cell.
    splitChoice (Empty [x]) = (Cell x, Cell x)
    splitChoice (Empty (x : xs)) = (Cell x, Empty xs)
    splitChoice _ = undefined

    replace p f xs = [ if i == p then f x else x | (x, i) <- zip xs [0..] ]
    replace2D v (x,y) = replace y (replace x (const v))


hasRepeated :: Eq a => [a] -> Bool
hasRepeated [] = False
hasRepeated [_] = False
hasRepeated (x : xs) = elem x xs || hasRepeated xs

solved :: Grid -> Bool
solved = not . any (any isEmpty)

unsolvable :: Grid -> Bool
unsolvable grid =
  invalid getRows grid || invalid getColumns grid || invalid getSubGrid grid
  where
    invalid t g = any invalidRow (t g)
    invalidRow row = hasRepeated (filter isCell row) || noChoicesLeft row
    noChoicesLeft row = not $ null [x | Empty x <- row, null x]

solve :: Grid -> Maybe Grid
solve grid = runReduceGridChoices grid >>= runSolve
  where
    runSolve g
      | unsolvable g = Nothing
      | solved g = Just g
      | otherwise = solve gridL <|> solve gridR
        where
          (gridL, gridR) = newGrid g
