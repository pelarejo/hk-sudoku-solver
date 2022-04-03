module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)

data Cell
  = Cell Int
  | Empty

instance Show Cell where
  show (Cell c) = show c
  show Empty = "_"

newtype Grid = Grid [[Cell]]
  deriving (Show)

rowToText :: [Cell] -> String
rowToText [] = ""
rowToText [x] = show x
rowToText (x : xs) = show x ++ " " ++ rowToText xs

gridToText :: Grid -> String
gridToText (Grid []) = ""
gridToText (Grid (x : xs)) = rowToText x ++ "\n" ++ gridToText (Grid xs)
