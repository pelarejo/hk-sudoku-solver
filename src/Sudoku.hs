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

-- parseGrid :: [String] -> Maybe Grid
-- parseGrid [] = Just $ Grid []
-- parseGrid (x : xs) = case parseRow x of
--   Nothing -> Nothing
--   Just ces -> case parseGrid xs of
--     Nothing -> Nothing
--     Just (Grid ces') -> Just $ Grid (ces : ces')
--   where
--     parseRow :: String -> Maybe [Cell]
--     parseRow [] = Just []
--     parseRow (x : xs) = case parseCell x of
--       Nothing -> Nothing
--       Just ce -> case parseRow xs of
--         Nothing -> Nothing
--         Just ces -> Just $ ce : ces

--     parseCell :: Char -> Maybe Cell
--     parseCell '0' = Just Empty
--     parseCell c
--       | isDigit c = Just (Cell $ digitToInt c)
--       | otherwise = Nothing

-- strGrid :: Grid -> [Char]
-- strGrid = undefined

-- strGrid (Grid cells) = concat $ intercalate ["\n"] $ map (map show) cells
--   where
--     lineToStr :: [Cell] -> String
--     lineToStr x = map show x
