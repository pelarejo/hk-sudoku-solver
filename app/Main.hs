module Main where

import Data.Char
import Data.Text (Text)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

data Cell
  = Cell Int
  | Empty
  deriving (Show)

newtype Sudoku = Sudoku [[Cell]]

main :: IO ()
main = do
  lines <- readLines "grids/example1.txt"
  let su = parseSudoku lines
  case su of
    Nothing -> putStrLn "Invalid board"
    Just su -> putStrLn "Success"

parseSudoku :: [String] -> Maybe Sudoku
parseSudoku [] = Just $ Sudoku []
parseSudoku (x : xs) = case parseRow x of
  Nothing -> Nothing
  Just ces -> case parseSudoku xs of
    Nothing -> Nothing
    Just (Sudoku ces') -> Just $ Sudoku (ces : ces')
  where
    parseRow :: String -> Maybe [Cell]
    parseRow [] = Just []
    parseRow (x : xs) = case parseCell x of
      Nothing -> Nothing
      Just ce -> case parseRow xs of
        Nothing -> Nothing
        Just ces -> Just $ ce : ces

    parseCell :: Char -> Maybe Cell
    parseCell '0' = Just Empty
    parseCell c
      | isDigit c = Just (Cell $ digitToInt c)
      | otherwise = Nothing

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
