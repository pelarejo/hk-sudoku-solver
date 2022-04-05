module Main where

import Data.Text (Text)
import Reader ( readGrid )
import Sudoku ( gridToText, gridToTextChoices, runReduceGridChoices, solve)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main = do
  lines <- readLines "grids/hard.txt"
  let grid = readGrid lines
  case grid of
    Nothing -> putStrLn "Invalid board"
    Just g -> case solve g of
      Nothing -> putStrLn "Impossible board"
      Just g' -> putStrLn $ gridToTextChoices g'
