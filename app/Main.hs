module Main where

import Data.Text (Text)
import Reader ( readGrid )
import Sudoku ( gridToText, gridToTextChoices, reduceGridChoices)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main = do
  lines <- readLines "grids/example1.txt"
  let grid = readGrid lines
  case grid of
    Nothing -> putStrLn "Invalid board"
    Just g -> putStrLn $ gridToTextChoices $ reduceGridChoices g
