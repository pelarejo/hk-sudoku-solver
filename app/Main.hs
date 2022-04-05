module Main where

import Data.Text (Text)
import Reader ( readGrid )
import Sudoku ( gridToText, solve)
import System.Exit
import System.Environment



main :: IO ()
main = getArgs >>= parse >>= runMain

runMain :: [String] -> IO ()
runMain lines = do
  let grid = readGrid lines
  case grid of
    Nothing -> putStrLn "Invalid board"
    Just g -> case solve g of
      Nothing -> putStrLn "Impossible board"
      Just g' -> putStrLn $ gridToText g'

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parse ["-h"] = usage >> exit
parse [] = usage >> exit
parse file = concat `fmap` mapM readLines file

usage = putStrLn "Usage: stack exec -- sudoku-solver-exe [-h] [file]"
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)
