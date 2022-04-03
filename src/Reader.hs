module Reader where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad (unless)
import Data.Char (digitToInt, isDigit)
import Data.Text (Text)
import Sudoku (Cell (Cell, Empty), Grid (Grid))

newtype Reader a = Reader
  { runReader :: String -> Maybe (String, a)
  }

instance Functor Reader where
  fmap f (Reader r) = Reader f'
    where
      -- Define reader as a functor
      f' input = do
        (rest, x) <- r input
        return (rest, f x)

instance Applicative Reader where
  pure x = Reader $ \input -> Just (input, x)
  Reader r1 <*> Reader r2 = Reader f
    where
      -- Allow to chain readers
      f input = do
        (input', f) <- r1 input
        (input'', x) <- r2 input'
        return (input'', f x)

instance Alternative Reader where
  empty = Reader $ \_ -> Nothing
  Reader r1 <|> Reader r2 = Reader $ \input -> r1 input <|> r2 input

cellR :: Reader Cell
cellR = Reader f
  where
    f (x : xs)
      | x == '0' = Just (xs, Empty)
      | isDigit x = Just (xs, Cell $ digitToInt x)
      | otherwise = Nothing
    f [] = Nothing

cellsR :: Reader [Cell]
cellsR = many cellR

isFullyConsumed :: String -> Bool
isFullyConsumed "" = True
isFullyConsumed l = False

readGrid :: [String] -> Maybe Grid
readGrid [] = Nothing
readGrid [x] = do
  (rest, cells) <- runReader cellsR x
  unless (isFullyConsumed rest) Nothing
  return (Grid [cells])
readGrid (x : xs) = do
  Grid cells <- readGrid [x]
  Grid cells' <- readGrid xs
  return (Grid (cells ++ cells'))
