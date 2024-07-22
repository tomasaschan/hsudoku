module Puzzle where

import qualified Data.Map as M
import Data.Maybe

type Item = (Int, Int, Int)

allNumbers :: [Int]
allNumbers = [1 .. 9]

allColumns :: [Int]
allColumns = [0 .. 8]

allRows :: [Int]
allRows = [0 .. 8]

value :: Item -> Int
value (_, _, v) = v

type Puzzle = M.Map (Int, Int) Int

puzzle :: String -> Puzzle
puzzle = M.fromList . fmap oneCell . filter nonzero . zip [0 .. 81]
  where
    nonzero (_, c) = c /= '0'
    oneCell (i, c) = ((row i, col i), read [c])
    row i = i `div` 9
    col i = i `mod` 9

at :: Int -> Int -> Puzzle -> Maybe Int
at r c = M.lookup (r, c)

type Component = [Item]

-- | Returns the components (row, column and subgrid) that contain the cell at (r, c)
components :: Int -> Int -> Puzzle -> [Component]
components r c p =
  fmap
    catMaybes
    [ [item r c' | c' <- allColumns],
      [item r' c | r' <- allRows],
      [item r' c' | r' <- sub r, c' <- sub c]
    ]
  where
    sub x = [x' .. x' + 2] where x' = 3 * (x `div` 3)
    item r' c' = case at r' c' p of
      Just v -> Just (r', c', v)
      Nothing -> Nothing

isSolved :: Puzzle -> Bool
isSolved = null . unsolved

isValid :: Puzzle -> Bool
isValid p = not $ any (null . \(r, c) -> candidates r c p) $ [(r, c) | r <- allRows, c <- allColumns]

unsolved :: Puzzle -> [(Int, Int)]
unsolved p = [(r, c) | r <- allRows, c <- allColumns, isNothing . at r c $ p]

candidates :: Int -> Int -> Puzzle -> [Int]
candidates r c p = case at r c p of
  Just v -> [v]
  Nothing -> allNumbers `without` (concatMap (fmap value) . components r c $ p)
  where
    without xs ys = filter (`notElem` ys) xs

type Edit = (Int, Int, Int)

edit :: Puzzle -> Edit -> Puzzle
edit p (r, c, v) = M.insert (r, c) v p
