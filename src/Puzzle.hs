module Puzzle where

import qualified Data.Map as M
import Data.Maybe

type Item = (Int, Int, Int)

value :: Item -> Int
value (_, _, v) = v

type Puzzle = M.Map (Int, Int) Int

puzzle :: String -> Puzzle
puzzle = M.fromList . fmap oneCell . filter nonzero . zip [0 ..]
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
    [ [item r c' | c' <- [0 .. 8]],
      [item r' c | r' <- [0 .. 8]],
      [item r' c' | r' <- sub r, c' <- sub c]
    ]
  where
    sub x = [x' .. x' + 2] where x' = 3 * (x `div` 3)
    item r' c' = case at r' c' p of
      Just v -> Just (r', c', v)
      Nothing -> Nothing

isSolved :: Puzzle -> Bool
isSolved = null . unsolved

unsolved :: Puzzle -> [(Int, Int)]
unsolved p = [(r, c) | r <- [0 .. 8], c <- [0 .. 8], isNothing . at r c $ p]

candidates :: Int -> Int -> Puzzle -> [Int]
candidates r c p = case at r c p of
  Just v -> [v]
  Nothing -> [1 .. 9] `without` (concatMap (fmap value) . components r c $ p)
  where
    without xs ys = filter (`notElem` ys) xs

type Edit = (Int, Int, Int)

edit :: Puzzle -> Edit -> Puzzle
edit p (r, c, v) = M.insert (r, c) v p
