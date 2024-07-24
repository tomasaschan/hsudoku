{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Puzzle where

import qualified Data.Map as M
import Data.Maybe

type Coord = (Int, Int)

data Cell = Solved Int | Candidates [Int] deriving (Show, Eq)

type Puzzle = M.Map Coord Cell

allNumbers :: [Int]
allNumbers = [1 .. 9]

allColumns :: [Int]
allColumns = [0 .. 8]

allRows :: [Int]
allRows = [0 .. 8]

puzzle :: String -> Puzzle
puzzle p = M.union allSolved allCandidates
 where
  allSolved = M.fromList . fmap oneSolved . filter ((/= '0') . snd) . zip [0 .. 81] $ p
  allUnsolved = [(r, c) | r <- allRows, c <- allColumns, (r, c) `M.notMember` allSolved]
  allCandidates = M.fromList . fmap oneCandidates $ allUnsolved
  oneSolved (i, c) = ((row i, col i), Solved (read [c]))
  oneCandidates (r, c) = ((r, c),) $ Candidates $ candidates allSolved (components r c)
  row i = i `div` 9
  col i = i `mod` 9

type Component = [(Int, Int)]

-- | Returns the components (row, column and subgrid) that contain the cell at (r, c)
components :: Int -> Int -> [Component]
components r c = filter (/= (r, c)) <$> items
 where
  items = [[(r, c') | c' <- allColumns], [(r', c) | r' <- allRows], [(r', c') | r' <- sub r, c' <- sub c]]
  sub x = [x' .. x' + 2] where x' = 3 * (x `div` 3)

isSolved :: Puzzle -> Bool
isSolved = null . unsolved

unsolved :: Puzzle -> [Coord]
unsolved = fmap fst . M.toList . M.filter (\case (Solved _) -> False; _ -> True)

candidates :: Puzzle -> [Component] -> [Int]
candidates p = without allNumbers . seen
 where
  without xs ys = filter (`notElem` ys) xs
  seen :: [Component] -> [Int]
  seen = concatMap (mapMaybe ((\case Just (Solved v) -> Just v; _ -> Nothing) . flip M.lookup p))
