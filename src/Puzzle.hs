{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Puzzle where

import qualified Data.Map   as M
import           Data.Maybe

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

type Component = [(Int, Int)]

-- | Returns the components (row, column and subgrid) that contain the cell at (r, c)
components :: Int -> Int -> [Component]
components r c = filter (/= (r, c)) <$> items
  where
    items = [[(r, c') | c' <- allColumns], [(r', c) | r' <- allRows], [(r', c') | r' <- sub r, c' <- sub c]]
    sub x = [x' .. x' + 2] where x' = 3 * (x `div` 3)

isSolved :: Puzzle -> Bool
isSolved = null . unsolved

isValid :: Puzzle -> Bool
isValid p = not . any null $ [candidates r c p | r <- allRows, c <- allColumns]

unsolved :: Puzzle -> [(Int, Int)]
unsolved p = [(r, c) | r <- allRows, c <- allColumns, isNothing . at r c $ p]

candidates :: Int -> Int -> Puzzle -> [Int]
candidates r c p = case at r c p of
  Just v -> [v]
  Nothing -> allNumbers `without` concatMap (mapMaybe (\(r', c') -> at r' c' p)) (components r c)
  where
    without xs ys = filter (`notElem` ys) xs

type Edit = (Int, Int, Int)

edit :: Puzzle -> Edit -> Puzzle
edit p (r, c, v) = M.insert (r, c) v p

{-# HLINT ignore countCandidates "Use catMaybes" #-}
countCandidates :: Puzzle -> Puzzle
countCandidates p = M.mapMaybe id $ M.fromList ([((r, c), v r c) | r <- allRows, c <- allColumns])
  where
    v r c =
      case at r c p of
        Nothing -> Just $ length $ candidates r c p
        Just _  -> Nothing
