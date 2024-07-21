module Puzzle.Parse where

import Puzzle

cells :: String -> Puzzle
cells = fromList . fmap oneCell . zip [0..]
  where oneCell :: ( Int, Char) -> (Int, Int, Cell)
        oneCell (i, '0') = (row i, col i, Candidates [1..9])
        oneCell (i, c) = (row i, col i, Solved $ read [c])
        row i = i `div` 9
        col i = i `mod` 9
