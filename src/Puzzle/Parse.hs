module Puzzle.Parse where

import Puzzle

cells :: String -> Puzzle
cells = fromList . fmap oneCell . filter nonzero . zip [0 ..]
  where
    nonzero (_, c) = c /= '0'
    oneCell (i, c) = (row i, col i, read [c])
    row i = i `div` 9
    col i = i `mod` 9
