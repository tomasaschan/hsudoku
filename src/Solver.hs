module Solver where

import Puzzle

type Technique = Puzzle -> Maybe Edit

trySolve :: Technique -> Puzzle -> Puzzle
trySolve _ p | isSolved p = p
trySolve t p = case t p of
  Nothing -> p
  Just e -> trySolve t (edit p e)
