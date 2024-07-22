module Solver where

import Puzzle
import Data.Maybe
import Data.Function

type Technique = Puzzle -> Maybe Edit

trySolve :: Technique -> Puzzle -> Puzzle
trySolve _ p | isSolved p = p
trySolve t p = case t p of
  Nothing -> p
  Just e -> trySolve t (edit p e)

combine :: [Technique] -> Technique
combine ts p = mapMaybe ($ p) ts & listToMaybe
