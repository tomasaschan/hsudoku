module Solver.OnlyCandidate where

import qualified Data.Map   as M
import           Data.Maybe
import           Puzzle
import           Solver

onlyCandidate :: Technique
onlyCandidate = listToMaybe . mapMaybe snd . M.toList . M.mapWithKey toSingleCandidate
    where
      toSingleCandidate c (Candidates [v]) = Just (Solve c v)
      toSingleCandidate _ _                = Nothing
