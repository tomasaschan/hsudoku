module Solver.OnlyCandidate where

import Data.Maybe
import Puzzle
import Solver

onlyCandidate :: Technique
onlyCandidate p = listToMaybe . mapMaybe (singleCandidate . getCandidates) . unsolved $ p
  where
    singleCandidate (r, c, [v]) = Just (r, c, v)
    singleCandidate _ = Nothing
    getCandidates (r, c) = (r, c, candidates r c p)
