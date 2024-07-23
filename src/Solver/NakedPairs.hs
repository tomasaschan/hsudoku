module Solver.NakedPairs where

import           Data.Maybe
import           Puzzle
import           Solver

nakedPairs :: Technique
nakedPairs p = listToMaybe pairs >>= toEdit
  where
    toEdit (_, _, [], _)    = Nothing
    toEdit (r, c, v : _, _) = Just (r, c, v)

    pairs =
      map (\(r, c, cs) -> (r, c, cs, buddies r c cs))
        . filter isPair
        . fmap (\(r, c) -> (r, c, candidates r c p))
        $ unsolved p

    buddies r c cs =
      filter (not . null)
        . fmap (filter (hasCandidates cs) . filter (`elem` unsolved p))
        $ components r c

    hasCandidates cs (r', c') = cs == candidates r' c' p

    isPair (_, _, cs) = length cs == 2
