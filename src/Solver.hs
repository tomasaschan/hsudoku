module Solver where

import Debug.Trace
import Puzzle
import qualified Solver.BruteForce as BF

solve :: Puzzle -> (Maybe Puzzle, Int)
solve p = dfs BF.nexts [p] 0

type Technique = Puzzle -> [Edit]

dfs :: Technique -> [Puzzle] -> Int -> (Maybe Puzzle, Int)
dfs _ [] n = (Nothing, n)
dfs _ (p : _) n | isSolved p = (Just p, n)
dfs _ _ n | n > 100000 = (Nothing, n)
dfs t (p : ps) n =
  let es = t p
      ps' = fmap (edit p) es
   in dfs t (ps' <> ps) $ case n `mod` 1000 of
        0 -> traceShow (n + 1, length ps) (n + 1)
        _ -> n + 1
