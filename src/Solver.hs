module Solver where

import Data.Map
import Debug.Trace
import Puzzle
import Puzzle.Print

solve :: Technique -> Puzzle -> (Maybe Puzzle, Int)
solve t p = dfs t [p] 0

type Technique = Puzzle -> [Edit]

dfs :: Technique -> [Puzzle] -> Int -> (Maybe Puzzle, Int)
dfs _ [] n = (Nothing, n)
dfs _ (p : _) n | isSolved p = (Just p, n)
dfs t (p : ps) n | not (isValid p) = dfs t ps (n + 1)
dfs _ _ n | n > 1000 = (Nothing, n)
dfs t (p : ps) n =
  let es = t p
      ps' = fmap (edit p) es
      editDisplay = sideBySide (showPuzzle "Current:" p : fmap (\(r, c, v) -> showPuzzle ("Edit " <> show (r, c, v)) (fromList [((r, c), v)])) es)
      tr = case n of
        0 -> trace (sideBySide [showPuzzle "Unsolved:" p, showPuzzle "Candidates:" (countCandidates p)])
        n' | n' < 2 -> trace ("trying another " <> show (length es) <> " edits: " <> show es) . trace editDisplay
        _ -> id
      tr1 = case n `mod` 10000 of
        0 | n > 0 -> traceShow (n, length ps)
        _ -> id
   in tr $
        dfs t (ps' <> ps) $ tr1 (n + 1)
