module Solver where

import           Data.Function
import qualified Data.Map      as M
import           Data.Maybe
import           Puzzle

type Technique = Puzzle -> Maybe Edit

trySolve :: Technique -> Puzzle -> Puzzle
trySolve _ p | isSolved p = p
trySolve t p = case t p of
  Nothing -> p
  Just e  -> trySolve t (edit p e)

combine :: [Technique] -> Technique
combine ts p = mapMaybe ($ p) ts & listToMaybe

data Edit = Solve Coord Int | Eliminate (M.Map Coord [Int]) deriving (Show, Eq)

edit :: Puzzle -> Edit -> Puzzle
edit p (Solve c v) = M.insert c (Solved v) p
edit p (Eliminate m) = M.foldrWithKey eliminate p m
  where
    eliminate k cs p' =
      case p' M.! k of
        (Candidates cs') -> M.insert k (Candidates (filter (`notElem` cs) cs')) p'
        _                -> p'
