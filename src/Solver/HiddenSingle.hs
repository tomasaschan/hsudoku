{-# LANGUAGE LambdaCase #-}

module Solver.HiddenSingle where

import qualified Data.Map as M
import Data.Maybe
import Puzzle
import Solver

hiddenSingle :: Technique
hiddenSingle p = listToMaybe . concatMap (findHiddenSingles p) $ unsolved p

findHiddenSingles :: Puzzle -> Coord -> [Edit]
findHiddenSingles p c = mapMaybe (findHiddenSingles1 p c) $ components c

findHiddenSingles1 :: Puzzle -> Coord -> [Coord] -> Maybe Edit
findHiddenSingles1 p c component = case unique c of [v] -> Just $ Solve c v; _ -> Nothing
 where
  unique = concatMap (filter (not . (`elem` others component)) . (\case (Candidates xs) -> xs; _ -> [])) . (`M.lookup` p)

  others = concatMap (\case (Candidates xs) -> xs; _ -> []) . mapMaybe (`M.lookup` p)
