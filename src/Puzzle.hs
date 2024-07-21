module Puzzle where

import qualified Data.Map as M

data Cell = Solved Int | Candidates [Int] deriving (Show, Eq)

type Puzzle = M.Map (Int, Int) Cell

fromList :: [(Int, Int, Cell)] -> Puzzle
fromList = M.fromList . fmap (\(r, c, v) -> ((r, c), v))

at :: Int -> Int -> Puzzle -> Cell
at r c p = p M.! (r, c)
