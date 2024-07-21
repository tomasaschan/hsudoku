module Puzzle where

import qualified Data.Map as M

data Cell = Solved Int | Unsolved deriving (Show, Eq)

type Item = (Int, Int, Cell)

type Puzzle = M.Map (Int, Int) Cell

fromList :: [Item] -> Puzzle
fromList = M.fromList . fmap (\(r, c, v) -> ((r, c), v))

at :: Int -> Int -> Puzzle -> Cell
at r c p = p M.! (r, c)

type Component = [Item]

-- | Returns the components (row, column and subgrid) that contain the cell at (r, c)
components :: Int -> Int -> Puzzle -> [Component]
components r c p =
  [ [(r, c', at r c' p) | c' <- [0 .. 8]],
    [(r', c, at r' c p) | r' <- [0 .. 8]],
    [(r', c', at r' c' p) | r' <- sub r, c' <- sub c]
  ]
  where
    sub x = [x' .. x' + 2] where x' = 3 * (x `div` 3)
