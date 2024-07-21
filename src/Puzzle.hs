module Puzzle where

import qualified Data.Map as M
import Data.Maybe

type Item = (Int, Int, Int)

type Puzzle = M.Map (Int, Int) Int

fromList :: [Item] -> Puzzle
fromList = M.fromList . fmap (\(r, c, v) -> ((r, c), v))

at :: Int -> Int -> Puzzle -> Maybe Int
at r c = M.lookup (r, c)

type Component = [Item]

-- | Returns the components (row, column and subgrid) that contain the cell at (r, c)
components :: Int -> Int -> Puzzle -> [Component]
components r c p =
  fmap
    catMaybes
    [ [item r c' | c' <- [0 .. 8]],
      [item r' c | r' <- [0 .. 8]],
      [item r' c' | r' <- sub r, c' <- sub c]
    ]
  where
    sub x = [x' .. x' + 2] where x' = 3 * (x `div` 3)
    item r' c' = case at r' c' p of
      Just v -> Just (r', c', v)
      Nothing -> Nothing
