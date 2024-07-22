module Solver.BruteForce where

import Puzzle

nexts :: Puzzle -> [Edit]
nexts p = [(r, c, v) | (r, c) <- unsolved p, v <- candidates r c p]
