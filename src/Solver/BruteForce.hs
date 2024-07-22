module Solver.BruteForce where

import Puzzle
import Solver

nexts :: Technique
nexts p = [(r, c, v) | (r, c) <- unsolved p, v <- candidates r c p]
