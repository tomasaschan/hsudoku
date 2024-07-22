module Main (main) where

import Data.Map
import Puzzle
import Puzzle.Print
import Solver
import System.Environment

main :: IO ()
main = do
  args <- getArgs

  solveAll args

solveAll :: [String] -> IO ()
solveAll [] = return ()
solveAll (p : ps) = do
  let puzzle' = puzzle p

  let problem = showPuzzle "Unsolved:" puzzle'

  let cands = countCandidates puzzle'
  let combinations = product $ elems cands
  let candidateCounts = showPuzzle ("Candidates (n=" <> show combinations <> "):") cands

  putStr "Input: "
  putStrLn p
  putStrLn $ sideBySide [problem, candidateCounts]

  let solution =
        case solve puzzle' of
          (Nothing, n) -> "No solution found after " <> show n <> " steps."
          (Just solved, n) -> showPuzzle ("Solved in " <> show n <> " steps:") solved

  putStrLn solution

  solveAll ps
