module Main (main) where

import Puzzle
import Puzzle.Print
import Solver
import Solver.NakedPairs
import Solver.OnlyCandidate
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
  let candidateCounts = showPuzzle ("Candidates:") cands

  let solved = trySolve (combine [onlyCandidate, nakedPairs]) puzzle'
  let solution =
        case solved of
          p' | isSolved p' -> showPuzzle "Solved!" p'
          p' -> showPuzzle "Failed :(" p'

  putStrLn ("Input: " <> p)
  putStrLn $ sideBySide [problem, candidateCounts, solution]

  if not $ isSolved solved
    then putStrLn $ "State at end: " <> pack solved
    else return ()

  solveAll ps
