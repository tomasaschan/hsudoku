module Main (main) where

import Control.Monad
import Puzzle
import Puzzle.Print
import Solver
import Solver.NakedSingle
import System.Environment

main :: IO ()
main = do
  args <- getArgs

  solveAll args

solveAll :: [String] -> IO ()
solveAll [] = return ()
solveAll (p : ps) = do
  let puzzle' = puzzle p
  let solved = trySolve (combine [nakedSingle]) puzzle'

  putStrLn ("Input: " <> p)
  putStrLn $
    sideBySide
      [ showPuzzle "Unsolved:" puzzle',
        showCandidates "Candidates:" puzzle',
        if isSolved solved
          then showPuzzle "Solved!" solved
          else showPuzzle "Failed :(" solved
      ]

  unless (isSolved solved) $ putStrLn $ "State at end: " <> pack solved

  solveAll ps
