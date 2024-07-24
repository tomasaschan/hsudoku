{-# LANGUAGE LambdaCase #-}

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
  let solved = trySolve (combine [onlyCandidate, nakedPairs]) puzzle'

  putStrLn ("Input: " <> p)
  putStrLn $
    sideBySide
      [ showPuzzle "Unsolved:" puzzle',
        showCandidates "Candidates:" puzzle',
        if isSolved solved
          then showPuzzle "Solved!" solved
          else showPuzzle "Failed :(" solved
      ]

  if not $ isSolved solved
    then putStrLn $ "State at end: " <> pack solved
    else return ()

  solveAll ps
