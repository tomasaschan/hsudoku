module Main (main) where

import Puzzle
import Puzzle.Print
import Solver
import Solver.NakedSingle
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs

  solveAll args

solveAll :: [String] -> IO ()
solveAll [] = return ()
solveAll (p : ps) = do
  putStr p
  hFlush stdout
  let puzzle' = puzzle p
  let solved = trySolve (combine [nakedSingle]) puzzle'

  if isSolved solved
    then putStrLn " ✅"
    else do
      putStrLn " ❌"

      putStrLn ""
      putStrLn $
        sideBySide
          [ showPuzzle "Input:" puzzle',
            showPuzzle "State at end:" solved
          ]

      putStrLn "State at end:"
      putStrLn $ pack solved
      exitFailure

  solveAll ps
