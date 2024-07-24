module Main (main) where

import Puzzle
import Puzzle.Print
import Solver
import Solver.HiddenSingle
import Solver.NakedSingle
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs

  solved <- solveAll 0 args

  putStr $ "Solved " ++ show solved

  if solved /= length args
    then do
      putStrLn " puzzles before failing"
      exitFailure
    else do
      putStrLn " puzzles"
      exitSuccess

solveAll :: Int -> [String] -> IO Int
solveAll n [] = return n
solveAll n (p : ps) = do
  putStr p
  hFlush stdout
  let puzzle' = puzzle p
  let solved = trySolve (combine [nakedSingle, hiddenSingle]) puzzle'

  if isSolved solved
    then do
      putStrLn " ✅"
      solveAll (n + 1) ps
    else do
      putStrLn " ❌"

      putStrLn ""
      putStrLn $
        sideBySide
          [ showPuzzle "Input:" puzzle',
            showPuzzle "State at end:" solved,
            showCandidates "Candidates at end:" solved
          ]

      putStrLn "State at end:"
      putStrLn $ pack solved

      putStrLn "Candidates at end:"
      putStrLn $ packCandidates solved

      return n
