module Main (main) where

import Puzzle
import Puzzle.Parse
import Puzzle.Print
import Solver
import System.Environment
import Text.Printf

main :: IO ()
main = do
  args <- getArgs

  solveAll args

solveAll :: [String] -> IO ()
solveAll [] = return ()
solveAll (p : ps) = do
  let puzzle = cells p

  let unsolved = showPuzzle "Unsolved:" puzzle
  let solved = showPuzzle "Solved:" $ solve puzzle

  putStrLn $ sideBySide unsolved solved

  solveAll ps

showPuzzle :: String -> Puzzle -> String
showPuzzle label p = label <> "\n" <> pretty p

sideBySide :: String -> String -> String
sideBySide a b = unlines $ zipWith (\x y -> x <> "  " <> y) (printf "%-19s" <$> lines a) (lines b)
