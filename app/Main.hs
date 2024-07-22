module Main (main) where

import Data.Map
import Data.Maybe
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
  let solution =
        case solve puzzle' of
          (Nothing, n) -> "No solution found after " <> show n <> " steps."
          (Just solved, n) -> showPuzzle ("Solved in " <> show n <> " steps:") solved

  let candidateCounts = showPuzzle "Candidates:" $ countCandidates puzzle'

  putStr "Input: "
  putStrLn p
  putStrLn $ sideBySide [problem, candidateCounts, solution]

  solveAll ps

countCandidates :: Puzzle -> Puzzle
countCandidates p = fromList $ catMaybes $ fmap lft [((r, c), v r c) | r <- allRows, c <- allColumns]
  where
    v r c =
      case at r c p of
        Nothing -> Just $ length $ candidates r c p
        Just _ -> Nothing
    lft (_, Nothing) = Nothing
    lft ((r, c), Just v') = Just ((r, c), v')
