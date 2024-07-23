{-# LANGUAGE LambdaCase #-}

module Solver.OnlyCandidateSpec where

import           Puzzle
import           Solver
import           Solver.OnlyCandidate
import           Test.Hspec

spec :: Spec
spec = describe "only candidates technique" $ do
  let p = puzzle "005024013006031000001089507160097005758300090009805000507060324010450970043002000"

  it "should emit correct edits" $ do
    let expected =
          [ Solve (2, 3) 6,
            Solve (3, 3) 2,
            Solve (4, 5) 6,
            Solve (6, 5) 8,
            Solve (7, 2) 2
          ] :: [Edit]

    let actual = onlyCandidate p

    actual `shouldSatisfy` (\case Just e -> e `elem` expected; _ -> False)
