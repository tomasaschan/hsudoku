module Solver.OnlyCandidateSpec where

import Puzzle
import Solver.OnlyCandidate

import Test.Hspec

spec :: Spec
spec = describe "only candidates technique" $ do
  let p = puzzle "005024013006031000001089507160097005758300090009805000507060324010450970043002000"

  it "should emit correct edits" $ do
    let expected = [
                      (2, 3, 6),
                      (3, 3, 2),
                      (4, 5, 6),
                      (6, 5, 8),
                      (7, 2, 2)
                    ]

    let actual = onlyCandidate p

    actual `shouldBe` expected
