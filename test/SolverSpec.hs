module SolverSpec where

import Puzzle
import Solver
import Test.Hspec

spec :: Spec
spec = describe "solver" $ do
  it "solves the first example" $ do
    let problem = "070000043040009610800634900094052000358460020000800530080070091902100005007040802"
    let solved = "679518243543729618821634957794352186358461729216897534485276391962183475137945862"

    pendingWith "solver not implemented yet"
    fst (solve (puzzle problem)) `shouldBe` Just (puzzle solved)

  it "solves the second example" $ do
    let problem = "301086504046521070500000001400800002080347900009050038004090200008734090007208103"
    let solved = "371986524846521379592473861463819752285347916719652438634195287128734695957268143"

    pendingWith "solver not implemented yet"
    fst (solve (puzzle problem)) `shouldBe` Just (puzzle solved)
