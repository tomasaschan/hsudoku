module SolverSpec where

import Puzzle
import Solver
import Solver.NakedSingle
import Test.Hspec

spec :: Spec
spec = do
  let solver = trySolve (combine [nakedSingle])

  it "solves the first example" $ do
    let problem = "070000043040009610800634900094052000358460020000800530080070091902100005007040802"
    let solved = "679518243543729618821634957794352186358461729216897534485276391962183475137945862"

    solver (puzzle problem) `shouldBe` puzzle solved

  it "solves the second example" $ do
    let problem = "301086504046521070500000001400800002080347900009050038004090200008734090007208103"
    let solved = "371986524846521379592473861463819752285347916719652438634195287128734695957268143"

    solver (puzzle problem) `shouldBe` puzzle solved

  it "solves ron's example" $ do
    pendingWith "skipping all solver tests until naked pairs is fixed"
    let problem = "005024013006031000001089507160097005758300090009805000507060324010450970043002000"
    let solved = "985724613476531289231689547164297835758316492329845761597168324612453978843972156"

    solver (puzzle problem) `shouldBe` puzzle solved

  describe "there are multiple options" $ do
    let p = puzzle "985724613476531289001689547164297835758316492009845761597168324012453970043972150"

    it "setting (2,0) to 2 and (7,0) to 6 is valid" $ do
      let edits = [Solve (2, 0) 2, Solve (7, 0) 6]
      let solved = trySolve nakedSingle $ foldr (flip edit) p edits
      solved `shouldSatisfy` isSolved

    it "setting (2,0) to 3 and (7,0) to 8 is valid" $ do
      let edits = [Solve (2, 0) 3, Solve (7, 0) 8]
      let solved = trySolve nakedSingle $ foldr (flip edit) p edits
      solved `shouldSatisfy` isSolved
