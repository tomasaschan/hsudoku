module SolverSpec where

import Puzzle
import Solver
import Solver.OnlyCandidate
import Test.Hspec

run :: Puzzle -> Maybe Puzzle
run = fst . solve onlyCandidate

spec :: Spec
spec = describe "solver" $ do
  it "solves the first example" $ do
    pendingWith "skipping this one for now"
    let problem = "070000043040009610800634900094052000358460020000800530080070091902100005007040802"
    let solved = "679518243543729618821634957794352186358461729216897534485276391962183475137945862"

    run (puzzle problem) `shouldBe` Just (puzzle solved)

  it "solves the second example" $ do
    pendingWith "skipping this one for now"
    let problem = "301086504046521070500000001400800002080347900009050038004090200008734090007208103"
    let solved = "371986524846521379592473861463819752285347916719652438634195287128734695957268143"

    run (puzzle problem) `shouldBe` Just (puzzle solved)

  it "solves ron's example" $ do
    pendingWith "we're not smart enough for this one yet"
    let problem = "005024013006031000001089507160097005758300090009805000507060324010450970043002000"
    let solved = "985724613476531289231689547164297835758316492329845761597168324612453978843972156"

    let actual = solve onlyCandidate (puzzle problem)

    fst actual `shouldBe` Just (puzzle solved)
    snd actual `shouldSatisfy` (< 100000)
