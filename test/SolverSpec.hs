module SolverSpec where

import           Puzzle
import           Puzzle.Print
import           Solver
import           Solver.NakedPairs
import           Solver.OnlyCandidate
import           Test.Hspec

spec :: Spec
spec = describe "solver" $ do
  let solver = trySolve (combine [onlyCandidate, nakedPairs])

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

  before
    ( putStrLn
        ( sideBySide
            [ showPuzzle "before" (puzzle "000090015000100840050380207000530000000602431012009500028003004037000002400050083"),
              showPuzzle "after" (solver $ puzzle "000090015000100840050380207000530000000602431012009500028003004037000002400050083"),
              showCandidates "candidates after" (solver $ puzzle "000090015000100840050380207000530000000602431012009500028003004037000002400050083"),
              showPuzzle "correct" (puzzle "284796315673125849951384267746531928895672431312849576128963754537418692469257183")
            ]
        )
    )
    $ do
      it "makes a mistake somewhere" $ do
        let p = puzzle "000090015000100840050380207000530000000602431012009500028003004037000002400050083"

        solver p `shouldSatisfy` isSolved
