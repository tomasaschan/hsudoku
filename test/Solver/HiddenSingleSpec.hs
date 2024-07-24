module Solver.HiddenSingleSpec where

import Puzzle
import Solver
import Solver.HiddenSingle
import Test.Hspec

spec :: Spec
spec = do
  describe "should emit correct edits" $ do
    it "first block" $ do
      let p = puzzle "960405103020061504001703206100004329496132857002007641219546738374218965600379412"
      let e = hiddenSingle p

      let expected =
            fmap
              Just
              -- this list might not be exhaustive; it's just the ones i found manually and could verify
              [ Solve (0, 4) 2,
                Solve (2, 1) 4,
                Solve (1, 2) 3
              ]

      expected `shouldContain` [e]

    it "second block" $ do
      let p = puzzle "960425103023061504041703206100604329496132857032007641219546738374218965600379412"
      let e = hiddenSingle p

      let expected =
            fmap
              Just
              -- this list might not be exhaustive; it's just the ones i found manually and could verify
              [ Solve (1, 0) 7,
                Solve (2, 0) 5
              ]

      expected `shouldContain` [e]
