module Solver.NakedPairsSpec where

import Data.Maybe
import Puzzle
import Solver.NakedPairs
import Test.Hspec

spec :: Spec
spec = describe "naked pairs technique" $ do
  let p = puzzle "985724613476531289001689547164297835758316492009845761597168324012453970043972150"

  it "finds a naked pair and constructs a valid edit from it" $ do
    let e = nakedPairs p

    let allowed =
          [ (2, 0, 2),
            (2, 0, 3),
            (3, 0, 2),
            (3, 0, 3),
            (5, 0, 2),
            (5, 0, 3),
            (7, 0, 6),
            (7, 0, 8),
            (8, 0, 6),
            (8, 0, 8),
            (7, 8, 6),
            (7, 8, 8),
            (8, 8, 6),
            (8, 8, 8)
          ]

    allowed `shouldContain` [fromMaybe (0, 0, 0) e]
