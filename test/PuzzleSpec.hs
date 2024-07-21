module PuzzleSpec where

import Data.List (intercalate)
import Puzzle
import Puzzle.Parse
import Puzzle.Print
import Test.Hspec

spec :: Spec
spec = describe "puzzle" $ do
  describe "io" $ do
    it "can read and display the example" $ do
      let input = "004300209005009001070060043006002087190007400050083000600000105003508690042910300"
      let expected =
            intercalate
              "\n"
              [ "┏━┯━┯━┳━┯━┯━┳━┯━┯━┓",
                "┃ │ │4┃3│ │ ┃2│ │9┃",
                "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨",
                "┃ │ │5┃ │ │9┃ │ │1┃",
                "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨",
                "┃ │7│ ┃ │6│ ┃ │4│3┃",
                "┣━┿━┿━╋━┿━┿━╋━┿━┿━┫",
                "┃ │ │6┃ │ │2┃ │8│7┃",
                "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨",
                "┃1│9│ ┃ │ │7┃4│ │ ┃",
                "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨",
                "┃ │5│ ┃ │8│3┃ │ │ ┃",
                "┣━┿━┿━╋━┿━┿━╋━┿━┿━┫",
                "┃6│ │ ┃ │ │ ┃1│ │5┃",
                "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨",
                "┃ │ │3┃5│ │8┃6│9│ ┃",
                "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨",
                "┃ │4│2┃9│1│ ┃3│ │ ┃",
                "┗━┷━┷━┻━┷━┷━┻━┷━┷━┛"
              ]
              <> "\n"

      let actual = pretty . cells $ input
      actual `shouldBe` expected

  describe "components" $ do
    let puzzle = cells "004300209005009001070060043006002087190007400050083000600000105003508690042910300"

    it "returns components in the top left correctly" $ do
      let actual = components 0 0 puzzle

      actual
        `shouldBe` [
                     -- row 0
                     [ (0, 2, 4),
                       (0, 3, 3),
                       (0, 6, 2),
                       (0, 8, 9)
                     ],
                     -- col 0
                     [ (4, 0, 1),
                       (6, 0, 6)
                     ],
                     -- subgrid 0
                     [ (0, 2, 4),
                       (1, 2, 5),
                       (2, 1, 7)
                     ]
                   ]

    it "returns components in the mid right correctly" $ do
      let actual = components 4 8 puzzle

      actual
        `shouldBe` [
                     -- row 4
                     [ (4, 0, 1),
                       (4, 1, 9),
                       (4, 5, 7),
                       (4, 6, 4)
                     ],
                     -- col 8
                     [ (0, 8, 9),
                       (1, 8, 1),
                       (2, 8, 3),
                       (3, 8, 7),
                       (6, 8, 5)
                     ],
                     -- subgrid 8
                     [ (3, 7, 8),
                       (3, 8, 7),
                       (4, 6, 4)
                     ]
                   ]
