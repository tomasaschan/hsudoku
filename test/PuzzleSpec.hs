module PuzzleSpec where

import           Data.List    (intercalate)
import           Puzzle
import           Puzzle.Print
import           Test.Hspec

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

      let actual = pretty . puzzle $ input
      actual `shouldBe` expected

  describe "components" $ do
    it "returns components in the top left correctly" $ do
      let actual = components 0 0

      actual
        `shouldBe` [
                     -- row 0
                     [ (0, 1),
                       (0, 2),
                       (0, 3),
                       (0, 4),
                       (0, 5),
                       (0, 6),
                       (0, 7),
                       (0, 8)
                     ],
                     -- col 0
                     [ (1, 0),
                       (2, 0),
                       (3, 0),
                       (4, 0),
                       (5, 0),
                       (6, 0),
                       (7, 0),
                       (8, 0)
                     ],
                     -- subgrid 0
                     [ (0, 1),
                       (0, 2),
                       (1, 0),
                       (1, 1),
                       (1, 2),
                       (2, 0),
                       (2, 1),
                       (2, 2)
                     ]
                   ]

    it "returns components in the mid right correctly" $ do
      let actual = components 4 8

      actual
        `shouldBe` [
                     -- row 4
                     [ (4, 0),
                       (4, 1),
                       (4, 2),
                       (4, 3),
                       (4, 4),
                       (4, 5),
                       (4, 6),
                       (4, 7)
                     ],
                     -- col 8
                     [ (0, 8),
                       (1, 8),
                       (2, 8),
                       (3, 8),
                       (5, 8),
                       (6, 8),
                       (7, 8),
                       (8, 8)
                     ],
                     -- subgrid 5
                     [ (3, 6),
                       (3, 7),
                       (3, 8),
                       (4, 6),
                       (4, 7),
                       (5, 6),
                       (5, 7),
                       (5, 8)
                     ]
                   ]

  describe "candidates" $ do
    let puzzle' = puzzle "004300209005009001070060043006002087190007400050083000600000105003508690042910300"

    it "returns candidates in the top left correctly" $ do
      candidates 0 0 puzzle' `shouldBe` [8]

    it "returns candidates in the mid right correctly" $ do
      candidates 4 7 puzzle' `shouldBe` [2, 3, 5, 6]
