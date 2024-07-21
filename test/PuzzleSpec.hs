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
                     [ (0, 0, Unsolved),
                       (0, 1, Unsolved),
                       (0, 2, Solved 4),
                       (0, 3, Solved 3),
                       (0, 4, Unsolved),
                       (0, 5, Unsolved),
                       (0, 6, Solved 2),
                       (0, 7, Unsolved),
                       (0, 8, Solved 9)
                     ],
                     -- col 0
                     [ (0, 0, Unsolved),
                       (1, 0, Unsolved),
                       (2, 0, Unsolved),
                       (3, 0, Unsolved),
                       (4, 0, Solved 1),
                       (5, 0, Unsolved),
                       (6, 0, Solved 6),
                       (7, 0, Unsolved),
                       (8, 0, Unsolved)
                     ],
                     -- subgrid 0
                     [ (0, 0, Unsolved),
                       (0, 1, Unsolved),
                       (0, 2, Solved 4),
                       (1, 0, Unsolved),
                       (1, 1, Unsolved),
                       (1, 2, Solved 5),
                       (2, 0, Unsolved),
                       (2, 1, Solved 7),
                       (2, 2, Unsolved)
                     ]
                   ]

    it "returns components in the mid right correctly" $ do
      let actual = components 4 8 puzzle

      actual
        `shouldBe` [
                     -- row 4
                     [ (4, 0, Solved 1),
                       (4, 1, Solved 9),
                       (4, 2, Unsolved),
                       (4, 3, Unsolved),
                       (4, 4, Unsolved),
                       (4, 5, Solved 7),
                       (4, 6, Solved 4),
                       (4, 7, Unsolved),
                       (4, 8, Unsolved)
                     ],
                     -- col 8
                     [ (0, 8, Solved 9),
                       (1, 8, Solved 1),
                       (2, 8, Solved 3),
                       (3, 8, Solved 7),
                       (4, 8, Unsolved),
                       (5, 8, Unsolved),
                       (6, 8, Solved 5),
                       (7, 8, Unsolved),
                       (8, 8, Unsolved)
                     ],
                     -- subgrid 8
                     [ (3, 6, Unsolved),
                       (3, 7, Solved 8),
                       (3, 8, Solved 7),
                       (4, 6, Solved 4),
                       (4, 7, Unsolved),
                       (4, 8, Unsolved),
                       (5, 6, Unsolved),
                       (5, 7, Unsolved),
                       (5, 8, Unsolved)
                     ]
                   ]
