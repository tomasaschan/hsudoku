module PuzzleSpec where

import Data.List (intercalate)
import Puzzle.Parse
import Puzzle.Print
import Test.Hspec

spec :: Spec
spec = describe "puzzle io" $ do
    it "can read and display the example" $ do
        let input = "004300209005009001070060043006002087190007400050083000600000105003508690042910300"
        let expected =
                intercalate "\n" [
                    "┏━┯━┯━┳━┯━┯━┳━┯━┯━┓",
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
                ] <> "\n"

        let actual = pretty . cells $ input
        actual `shouldBe` expected
