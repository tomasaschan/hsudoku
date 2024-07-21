module Puzzle.Print where

import Data.List (intercalate)
import Puzzle
import Text.PrettyPrint hiding ((<>))

prettyCell :: Cell -> String
prettyCell (Solved n) = show n
prettyCell (Candidates _) = " "

pretty :: Puzzle -> String
pretty p = render $ text $ (top <> rows <> bottom)
  where
    top = "┏━┯━┯━┳━┯━┯━┳━┯━┯━┓\n"
    bottom = "┗━┷━┷━┻━┷━┷━┻━┷━┷━┛\n"

    every3 outer inner gen = intercalate outer $ [intercalate inner $ fmap gen ns | ns <- [[0 .. 2], [3 .. 5], [6 .. 8]]]

    row r = "┃" <> every3 "┃" "│" (\c -> prettyCell $ at r c p) <> "┃\n"
    rows = every3 "┣━┿━┿━╋━┿━┿━╋━┿━┿━┫\n" "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n" row
