module Puzzle.Print where

import Data.List.Extra
import Data.Maybe
import Puzzle
import Text.PrettyPrint hiding ((<>))
import Text.Printf

pretty :: Puzzle -> String
pretty p = render $ text $ top <> rows <> bottom
  where
    top = "┏━┯━┯━┳━┯━┯━┳━┯━┯━┓\n"
    bottom = "┗━┷━┷━┻━┷━┷━┻━┷━┷━┛\n"

    every3 outer inner gen = intercalate outer $ [intercalate inner $ fmap gen ns | ns <- [[0 .. 2], [3 .. 5], [6 .. 8]]]
    cell r c = maybe " " show (at r c p)
    row r = "┃" <> every3 "┃" "│" (cell r) <> "┃\n"
    rows = every3 "┣━┿━┿━╋━┿━┿━╋━┿━┿━┫\n" "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n" row

showPuzzle :: String -> Puzzle -> String
showPuzzle label p = label <> "\n" <> pretty p

sideBySide :: [String] -> String
sideBySide [] = ""
sideBySide [single] = single
sideBySide (a : b : rest) = sideBySide (unlines (zipWithLongest (\x y -> fromMaybe "" x <> "  " <> fromMaybe "" y) (printf "%-19s" <$> lines a) (lines b)) : rest)
