{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Puzzle.Print where

import Data.List.Extra
import Data.Map
import Data.Maybe
import Puzzle
import Text.PrettyPrint hiding ((<>))
import Text.Printf

pretty :: (Cell -> Maybe String) -> Puzzle -> String
pretty f p = render $ text $ top <> rows <> bottom
 where
  top = "┏━┯━┯━┳━┯━┯━┳━┯━┯━┓\n"
  bottom = "┗━┷━┷━┻━┷━┷━┻━┷━┷━┛\n"

  every3 outer inner gen = intercalate outer $ [intercalate inner $ fmap gen ns | ns <- [[0 .. 2], [3 .. 5], [6 .. 8]]]
  cell r c = fromMaybe " " $ f (p ! (r, c))
  row r = "┃" <> every3 "┃" "│" (cell r) <> "┃\n"
  rows = every3 "┣━┿━┿━╋━┿━┿━╋━┿━┿━┫\n" "┠─┼─┼─╂─┼─┼─╂─┼─┼─┨\n" row

showPuzzle :: String -> Puzzle -> String
showPuzzle label p = label <> "\n" <> pretty (\case Solved v -> Just (show v); _ -> Nothing) p

showCandidates :: String -> Puzzle -> String
showCandidates label p = label <> "\n" <> pretty (\case Candidates vs -> Just $ show (length vs); _ -> Nothing) p

sideBySide :: [String] -> String
sideBySide [] = ""
sideBySide [single] = unlines $ printf "%-19s" <$> lines single
sideBySide (a : b : rest) = sideBySide (unlines (zipWithLongest (\x y -> fromMaybe "" x <> "  " <> fromMaybe "" y) (printf "%-19s" <$> lines a) (printf "%-19s" <$> lines b)) : rest)

pack :: Puzzle -> String
pack = concatMap (show . fromMaybe 0 . (\case Solved v -> Just v; _ -> Nothing))

packCandidates :: Puzzle -> String
packCandidates = unlines . Data.Maybe.mapMaybe toLine . toList
  where
    toLine x =
      case x of
        (c, Candidates vs) -> Just $ show c <> ": " <> show vs
        _ -> Nothing
