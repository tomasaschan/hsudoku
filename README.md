# hsudoku

0. Install [ghcup](https://www.haskell.org/ghcup/) and use it to get the other tools you need; in particular, `stack` (but you'll need `ghc` and `cabal` too)

1. Build and test with `stack build --test`. Add `--file-watch` to automatically rebuild when you change something.

2. Run on a specific puzzle with `stack exec hsudoku-exe <puzzle-string>`, where the string is the digits of the puzzle from top down, left to right, with `0` denoting empty cells. You can specify more than one puzzle if you want.
