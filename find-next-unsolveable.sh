#!/bin/bash

# run this script on each update of the program by running,
# in two separate terminals:
#
# stack build --file-watch
# ls find-next-unsolveable.sh $(stack path --local-install-root)/bin/* | entr -rcc ./find-next-unsolveable.sh

set -e

# read the 1.5G file of puzzles, output 100 at a time and send them to hsudoku-exe
# the latter will batch-solve them and output the first unsolveable one
# we can't use xargs -n100 directly to stack exec hsudoku-exe, because it will
# not exit on the first failure; that's what the while loop (with set -e) does.
<data/sudoku.csv \
awk -F, 'NR > 1 {print $1}' \
| xargs -n100 echo 2>/dev/null \
| while read -r puzzles; do
    echo "$puzzles" | xargs stack exec hsudoku-exe
done
