#!/usr/bin/env python3

import sys

for puzzle in sys.argv[1:]:
    puzzle = "".join([
        puzzle[0:3],
        puzzle[9:12],
        puzzle[18:21],

        puzzle[3:6],
        puzzle[12:15],
        puzzle[21:24],

        puzzle[6:9],
        puzzle[15:18],
        puzzle[24:27],


        puzzle[27:30],
        puzzle[36:39],
        puzzle[45:48],

        puzzle[30:33],
        puzzle[39:42],
        puzzle[48:51],

        puzzle[33:36],
        puzzle[42:45],
        puzzle[51:54],


        puzzle[54:57],
        puzzle[63:66],
        puzzle[72:75],

        puzzle[57:60],
        puzzle[66:69],
        puzzle[75:78],

        puzzle[60:63],
        puzzle[69:72],
        puzzle[78:81]
    ]).replace("0",".")

    print(puzzle)
