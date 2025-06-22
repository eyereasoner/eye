#!/usr/bin/env python3
"""
Deterministic Sierpiński Triangle (ASCII)

Run without arguments for a 64-row triangle drawn with '*'.
You can pass:
    • the height (row count) as the first argument
    • an alternative glyph as the second argument

Examples
--------
    python sierpinski_ascii.py        # 32 rows, '*'
    python sierpinski_ascii.py 64     # 64 rows
    python sierpinski_ascii.py 64 #   # 64 rows, '#'
"""
import sys

HEIGHT      = int(sys.argv[1]) if len(sys.argv) > 1 else 64
POINT_CHAR  = sys.argv[2] if len(sys.argv) > 2 else "*"
SPACE_CHAR  = " "

row = [1]  # first row of Pascal’s triangle (mod 2)

for _ in range(HEIGHT):
    line = "".join(POINT_CHAR if bit else SPACE_CHAR for bit in row)
    print(line.center(2 * HEIGHT))   # keep the triangle aligned

    # next row via Pascal’s rule mod 2 → XOR of adjacent bits, wrapped in 1’s
    row = [1] + [row[i] ^ row[i + 1] for i in range(len(row) - 1)] + [1]
