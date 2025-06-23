#!/usr/bin/env python3
"""
ackermann.py  –  non-recursive Python translation of the EYEREASONER rules

Two-argument Ackermann                       A₂(x, y) = H(x, y+3, 2) – 3
Hyper-operation H implemented with *loops*:
    H(0, y, z) = y + 1
    H(1, y, z) = y + z
    H(2, y, z) = y · z
    H(x≥3, 0, z) = 1
    H(x≥3, y>0, z) =      # loop-based expansion of the last rule
        repeat y times:   result = H(x-1, result, z)   (start result=1)
"""

import sys
try:
    # 0 = “no limit”, or pick any value ≥ 19729
    sys.set_int_max_str_digits(0)      # or e.g.  50_000
except AttributeError:
    # Older Pythons (<3.11) don’t have the limiter
    pass

# ---------------------------------------------------------------------------
# Hyper-operation (iterative — no deep Python recursion)
# ---------------------------------------------------------------------------
def hyper(x: int, y: int, z: int) -> int:
    if x == 0:                       # successor
        return y + 1
    if x == 1:                       # addition
        return y + z
    if x == 2:                       # multiplication
        return y * z
    if y == 0:                       # identity element for x ≥ 3
        return 1

    result = 1
    for _ in range(y):               # outer loop replaces the “y>0” recursion
        result = hyper(x - 1, result, z)
    return result


# ---------------------------------------------------------------------------
# Two-argument Ackermann A₂(x, y)
# ---------------------------------------------------------------------------
def ackermann2(x: int, y: int) -> int:
    return hyper(x, y + 3, 2) - 3


# ---------------------------------------------------------------------------
# Demo — the exact 12 queries from the N3 log:query block
# ---------------------------------------------------------------------------
if __name__ == "__main__":
    queries = [
        (0, 0), (0, 6), (1, 2), (1, 7),
        (2, 2), (2, 9), (3, 4), (3, 14),
        (4, 0), (4, 1), (4, 2), (5, 0),
    ]

    for idx, (x, y) in enumerate(queries):
        value = ackermann2(x, y)
        label = f"A{idx}"

        print(f"{label} = {value}")

