#!/usr/bin/env python3
"""
collatz.py  —  Verify the Collatz conjecture for n = 1 … 100 000
Fast version that memoises total-stopping-times.
"""

from __future__ import annotations
from typing import Dict, Tuple

LIMIT      = 100_000          # highest start value to test
MAX_STEPS  = 10_000           # guard against runaway loops


def collatz_steps(n: int,
                  cache: Dict[int, int]) -> Tuple[int, int]:
    """
    Return (steps, peak) for start value n.
    *steps* is the total-stopping-time (including the final 1 → stop).
    *peak* is the highest integer reached along the way.

    Every discovered steps-count is cached for reuse.
    """
    x = n
    steps = 0
    peak  = x
    path  = []                     # numbers whose steps aren't cached yet

    while x != 1 and x not in cache:
        path.append(x)
        steps += 1
        if steps > MAX_STEPS:
            raise RuntimeError(
                f"Exceeded {MAX_STEPS} steps for n = {n} "
                "→ conjecture possibly false!"
            )

        # Collatz rule
        x = 3 * x + 1 if x & 1 else x >> 1
        if x > peak:
            peak = x

    # We now know the tail length:
    tail = cache.get(x, 0)         # 0 if x == 1, otherwise cached value
    total = steps + tail           # full length for *n*

    # Write results for the entire discovered path back into the cache
    #   (path[-1] is the number just before an already-cached value)
    for s, value in enumerate(reversed(path), 1):
        cache[value] = tail + s

    return total, peak


def main() -> None:

    # cache[integer] = total-stopping-time(integer)
    cache: Dict[int, int] = {1: 0}

    worst_n    = 0
    worst_step = 0
    worst_peak = 0

    for n in range(1, LIMIT + 1):
        steps, peak = collatz_steps(n, cache)

        if steps > worst_step:
            worst_step, worst_peak, worst_n = steps, peak, n
        elif peak > worst_peak:
            worst_peak = peak

    # ─── results ────────────────────────────────────────────────────────────
    print(f"Checked n = 1 … {LIMIT:,}")
    print(f"Longest total-stopping-time   : {worst_step} steps (for n = {worst_n})")
    print(f"Highest value ever observed   : {worst_peak}")
    print("Collatz conjecture holds for all tested numbers ✔️")


if __name__ == "__main__":
    main()

