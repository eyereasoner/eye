#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# Collatz sweep (computational check)
#
# What this script does
# ---------------------
# Verifies the Collatz conjecture for all start values n in [1, N]:
# repeatedly apply n -> 3n+1 if n is odd, else n -> n/2, and check that
# every sequence reaches 1. We also track:
#   • the total-stopping-time (number of steps to reach 1, counting the final 1→stop),
#   • the maximum value (peak) seen along each trajectory,
#   • a distribution of stopping-times, and the longest/peakiest cases.
#
# Output format
# -------------
# 1/ Answer         : concise results (verified range, longest time, highest peak)
# 2/ Reason why     : what the conjecture says + how we verified it here
# 3/ Check (harness): distribution summary and example extremes
#
# Usage
# -----
#   python collatz.py                    # uses default N=100000
#   python collatz.py -n 200000          # custom upper bound
#   python collatz.py -n 200000 --no-dist  # skip distribution summary
#
# Notes
# -----
# - This is not a proof—only a computational check up to N.
# - We memoise total-stopping-times for speed.
# - A safety cap guards against runaway loops if something goes wrong.
# -----------------------------------------------------------------------------

from __future__ import annotations
from typing import Dict, Tuple, List
from collections import Counter
import argparse

DEFAULT_LIMIT   = 100_000    # highest start value to test by default
MAX_STEPS_GUARD = 10_000     # guard against runaway loops

def collatz_steps(n: int, cache: Dict[int, int], guard: int = MAX_STEPS_GUARD) -> Tuple[int, int]:
    """
    Return (steps, peak) for start value n.
    *steps* is the total-stopping-time (including the final 1 → stop).
    *peak* is the highest integer reached along the way.

    Discovered steps-counts are cached for reuse in `cache[n]`.
    """
    x = n
    steps = 0
    peak  = x
    path: List[int] = []  # numbers whose steps aren't cached yet

    while x != 1 and x not in cache:
        path.append(x)
        steps += 1
        if steps > guard:
            raise RuntimeError(
                f"Exceeded {guard} steps for n = {n} "
                "→ conjecture possibly false or guard too small!"
            )
        # Collatz rule
        x = 3 * x + 1 if (x & 1) else (x >> 1)
        if x > peak:
            peak = x

    # We now know the tail length:
    tail  = cache.get(x, 0)   # 0 if x == 1, otherwise cached value
    total = steps + tail      # full length for n

    # Write results for the entire discovered path back into the cache
    # (path[-1] is the number just before an already-cached value)
    for s, value in enumerate(reversed(path), 1):
        cache[value] = tail + s

    return total, peak

def parse_args() -> argparse.Namespace:
    ap = argparse.ArgumentParser(description="Collatz conjecture sweep up to N.")
    ap.add_argument("-n", "--limit", type=int, default=DEFAULT_LIMIT,
                    help=f"Upper bound N (verify all n in [1, N]). Default: {DEFAULT_LIMIT}")
    ap.add_argument("--no-dist", action="store_true",
                    help="Skip distribution of total-stopping-times.")
    ap.add_argument("--top", type=int, default=10,
                    help="Show top K starts by longest stopping time. Default: 10")
    ap.add_argument("--guard", type=int, default=MAX_STEPS_GUARD,
                    help=f"Max per-trajectory steps before raising. Default: {MAX_STEPS_GUARD}")
    return ap.parse_args()

def main() -> None:
    args = parse_args()
    LIMIT = max(1, int(args.limit))
    TOPK  = max(1, int(args.top))
    GUARD = max(10, int(args.guard))

    # cache[integer] = total-stopping-time(integer)
    cache: Dict[int, int] = {1: 0}

    worst_n        = 1
    worst_steps    = 0
    worst_peak     = 1
    worst_peak_n   = 1
    dist           = Counter()     # steps -> count
    top_longest: List[Tuple[int,int]] = []  # list of (steps, n)

    # We’ll collect any anomalies (should be none)
    anomalies: List[Tuple[int, str]] = []

    for n in range(1, LIMIT + 1):
        try:
            steps, peak = collatz_steps(n, cache, guard=GUARD)
        except RuntimeError as e:
            anomalies.append((n, str(e)))
            continue

        dist[steps] += 1

        # Track longest total-stopping-time
        if steps > worst_steps:
            worst_steps, worst_peak, worst_n = steps, peak, n
        # Track highest peak observed (and its source n)
        if peak > worst_peak:
            worst_peak, worst_peak_n = peak, n

        # Maintain a lightweight top list (keep small; sort at the end)
        if len(top_longest) < TOPK:
            top_longest.append((steps, n))
        else:
            # replace the current minimum if this one is larger
            min_idx, (min_steps, _) = min(enumerate(top_longest), key=lambda t: t[1][0])
            if steps > min_steps:
                top_longest[min_idx] = (steps, n)

    top_longest.sort(reverse=True)  # highest steps first

    # -------------------- 1/ Answer ------------------------------------------
    print("1/ Answer")
    if anomalies:
        print(f"- Anomalies encountered: {len(anomalies)} (first few shown below).")
    else:
        print(f"- Verified Collatz up to n = {LIMIT:,}: every start reaches 1.")
    print(f"- Longest total-stopping-time : {worst_steps} steps (for n = {worst_n})")
    print(f"- Highest value observed      : {worst_peak} (along the trajectory starting at n = {worst_peak_n})")
    print()

    # -------------------- 2/ Reason why --------------------------------------
    print("2/ Reason why")
    print("- Collatz map: n → 3n+1 if n is odd, else n → n/2. The conjecture states every positive integer")
    print("  eventually reaches 1 when iterating this map.")
    print("- We compute each trajectory, memoising total-stopping-times in a cache to avoid recomputation.")
    print("- A guard caps per-trajectory steps to catch anomalies early. This is a finite verification up to N,")
    print("  not a proof for all integers.")
    print()

    # -------------------- 3/ Check (harness) ---------------------------------
    print("3/ Check (harness)")
    if anomalies:
        print("Anomalies (up to 5 shown):")
        for n, msg in anomalies[:5]:
            print(f"  n={n}: {msg}")
        print()

    # Distribution summary (truncate to keep tidy)
    if dist:
        print("Distribution (#starts with exactly k total-stopping steps), for n ≤", f"{LIMIT:,}")
        keys = sorted(dist.keys())
        MAX_ROWS = 25
        for i, k in enumerate(keys):
            if i == MAX_ROWS:
                rest = len(keys) - MAX_ROWS
                print(f"... ({rest} more k-values)")
                break
            print(f"{k:3d} → {dist[k]:7d}")
        print()

    # Top K starts by longest total-stopping-time
    print(f"Top {min(TOPK, len(top_longest))} by total-stopping-time:")
    for rank, (steps, n) in enumerate(top_longest, 1):
        print(f"{rank:2d}. n = {n:>7d}  →  steps = {steps}")

if __name__ == "__main__":
    main()

