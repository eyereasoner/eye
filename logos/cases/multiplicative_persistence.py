#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# Multiplicative Persistence sweep (computational check)
#
# What this script does
# ---------------------
# For each n in [1, N], repeatedly replace n by the product of its decimal digits
# until a single-digit is reached. The number of iterations is the
# “multiplicative persistence” of n. We:
#   • verify / compute persistence for all n ≤ N,
#   • find the maximum persistence and its witnesses,
#   • track the distribution of persistence values,
#   • track the final single-digit “attractor” distribution,
#   • show example chains and record-breakers.
#
# Output format
# -------------
# 1/ Answer         : concise results (range checked, max persistence, summaries)
# 2/ Reason why     : what persistence is + how we computed it
# 3/ Check (harness): distributions, examples, record-breakers, top-K list
#
# Usage
# -----
#   python persistence.py                      # uses default N
#   python persistence.py -n 300000            # custom upper bound
#   python persistence.py -n 300000 --top 15   # show more top entries
#   python persistence.py --examples 39 77 679 6788
#
# Notes
# -----
# - This isn’t proving anything about *unbounded* persistence; it’s a finite sweep.
# - We memoise (steps, end-digit) for speed.
# - A small guard caps per-trajectory steps in case something goes wrong.
# -----------------------------------------------------------------------------

from __future__ import annotations
from typing import Dict, List, Tuple
from collections import Counter
import argparse

DEFAULT_LIMIT   = 200_000     # default highest start value to test
MAX_STEPS_GUARD = 1_000       # guard against runaway loops (should never trigger)

# Types
StepEnd = Tuple[int, int]     # (steps_to_single_digit, final_single_digit)

def digit_product(n: int) -> int:
    """Return the product of the decimal digits of n (n >= 0)."""
    if n == 0:
        return 0
    prod = 1
    while n:
        n, d = divmod(n, 10)
        prod *= d
        if prod == 0:
            return 0
    return prod

def persistence_steps(n: int, cache: Dict[int, StepEnd], guard: int = MAX_STEPS_GUARD) -> StepEnd:
    """
    Return (steps, end_digit) for start value n.
    *steps* is the number of digit-product iterations to reach a single digit.
    *end_digit* is that final single digit (0..9).
    Discovered results are cached for reuse.
    """
    x = n
    steps = 0
    path: List[int] = []

    while x not in cache:
        if x < 10:
            cache[x] = (0, x)  # base: already a single digit
            break
        path.append(x)
        steps += 1
        if steps > guard:
            raise RuntimeError(f"Exceeded {guard} steps for n={n} (unexpected for base-10 persistence)")
        x = digit_product(x)

    tail_steps, tail_end = cache[x]
    total = steps + tail_steps

    # write back for the discovered path
    for s, value in enumerate(reversed(path), 1):
        cache[value] = (tail_steps + s, tail_end)

    return total, tail_end

def build_chain(n: int) -> List[int]:
    """Return the full sequence n, f(n), f(f(n)), … down to a single digit."""
    seq = [n]
    while seq[-1] >= 10:
        seq.append(digit_product(seq[-1]))
    return seq

def parse_args() -> argparse.Namespace:
    ap = argparse.ArgumentParser(description="Multiplicative persistence sweep up to N.")
    ap.add_argument("-n", "--limit", type=int, default=DEFAULT_LIMIT,
                    help=f"Upper bound N (verify all n in [1, N]). Default: {DEFAULT_LIMIT}")
    ap.add_argument("--top", type=int, default=10,
                    help="Show top K starts by highest persistence. Default: 10")
    ap.add_argument("--examples", type=int, nargs="*", default=[39, 77, 679, 6788],
                    help="Specific n values to display chains for.")
    return ap.parse_args()

def main() -> None:
    args  = parse_args()
    N     = max(1, int(args.limit))
    TOPK  = max(1, int(args.top))

    cache: Dict[int, StepEnd] = {i: (0, i) for i in range(10)}  # base cases

    max_steps   = 0
    max_witness = 1
    dist        = Counter()   # persistence steps -> count
    end_dist    = Counter()   # final single digit -> count
    top_list: List[Tuple[int, int]] = []  # (steps, n), kept small
    record_breakers: List[Tuple[int, int]] = []  # (n, steps) when a new max is hit

    for n in range(1, N + 1):
        steps, end = persistence_steps(n, cache)
        dist[steps] += 1
        end_dist[end] += 1

        if steps > max_steps:
            max_steps, max_witness = steps, n
            record_breakers.append((n, steps))

        # maintain simple top-K by steps (break ties by smaller n)
        if len(top_list) < TOPK:
            top_list.append((steps, n))
        else:
            i_min, (s_min, n_min) = min(enumerate(top_list), key=lambda t: (t[1][0], -t[1][1]==-t[1][1]))
            if steps > s_min or (steps == s_min and n < n_min):
                top_list[i_min] = (steps, n)

    top_list.sort(key=lambda t: (t[0], -t[1]), reverse=True)

    # -------------------- 1/ Answer ------------------------------------------
    print("1/ Answer")
    print(f"- Verified multiplicative persistence for 1 … {N:,}.")
    print(f"- Maximum persistence observed: {max_steps} steps (witness: {max_witness}).")
    # compact end-digit distribution
    ed_sorted = sorted(end_dist.items())
    ed_msg = ", ".join(f"{d}:{c:,}" for d, c in ed_sorted)
    print(f"- Final single-digit distribution (0–9): {ed_msg}")
    print()

    # -------------------- 2/ Reason why --------------------------------------
    print("2/ Reason why")
    print("- Multiplicative persistence of n is the number of times you must multiply n’s digits")
    print("  to reach a single digit (e.g., 39 → 27 → 14 → 4 has persistence 3).")
    print("- We compute it for each n, caching (steps, end-digit) so repeated subproblems are O(1).")
    print("- Zeros in the digits immediately send the next step to 0, ending the process quickly.")
    print("- This is a finite check up to N; it does not address whether persistence is unbounded.")
    print()

    # -------------------- 3/ Check (harness) ---------------------------------
    print("3/ Check (harness)")
    # Distribution summary (truncate for tidiness)
    print("Distribution (#n with exactly k steps), for n ≤", f"{N:,}")
    keys = sorted(dist.keys())
    MAX_ROWS = 25
    for i, k in enumerate(keys):
        if i == MAX_ROWS:
            rest = len(keys) - MAX_ROWS
            print(f"... ({rest} more k-values)")
            break
        print(f"{k:3d} → {dist[k]:8d}")

    # Record-breakers
    if record_breakers:
        print("\nRecord-breakers (first time a new max is reached):")
        for n, s in record_breakers[:20]:
            print(f"  n={n:<12d} steps={s}")
        if len(record_breakers) > 20:
            print(f"  ... ({len(record_breakers) - 20} more)")

    # Top K list
    print(f"\nTop {min(TOPK, len(top_list))} by persistence:")
    for rank, (steps, n) in enumerate(top_list, 1):
        print(f"{rank:2d}. n = {n:>10d}  →  steps = {steps}")

    # Example chains
    ex = []
    seen = set()
    for x in (args.examples or []):
        if x >= 1 and x not in seen:
            seen.add(x)
            ex.append(x)
    print("\nSample chains:")
    for x in ex:
        chain = build_chain(x)
        print(f"n={x:>10d} → {chain}")

if __name__ == "__main__":
    main()

