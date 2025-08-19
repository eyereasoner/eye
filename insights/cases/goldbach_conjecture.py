#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# Goldbach sweep (computational check)
#
# What this script does
# ---------------------
# Verifies the (strong) Goldbach conjecture up to a chosen bound N:
# every even integer > 2 can be written as the sum of two primes.
# It:
#   • checks all even n in [4, N] for at least one prime decomposition n = p + q,
#   • (optionally) counts how many such decompositions each even n has,
#   • reports extremes and sample decompositions.
#
# Output format
# -------------
# 1/ Answer         : concise results (verified up to N? any counterexamples? extremes)
# 2/ Reason why     : what the conjecture states + how we verified it here
# 3/ Check (harness): distribution summary, example decompositions, edge cases
#
# Usage
# -----
#   python goldbach_conjecture.py                # uses default N
#   python goldbach_conjecture.py -n 500000      # custom upper bound
#   python goldbach_conjecture.py -n 500000 --no-dist   # skip counting all representations
#
# Notes
# -----
# - Counting *all* decompositions for every even number can be slow for very
#   large N. By default we compute a distribution up to min(N, DIST_LIMIT).
#   Use --no-dist to skip that step, or increase --dist-limit if you want more.
# -----------------------------------------------------------------------------

from collections import Counter
import argparse
import math
from typing import Iterable, List, Optional, Tuple

Pair = Tuple[int, int]

def sieve(limit: int) -> bytearray:
    """Simple sieve of Eratosthenes; returns bytearray is_prime[0..limit]."""
    is_prime = bytearray(b"\x01") * (limit + 1)
    if limit >= 0: is_prime[0] = 0
    if limit >= 1: is_prime[1] = 0
    for p in range(2, int(math.isqrt(limit)) + 1):
        if is_prime[p]:
            start = p * p
            is_prime[start:limit + 1:p] = b"\x00" * (((limit - start) // p) + 1)
    return is_prime

def primes_up_to(limit: int, is_prime: bytearray) -> List[int]:
    """Return the list of primes ≤ limit using the given sieve array."""
    return [i for i in range(2, limit + 1) if is_prime[i]]

def goldbach_first_pair(n: int, is_prime: bytearray, primes: Iterable[int]) -> Optional[Pair]:
    """Return one prime pair (p, q) with p + q = n (unordered, p ≤ q), or None if none exists."""
    for p in primes:
        if p > n - p: break
        q = n - p
        if is_prime[q]:
            return (p, q)
    return None

def goldbach_count(n: int, is_prime: bytearray, primes: Iterable[int]) -> int:
    """Return the number of unordered prime pairs (p, q) with p + q = n and p ≤ q."""
    cnt = 0
    for p in primes:
        if p > n - p: break
        if is_prime[n - p]:
            cnt += 1
    return cnt

def goldbach_pairs_limited(n: int, is_prime: bytearray, primes: Iterable[int], limit: int = 10) -> List[Pair]:
    """Return up to `limit` prime pairs for display."""
    out: List[Pair] = []
    for p in primes:
        if p > n - p: break
        q = n - p
        if is_prime[q]:
            out.append((p, q))
            if len(out) >= limit:
                break
    return out

def parse_args():
    ap = argparse.ArgumentParser(description="Goldbach conjecture sweep up to N.")
    ap.add_argument("-n", "--limit", type=int, default=20_000,
                    help="Upper bound N (verify all even numbers 4..N). Default: 20000")
    ap.add_argument("--no-dist", action="store_true",
                    help="Skip counting all representations / distribution.")
    ap.add_argument("--dist-limit", type=int, default=200_000,
                    help="Cap for distribution counting (uses min(N, dist-limit)).")
    ap.add_argument("--examples", type=int, nargs="*", default=[10, 28, 100, 1000],
                    help="Even numbers to show example decompositions for.")
    return ap.parse_args()

if __name__ == "__main__":
    args = parse_args()
    N = max(4, int(args.limit))

    # ---- Sieve and prime list ------------------------------------------------
    is_prime = sieve(N)
    primes = primes_up_to(N, is_prime)

    # ---- Verify all even numbers up to N have at least one representation ----
    counterexamples: List[int] = []
    first_pairs: dict[int, Pair] = {}   # store a witness pair for a few n (optional)

    for n in range(4, N + 1, 2):
        pair = goldbach_first_pair(n, is_prime, primes)
        if pair is None:
            counterexamples.append(n)
            # no early break: keep scanning to collect more (if any)
        else:
            # keep a tiny sample of witnesses (for potential debugging / display)
            if n <= 100 or n in (N,):
                first_pairs[n] = pair

    # ---- Optionally compute distribution of #representations -----------------
    do_dist = not args.no_dist
    dist_limit = min(N, int(args.dist_limit))
    dist: Counter[int] = Counter()
    min_count_info: Optional[Tuple[int, int]] = None  # (n, count) for smallest count ≥ 1
    max_count_info: Optional[Tuple[int, int]] = None  # (n, count) for largest count

    if do_dist:
        for n in range(4, dist_limit + 1, 2):
            c = goldbach_count(n, is_prime, primes)
            dist[c] += 1
            if c >= 1:
                if (min_count_info is None) or (c < min_count_info[1]):
                    min_count_info = (n, c)
                if (max_count_info is None) or (c > max_count_info[1]):
                    max_count_info = (n, c)

    # ---- Prepare examples list (clip to ≤ N and even) ------------------------
    examples = []
    for x in (args.examples + [N]):
        x = min(x, N)
        if x % 2 == 1:
            x -= 1
        if x >= 4 and x not in examples:
            examples.append(x)

    # -------------------- 1/ Answer ------------------------------------------
    print("1/ Answer")
    if counterexamples:
        print(f"- Counterexamples found up to N={N}: {len(counterexamples)} (first few): {counterexamples[:10]}")
    else:
        print(f"- Verified: every even n in [4, {N}] has at least one prime sum n = p + q.")
    if do_dist and dist:
        min_msg = f"{min_count_info[0]} (count={min_count_info[1]})" if min_count_info else "n/a"
        max_msg = f"{max_count_info[0]} (count={max_count_info[1]})" if max_count_info else "n/a"
        print(f"- Distribution computed for evens up to {dist_limit}.")
        print(f"- Fewest representations observed: {min_msg}")
        print(f"- Most representations observed   : {max_msg}")
    print()

    # -------------------- 2/ Reason why --------------------------------------
    print("2/ Reason why")
    print("- Goldbach’s (strong) conjecture says every even integer > 2 is the sum of two primes.")
    print("- We do a computational check: build a prime table (sieve) and, for each even n,")
    print("  look for some p ≤ n/2 with (n − p) also prime. If found, n has a Goldbach decomposition.")
    print("- This verifies the conjecture only up to N; it is not a proof for all integers.")
    print()

    # -------------------- 3/ Check (harness) ---------------------------------
    print("3/ Check (harness)")
    if do_dist and dist:
        # Show a compact distribution summary
        print("Distribution (#evens with exactly k representations), for n ≤", dist_limit)
        keys = sorted(dist.keys())
        MAX_ROWS = 25
        for i, k in enumerate(keys):
            if i == MAX_ROWS:
                rest = len(keys) - MAX_ROWS
                print(f"... ({rest} more k-values)")
                break
            print(f"{k:3d} → {dist[k]:6d}")
        if min_count_info:
            n_min, c_min = min_count_info
            pairs_min = goldbach_pairs_limited(n_min, is_prime, primes, limit=10)
            print(f"\nExample pairs for n={n_min} (count={c_min}, up to 10 shown): {pairs_min}")
        if max_count_info:
            n_max, c_max = max_count_info
            pairs_max = goldbach_pairs_limited(n_max, is_prime, primes, limit=10)
            print(f"Example pairs for n={n_max} (count={c_max}, first 10 shown): {pairs_max}")
    else:
        print("(distribution skipped; run without --no-dist to compute it)")

    # A few example decompositions (always shown)
    print("\nSample decompositions:")
    for n in examples:
        pair_list = goldbach_pairs_limited(n, is_prime, primes, limit=10)
        print(f"n={n:>7d} → {pair_list[:10]}{' ...' if len(pair_list) == 10 else ''}")

    # If there were any counterexamples, list them explicitly.
    if counterexamples:
        print("\nCounterexamples (all):", counterexamples)

