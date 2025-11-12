#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Prompt → Program → Proof  (Deterministic, Self-Contained, Optimized)

SCENARIO
--------
Periodical cicadas emerge every C years. Several predator populations follow
short periodic cycles with period P years; their phases (offsets) can differ
by region/species.

Question: Do *prime* cicada cycles reduce coincidence with predators compared
to a similarly sized *composite* cycle?

In this case study we compare cicada cycles:
  - Composite: 12 years
  - Primes  : 13, 17 years
against predator periods:
  P ∈ {2, 3, 4, 6} years

DEFINITIONS
-----------
- Cicada emergence years for cycle C: {k*C | k ≥ 1}.
- Predator with period P and phase offset o: {o + k*P | k ≥ 0}.
- We count *union-overlap years* within a finite horizon: years when a cicada
  emergence coincides with at least one predator emergence (a year is counted
  once even if multiple predators coincide).

METHOD (deterministic)
----------------------
1) EXACT comparison over horizon H = lcm(all periods and candidates). For
   aligned predators (offsets = 0), report each candidate's overlap count & rate.
2) INDEPENDENT CHECK by **exhaustively enumerating all phase combinations** for
   every predator period (no randomness). For each candidate, compute mean,
   stdev, min, max overlap rate across all phase tuples.

OPTIMIZATION (what makes this fast)
-----------------------------------
Instead of scanning year-by-year, we work in the *emergence-index* domain:
- The k-th cicada emergence occurs at year y = k*C.
- Coincidence with a predator (period P, phase o) requires C*k ≡ o (mod P).
- Let g = gcd(C, P). A solution exists iff o ≡ 0 (mod g).
- Reduce to C' = C/g, P' = P/g, r = (o/g) with gcd(C', P') = 1.
- Solve for k ≡ r * inv(C' mod P') (mod P').
This yields an arithmetic progression of k’s; we union-mark these indices
using a compact bytearray. Complexity is roughly O(Σ n/P') instead of O(H),
where n = H/C. This is much faster for larger H.

WHY PRIMES HELP (intuition)
---------------------------
If C is prime and larger than P, then lcm(C,P) = C*P, so overlaps with that
predator occur only every C*P years → density 1/P relative to cicada emergences.
Composite C that shares factors with P (e.g., C=12 with P∈{2,3,4,6}) creates
more frequent coincidences.

HOW TO RUN
----------
$ python3 cicadas.py

OUTPUT
------
- # Answer: best cycle(s) by exact overlap rate.
- # Reason Why: exact counts and rates over H.
- # Check: exhaustive phase analysis stats confirming the ranking.

SCOPE / LIMITS
--------------
This is a number-theory toy model; real ecosystems include variable survival,
spatial heterogeneity, and non-integer timing. The goal is to demonstrate the
combinatorial advantage of prime intervals.
"""

from math import gcd
from functools import reduce
from itertools import product

# ---------- Utilities ----------

def lcm(a: int, b: int) -> int:
    """Least common multiple of two positive integers."""
    return a // gcd(a, b) * b

def lcm_many(ns):
    """LCM of a list of positive integers."""
    return reduce(lcm, ns, 1)

def egcd_inv(a: int, m: int) -> int:
    """
    Multiplicative inverse of a modulo m (m >= 1), assuming gcd(a, m) == 1.
    Uses extended Euclid; returns value in [0, m-1].
    """
    t, new_t = 0, 1
    r, new_r = m, a % m
    while new_r != 0:
        q = r // new_r
        t, new_t = new_t, t - q * new_t
        r, new_r = new_r, r - q * new_r
    if r != 1:
        raise ValueError("No modular inverse exists")
    return t % m

# ---------- Fast overlap counter in emergence-index space ----------

def prepare_index_maps(C: int, predator_periods):
    """
    Precompute per-predator constants for a given cicada cycle C to speed up
    repeated overlap counts with different offsets.

    For each predator period P, compute:
      g   = gcd(C, P)
      P'  = P // g
      C'  = C // g
      inv = inverse of C' modulo P'  (only used when an offset o is divisible by g)
    """
    maps = []
    for P in predator_periods:
        g = gcd(C, P)
        Pprime = P // g
        Cprime = C // g
        invCprime = egcd_inv(Cprime % Pprime, Pprime) if Pprime > 1 else 0  # mod 1 is degenerate
        maps.append((P, g, Pprime, invCprime))
    return maps

def union_overlap_count_fast(C: int, predator_periods, H: int, offsets, precomp):
    """
    Fast union-overlap count using arithmetic progressions over cicada-emergence indices.
    - C: cicada cycle
    - predator_periods: list of P
    - H: horizon (inclusive)
    - offsets: list of o for each predator
    - precomp: output of prepare_index_maps(C, predator_periods)
    """
    n = H // C                   # number of cicada emergences within [1..H]
    marked = bytearray(n)        # marked[i] == 1 means emergence index (i+1) overlaps some predator
    for (P, g, Pprime, invCprime), o in zip(precomp, offsets):
        o_mod = o % P
        if o_mod % g != 0:
            continue  # no solution to C*k ≡ o (mod P)
        if Pprime == 1:
            # Every emergence index k is a solution (period divides C and offset aligns)
            for i in range(n):
                marked[i] = 1
            continue
        r = (o_mod // g) % Pprime
        k0 = (r * invCprime) % Pprime  # solution class for k modulo P'
        # k runs over positive integers; convert to first k >= 1
        k_first = k0 if k0 >= 1 else Pprime
        # Mark all k = k_first + t*P'  up to n
        i = k_first - 1
        step = Pprime
        while i < n:
            marked[i] = 1
            i += step
    return sum(marked)

# ---------- Inputs (this case) ----------
predator_periods = [2, 3, 4, 6]   # predator cycles
candidates       = [12, 13, 17]   # cicada cycles (composite vs primes)

# Exact horizon for fair comparison
H = lcm_many(predator_periods + candidates)

# ---------- Precompute per-candidate maps (speeds up both stages) ----------
precomputed = {C: prepare_index_maps(C, predator_periods) for C in candidates}

# ---------- Exact calculation (Reason Why) ----------
exact_results = {}
for C in candidates:
    count0 = union_overlap_count_fast(
        C,
        predator_periods,
        H,
        offsets=[0]*len(predator_periods),
        precomp=precomputed[C],
    )
    exact_results[C] = {
        "overlap_years": count0,
        "horizon_years": H,
        "overlap_rate": count0 / H
    }

rank_exact = sorted(candidates, key=lambda c: exact_results[c]["overlap_rate"])
best_exact = rank_exact[0]
ties_exact = [c for c in candidates
              if abs(exact_results[c]["overlap_rate"] - exact_results[best_exact]["overlap_rate"]) < 1e-12]

# ---------- Deterministic independent CHECK ----------
# Enumerate all predator phase combinations: offsets in [0..P-1] for each P.
def exhaustive_stats(C):
    """Return mean/stdev/min/max overlap rates over all predator phases (deterministic, fast)."""
    n_cases = 1
    for P in predator_periods:
        n_cases *= P
    total = 0.0
    total_sq = 0.0
    min_rate = 1.0
    max_rate = 0.0

    for offs in product(*[range(P) for P in predator_periods]):
        count = union_overlap_count_fast(C, predator_periods, H, offsets=offs, precomp=precomputed[C])
        rate = count / H
        total += rate
        total_sq += rate * rate
        if rate < min_rate:
            min_rate = rate
        if rate > max_rate:
            max_rate = rate

    mean_rate = total / n_cases
    stdev_rate = (total_sq / n_cases - mean_rate * mean_rate) ** 0.5
    return {
        "mean_rate": mean_rate,
        "stdev_rate": stdev_rate,
        "min_rate": min_rate,
        "max_rate": max_rate,
        "cases": n_cases
    }

check_results = {C: exhaustive_stats(C) for C in candidates}
rank_check = sorted(candidates, key=lambda c: check_results[c]["mean_rate"])
best_check = rank_check[0]
ties_check = [c for c in candidates
              if abs(check_results[c]["mean_rate"] - check_results[best_check]["mean_rate"]) < 1e-12]

# ---------- Emit the P3 triad ----------
print("# Answer")
if len(ties_exact) == 1:
    print(f"The best cicada cycle against predators {predator_periods} is **{best_exact} years**.")
else:
    print("The best cicada cycles are:", ", ".join(map(str, ties_exact)), "years.")

print("\n# Reason Why (Exact, union-overlap over a full horizon)")
print(f"Horizon H = lcm({predator_periods + candidates}) = {H} years.\n")
for C in candidates:
    r = exact_results[C]
    print(f"- Cicada {C:>2}y: {r['overlap_years']} overlap years out of {r['horizon_years']} "
          f"({r['overlap_rate']*100:.4f}%).")
print("Ranking (lower is better):", " < ".join(map(str, rank_exact)))

print("\n# Check (Deterministic: all predator phases enumerated)")
for C in candidates:
    m = check_results[C]
    print(
        f"- Cicada {C:>2}y: mean={m['mean_rate']*100:.4f}%  "
        f"stdev={m['stdev_rate']*100:.4f}%  "
        f"min={m['min_rate']*100:.4f}%  max={m['max_rate']*100:.4f}%  "
        f"(cases={m['cases']})"
    )
print("Check ranking (lower mean is better):", " < ".join(map(str, rank_check)))

# Exit nonzero if primes don't beat 12y in both exact and check rankings.
ok = (best_exact in (13, 17)) and (best_check in (13, 17))
if not ok:
    raise SystemExit(1)

