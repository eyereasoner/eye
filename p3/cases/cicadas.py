#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Prompt → Program → Proof  (Deterministic, Self-Contained)

PROMPT
------
Mimic the reasoning in the Dutch passage: some cicada families have *prime-number*
life cycles (e.g., 13 or 17 years). This grants them an evolutionary advantage
because they coincide less often with periodically emerging predators that have
shorter cycles (2, 3, 4, 6 years). Show—by calculation and an independent check—
that prime cycles collide less than a composite 12-year cycle.

PROGRAM (What this file does)
-----------------------------
1) Defines utility math (gcd/lcm) and a function that counts *union overlaps*:
   in a finite horizon, how many distinct years do cicadas and at least one
   predator emerge together?

2) EXACT REASONING:
   - Uses a comparison horizon H equal to the least common multiple (LCM) of
     all predator periods and candidate cicada periods. Over [1..H], periodic
     processes repeat exactly, so counts are exact and phase-independent for
     aligned series.
   - Computes overlap rate for candidates {12, 13, 17} with predators {2,3,4,6}
     assuming zero phase offset for predators. This corresponds to the simple,
     illustrative case often used in explanations.

3) INDEPENDENT CHECK (Deterministic):
   - Enumerates *all* predator phase offsets (product of ranges 0..P-1), i.e.
     2×3×4×6 = 144 phase combinations—no randomness.
   - For each combination, computes the union overlap rate and reports mean,
     stdev, min, and max. This verifies the conclusion independent of any
     single alignment.

PROOF (What counts as evidence)
-------------------------------
- The program prints:
  # Answer       → best cycle(s) by exact overlap rate.
  # Reason Why   → exact counts and rates over the full horizon H.
  # Check        → exhaustive phase analysis statistics confirming the ranking.

KEY DEFINITIONS
---------------
- Cicada cycle C: years {k*C | k ≥ 1}.
- Predator with period P and phase offset o: years {o + k*P | k ≥ 0}.
- Union overlap: a year in which the cicada emergence coincides with *any*
  predator’s emergence (we count each year once, even if multiple predators
  coincide that same year).

ASSUMPTIONS (kept minimal)
--------------------------
- Predators’ cycles are strictly periodic with fixed integer years.
- Emergence happens at integer year boundaries (consistent with period counts).
- We compare rates over an exact repeating horizon H = lcm(predators ∪ candidates).

WHY “PRIME” HELPS (intuition)
-----------------------------
If C is prime and larger than all predator periods, then for any single predator
period P, overlaps occur every lcm(C,P) = C*P years; relative to the cicada’s
every-C emergence, that yields 1/P overlap density per predator. Composite C can
share factors with predators (e.g., C=12 shares factors with 2,3,4,6), causing
far more frequent coincidences.

DETERMINISM & REPRODUCIBILITY
-----------------------------
- No randomness is used.
- The CHECK exhaustively enumerates all predator phases.
- Output is identical across runs on any Python 3 interpreter.

COMPLEXITY
----------
- Exact step: O(H / C) set work per candidate; overall feasible because H is the
  LCM of small integers (here 2652).
- Check step: O( (#phase-combos) × (#cicada-emergences within H) ).

HOW TO RUN
----------
$ python3 cicadas.py
(prints the P3 triad)

LIMITATIONS / SCOPE
-------------------
- This is a simple period/phase model; real ecosystems involve variable survival,
  spatial heterogeneity, and non-integer timing. The program is intended only to
  demonstrate the *number-theoretic* advantage described in the passage.
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

def union_overlap_count(cicada_cycle, predator_periods, horizon, offsets):
    """
    Count distinct overlap years within [1..horizon] where the cicada emerges
    and at least one predator emerges in the same year, given fixed phase offsets.

    Parameters
    ----------
    cicada_cycle : int
        Cicada life-cycle length in years (C).
    predator_periods : list[int]
        Periods P_i for each predator group.
    horizon : int
        Inclusive upper bound for years considered (start is year 1).
    offsets : iterable[int]
        Phases o_i for predators, interpreted modulo P_i.

    Returns
    -------
    int : number of distinct overlap years within [1..horizon].
    """
    cicada_years = set(range(cicada_cycle, horizon + 1, cicada_cycle))
    overlap_years = set()
    for P, o in zip(predator_periods, offsets):
        o %= P  # normalize phase into [0, P-1]
        # first year >= 1 in this arithmetic progression
        y = o if o >= 1 else o + ((1 - o + P - 1) // P) * P
        while y <= horizon:
            if y in cicada_years:
                overlap_years.add(y)
            y += P
    return len(overlap_years)

# ---------- Inputs ----------
predator_periods = [2, 3, 4, 6]
candidates = [12, 13, 17]  # composite vs. primes highlighted in the passage

# Exact horizon for fair comparison (periods repeat exactly over H)
H = lcm_many(predator_periods + candidates)

# ---------- Exact calculation (Reason Why) ----------
exact_results = {}
for C in candidates:
    # aligned predators (offset 0) for the core illustrative statement
    count0 = union_overlap_count(C, predator_periods, H, offsets=[0]*len(predator_periods))
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
# Enumerate all predator phase combinations: offsets in [0..P-1] for each P (2*3*4*6 = 144).
all_offsets = list(product(*[range(P) for P in predator_periods]))

def exhaustive_stats(C):
    """Return mean/stdev/min/max overlap rates over all predator phases (deterministic)."""
    rates = []
    for offs in all_offsets:
        count = union_overlap_count(C, predator_periods, H, offsets=offs)
        rates.append(count / H)
    n = len(rates)
    mean_rate = sum(rates) / n
    mean_sq = sum(r*r for r in rates) / n
    stdev = (mean_sq - mean_rate*mean_rate) ** 0.5  # population stdev
    return {
        "mean_rate": mean_rate,
        "stdev_rate": stdev,
        "min_rate": min(rates),
        "max_rate": max(rates),
        "cases": n
    }

check_results = {C: exhaustive_stats(C) for C in candidates}
rank_check = sorted(candidates, key=lambda c: check_results[c]["mean_rate"])
best_check = rank_check[0]
ties_check = [c for c in candidates
              if abs(check_results[c]["mean_rate"] - check_results[best_check]["mean_rate"]) < 1e-12]

# ---------- Emit the P3 triad ----------
print("# Answer")
if len(ties_exact) == 1:
    print(f"The best life cycle against predators (2, 3, 4, 6 years) is **{best_exact} years**.")
else:
    print("The best life cycles are:", ", ".join(map(str, ties_exact)), "years.")

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

