#!/usr/bin/env python3
"""
qm_backward.py  –  Quine-McCluskey with backward-proof trace

Target Boolean function (4 variables A,B,C,D):
    f = Σ m(1, 3, 7, 11, 15)  +  Σ d(0, 2, 5)

Expected minimal SOP:
    f =  C·D  +  ¬A¬B
"""

from itertools import combinations
from typing import Dict, List, Set, Tuple

# ─────────────────────────────────────────────────────────────
# 0. Input sets
# ─────────────────────────────────────────────────────────────
MINTERMS = {1, 3, 7, 11, 15}
DONTCARE = {0, 2, 5}
ALL      = MINTERMS | DONTCARE             # combine for algorithm
WIDTH    = 4                               # four variables

# ─────────────────────────────────────────────────────────────
# 1. Helper functions
# ─────────────────────────────────────────────────────────────
def bits(n: int) -> str:
    """Return n as WIDTH-bit binary string."""
    return f"{n:0{WIDTH}b}"

def combine(a: str, b: str) -> str | None:
    """Return merged pattern with exactly ONE differing bit → '-', else None."""
    diff, out = 0, []
    for x, y in zip(a, b):
        if x != y:
            diff += 1
            out.append('-')
        else:
            out.append(x)
        if diff > 1:
            return None
    return ''.join(out) if diff == 1 else None

def covers(pat: str, m: int) -> bool:
    """Does implicant pattern cover minterm m ?"""
    return all(p == '-' or p == b for p, b in zip(pat, bits(m)))

# ─────────────────────────────────────────────────────────────
# 2. Initial grouping by #ones – proof start
# ─────────────────────────────────────────────────────────────
groups: Dict[int, List[str]] = {}
for num in sorted(ALL):
    groups.setdefault(bits(num).count('1'), []).append(bits(num))

print("\nInitial groups:")
for ones in sorted(groups):
    print(f"  {ones} ones: {groups[ones]}")

# ─────────────────────────────────────────────────────────────
# 3. Iterative combination (prints every step)
# ─────────────────────────────────────────────────────────────
primes: Set[str] = set()
iteration = 0
while True:
    print(f"\n=== Iteration {iteration} ===")
    next_groups: Dict[int, List[str]] = {}
    used: Set[str] = set()

    # Compare group i with i+1
    for i in sorted(groups):
        for p in groups[i]:
            for q in groups.get(i + 1, []):
                merged = combine(p, q)
                if merged:
                    print(f"  combine {p} + {q}  →  {merged}")
                    next_groups.setdefault(merged.count('1'), []).append(merged)
                    used |= {p, q}

    # Patterns not used become primes
    for lst in groups.values():
        primes |= {pat for pat in lst if pat not in used}

    if not next_groups:           # no further combination possible
        break

    # Deduplicate in next iteration
    for k in next_groups:
        next_groups[k] = sorted(set(next_groups[k]))

    groups = next_groups
    iteration += 1

print("\nPrime implicants found:")
for p in sorted(primes):
    print(" ", p)

# ─────────────────────────────────────────────────────────────
# 4. Prime-implicant chart (backward justification)
# ─────────────────────────────────────────────────────────────
chart: Dict[int, List[str]] = {m: [] for m in MINTERMS}
for p in primes:
    for m in MINTERMS:
        if covers(p, m):
            chart[m].append(p)

print("\nPrime-implicant chart  (minterm → covering primes)")
for m in sorted(chart):
    print(f"  {m:>2}: {chart[m]}")

# ─────────────────────────────────────────────────────────────
# 5. Essential primes + minimal cover (tiny brute force)
# ─────────────────────────────────────────────────────────────
essential: Set[str] = set()
remaining = set(MINTERMS)

# (a) Essentials: minterms covered by exactly one prime
for m, plist in chart.items():
    if len(plist) == 1:
        essential.add(plist[0])
for p in essential:
    remaining -= {m for m in remaining if covers(p, m)}

# (b) Choose among non-essential primes to cover the rest
noness = [p for p in primes if p not in essential]
minimal_cover = set(essential)

if remaining:
    for r in range(1, len(noness) + 1):
        for combo in combinations(noness, r):
            covered = set()
            for pat in combo:
                covered |= {m for m in remaining if covers(pat, m)}
            if covered == remaining:
                minimal_cover |= set(combo)     # ← fixed line
                remaining = set()
                break
        if not remaining:
            break

print("\nSelected minimal cover:", minimal_cover)

# ─────────────────────────────────────────────────────────────
# 6. Convert patterns to human-readable literals
# ─────────────────────────────────────────────────────────────
vars = ['A', 'B', 'C', 'D']
def pat_to_product(pat: str) -> str:
    term = []
    for v, bit in zip(vars, pat):
        if bit == '1':
            term.append(v)
        elif bit == '0':
            term.append('¬' + v)
    return ''.join(term) if term else '1'

expression = '  +  '.join(pat_to_product(p) for p in sorted(minimal_cover))
print("\nMinimal Sum-of-Products:")
print("  f =", expression)

