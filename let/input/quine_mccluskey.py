#!/usr/bin/env python3
"""
quine_mccluskey.py  –  Quine-McCluskey with *deterministic* backward proof

Function
    f(A,B,C,D) = Σ m(1,3,7,11,15) + Σ d(0,2,5)

Chosen minimal cover (lexicographically first):
    {'00--', '--11'}   →   f = CD  +  ¬A¬B
"""

from itertools import combinations
from typing import Dict, List, Set

# ─── Data sets ───────────────────────────────────────────────
MINTERMS = {1, 3, 7, 11, 15}
DONTCARE = {0, 2, 5}
ALL      = sorted(MINTERMS | DONTCARE)
WIDTH    = 4
VARS     = ['A','B','C','D']

bits = lambda n: f"{n:0{WIDTH}b}"
pat_key = lambda p: ''.join({'0':'0','1':'1','-':'2'}[ch] for ch in p)

# ─── Helper functions ────────────────────────────────────────
def combine(a: str, b: str) -> str|None:
    diff, out = 0, []
    for x, y in zip(a, b):
        if x != y:
            diff += 1
            out.append('-')
        else:
            out.append(x)
        if diff > 1: return None
    return ''.join(out) if diff == 1 else None

covers = lambda pat,m: all(p == '-' or p == b for p,b in zip(pat,bits(m)))

# ─── 1. Initial grouping ─────────────────────────────────────
groups: Dict[int, List[str]] = {}
for n in ALL:
    groups.setdefault(bits(n).count('1'), []).append(bits(n))
for k in groups: groups[k].sort(key=pat_key)

print("\nInitial groups:")
for k in sorted(groups):
    print(f"  {k}: {groups[k]}")

# ─── 2. Iterative combination (proof trace) ──────────────────
primes: Set[str] = set()
iteration = 0
while True:
    print(f"\n=== Iteration {iteration} ===")
    next_groups: Dict[int, List[str]] = {}
    used = set()

    for i in sorted(groups):
        for p in groups[i]:
            for q in groups.get(i+1, []):
                merged = combine(p, q)
                if merged:
                    print(f"  combine {p} + {q}  →  {merged}")
                    next_groups.setdefault(merged.count('1'), []).append(merged)
                    used.update({p, q})

    primes.update(p for lst in groups.values() for p in lst if p not in used)
    if not next_groups:
        break
    for k in next_groups:
        next_groups[k] = sorted(set(next_groups[k]), key=pat_key)
    groups = next_groups
    iteration += 1

print("\nPrime implicants:")
for p in sorted(primes, key=pat_key):
    print(" ", p)

# ─── 3. Prime-implicant chart ────────────────────────────────
chart = {m: [] for m in MINTERMS}
for p in primes:
    for m in MINTERMS:
        if covers(p, m):
            chart[m].append(p)
for m in chart: chart[m].sort(key=pat_key)

print("\nChart (minterm → covering primes)")
for m in sorted(chart):
    print(f"  {m:>2}: {chart[m]}")

# ─── 4. Essentials & deterministic minimal cover ─────────────
essential = {plist[0] for plist in chart.values() if len(plist) == 1}
print("\nEssential primes:", essential)

remaining = {m for m in MINTERMS if not any(covers(p, m) for p in essential)}
if remaining:
    print("Remaining minterms to cover:", remaining)
else:
    print("All minterms covered by essentials.")

noness = sorted([p for p in primes if p not in essential], key=pat_key)

best_cover: Set[str] = set()
for r in range(len(noness)+1):
    candidates = []
    for combo in combinations(noness, r):
        rest = set(remaining)
        for pat in combo:
            rest = {m for m in rest if not covers(pat, m)}
        if not rest:
            candidates.append(tuple(sorted(combo, key=pat_key)))
    if candidates:
        best_cover = set(min(candidates, key=lambda tup:[pat_key(p) for p in tup]))
        break

minimal_cover = essential | best_cover
ordered_cover = sorted(minimal_cover, key=pat_key)
print("\nSelected minimal cover:", ordered_cover)

# ─── 5. SOP expression ───────────────────────────────────────
def pat2term(p: str) -> str:
    out = []
    for v, b in zip(VARS, p):
        if b == '1':  out.append(v)
        elif b == '0':out.append('¬'+v)
    return ''.join(out) or '1'

sop = '  +  '.join(pat2term(p) for p in ordered_cover)
print("\nMinimal Sum-of-Products:")
print("  f =", sop)

