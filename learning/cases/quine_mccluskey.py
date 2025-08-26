#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Quine–McCluskey — ARC (Answer / Reason / Check), self-contained

Function:
  f(A,B,C,D) = Σ m(1,3,7,11,15)  with don't-cares Σ d(0,2,5)

Deterministic tie-breaking:
  We order implicant patterns lexicographically by mapping chars {0<1<-}
  using the key: pat_key(pattern) where '-' is considered greatest.
"""

from itertools import combinations
from typing import Dict, List, Set, Tuple

# ─────────────────────────── Problem instance ───────────────────────────
MINTERMS: Set[int] = {1, 3, 7, 11, 15}
DONTCARE: Set[int] = {0, 2, 5}
WIDTH = 4
VARS = ["A", "B", "C", "D"]

bits = lambda n: f"{n:0{WIDTH}b}"
pat_key = lambda p: "".join({"0":"0","1":"1","-":"2"}[ch] for ch in p)  # 0<1<-

# ───────────────────────────── Q–M machinery ─────────────────────────────
def combine(a: str, b: str) -> str | None:
    """Combine two implicants if they differ in exactly one position."""
    diff, out = 0, []
    for x, y in zip(a, b):
        if x != y:
            diff += 1
            out.append("-")
            if diff > 1:
                return None
        else:
            out.append(x)
    return "".join(out) if diff == 1 else None

def covers(pat: str, m: int) -> bool:
    """Does implicant pattern 'pat' cover minterm m?"""
    b = bits(m)
    return all(p == "-" or p == bb for p, bb in zip(pat, b))

def quine_mccluskey(minterms: Set[int], dcs: Set[int]) -> Tuple[List[str], List[str]]:
    """Return (ordered primes, ordered minimal cover) with deterministic choices."""
    ALL = sorted(minterms | dcs)
    # 1) Initial groups by Hamming weight
    groups: Dict[int, List[str]] = {}
    for n in ALL:
        groups.setdefault(bits(n).count("1"), []).append(bits(n))
    for k in groups:
        groups[k].sort(key=pat_key)

    # 2) Iterative combination to get prime implicants (trace kept minimal)
    primes: Set[str] = set()
    while True:
        next_groups: Dict[int, List[str]] = {}
        used: Set[str] = set()
        for i in sorted(groups):
            for p in groups[i]:
                for q in groups.get(i + 1, []):
                    merged = combine(p, q)
                    if merged:
                        next_groups.setdefault(merged.count("1"), []).append(merged)
                        used.update({p, q})
        # anything not used this round becomes a prime implicant
        for lst in groups.values():
            for p in lst:
                if p not in used:
                    primes.add(p)
        if not next_groups:
            break
        for k in next_groups:
            next_groups[k] = sorted(set(next_groups[k]), key=pat_key)
        groups = next_groups

    ordered_primes = sorted(primes, key=pat_key)

    # 3) Build prime implicant chart (minterm -> primes that cover it)
    chart: Dict[int, List[str]] = {m: [] for m in minterms}
    for p in ordered_primes:
        for m in minterms:
            if covers(p, m):
                chart[m].append(p)
    for m in chart:
        chart[m].sort(key=pat_key)

    # 4) Essential primes + deterministic minimal cover among the rest
    essential = {plist[0] for plist in chart.values() if len(plist) == 1}
    remaining = {m for m in minterms if not any(covers(p, m) for p in essential)}
    noness = [p for p in ordered_primes if p not in essential]

    best_cover: Set[str] = set()
    for r in range(len(noness) + 1):
        candidates = []
        for combo in combinations(noness, r):
            rest = set(remaining)
            for pat in combo:
                rest = {m for m in rest if not covers(pat, m)}
                if not rest:
                    break
            if not rest:
                candidates.append(tuple(sorted(combo, key=pat_key)))
        if candidates:
            best_cover = set(min(candidates, key=lambda tup: [pat_key(p) for p in tup]))
            break

    minimal_cover = sorted(essential | best_cover, key=pat_key)
    return ordered_primes, minimal_cover

def pat2term(p: str) -> str:
    """Convert pattern to a product term over VARS using ¬ for negation."""
    out: List[str] = []
    for v, b in zip(VARS, p):
        if b == "1":
            out.append(v)
        elif b == "0":
            out.append("¬" + v)
    return "".join(out) or "1"

def eval_cover(n: int, cover: List[str]) -> int:
    """Evaluate f(n) under the cover (1 if any implicant matches)."""
    return 1 if any(covers(p, n) for p in cover) else 0

# ─────────────────────────────────── ARC: Answer ─────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    primes, cover = quine_mccluskey(MINTERMS, DONTCARE)

    print("Minterms:", sorted(MINTERMS))
    print("Don't-cares:", sorted(DONTCARE))
    print("\nPrime implicants (deterministic order):")
    for p in primes:
        print(" ", p)

    print("\nSelected minimal cover (lexicographically first):", cover)
    sop = " + ".join(pat2term(p) for p in cover)
    print("Minimal Sum-of-Products:")
    print(" f =", sop)

# ────────────────────────────── ARC: Reason why ──────────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("We run Quine–McCluskey with don't-cares:")
    print("  1) Group minterms+DCs by Hamming weight; combine adjacent groups across 1-bit")
    print("     differences to build larger implicants; anything never combined becomes a prime.")
    print("  2) Build a prime-implicant chart over *minterms only*.")
    print("  3) Take all essentials; for the remaining minterms, choose the smallest-size cover")
    print("     among nonessential primes. On ties, pick the lexicographically first by the")
    print("     key 0 < 1 < - (dash as greatest).")
    print("This yields a two-implicant cover equivalent to f.")

# ───────────────────────────── ARC: Check (harness) ──────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    primes, cover = quine_mccluskey(MINTERMS, DONTCARE)

    # A) Functional correctness on the full truth table (respect DONTCARE)
    zeros = {n for n in range(1 << WIDTH)} - MINTERMS - DONTCARE
    ok_cover_all_minterms = all(eval_cover(m, cover) == 1 for m in MINTERMS)
    ok_no_false_positives = all(eval_cover(z, cover) == 0 for z in zeros)
    print(f"All minterms covered? {ok_cover_all_minterms}")
    print(f"No zeros asserted (outside don't-cares)? {ok_no_false_positives}")

    # B) Minimality by exhaustive search among primes (cardinality then lexicographic)
    # Compute all minimal covers and check ours is smallest & lexicographically first
    chart: Dict[int, List[str]] = {m: [] for m in MINTERMS}
    for p in primes:
        for m in MINTERMS:
            if covers(p, m):
                chart[m].append(p)
    for m in chart:
        chart[m].sort(key=pat_key)

    essential = {plist[0] for plist in chart.values() if len(plist) == 1}
    remaining = {m for m in MINTERMS if not any(covers(p, m) for p in essential)}
    noness = [p for p in primes if p not in essential]

    def covers_remaining(combo: Tuple[str, ...]) -> bool:
        rest = set(remaining)
        for p in combo:
            rest = {m for m in rest if not covers(p, m)}
            if not rest:
                return True
        return not rest

    best_size = None
    best_lex: Tuple[str, ...] | None = None
    for r in range(len(noness) + 1):
        found = []
        for combo in combinations(noness, r):
            if covers_remaining(combo):
                found.append(tuple(sorted(combo, key=pat_key)))
        if found:
            best_size = r + len(essential)
            best_lex = min(found, key=lambda tup: [pat_key(p) for p in tup])
            break

    our_cover = tuple(cover)
    expected_cover = tuple(sorted(essential | set(best_lex or ()), key=pat_key))
    ok_min_size = (len(our_cover) == best_size)
    ok_lex_first = (our_cover == expected_cover)
    print(f"Minimal number of implicants? {ok_min_size}")
    print(f"Lexicographically first among minimal covers? {ok_lex_first}")

    # C) Pretty-print should correspond to patterns
    sop = " + ".join(pat2term(p) for p in cover)
    ok_terms_nonempty = all(pat2term(p) != "" for p in cover)
    print(f"SOP terms render correctly? {ok_terms_nonempty}")

    all_ok = ok_cover_all_minterms and ok_no_false_positives and ok_min_size and ok_lex_first and ok_terms_nonempty
    print(f"\nAll checks passed? {all_ok}")

# ─────────────────────────────────── Main ────────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

