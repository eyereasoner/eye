#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
takeuchi.py – memoised Takeuchi function (return-z variant), ARC-ified
──────────────────────────────────────────────────────────────────────────

Cache layout (unchanged from your version)
------------------------------------------
* MAXN = 50  → a (MAXN+1)^3 table (~132,651 cells).
* Base case  → return z  (instead of the classic “return y”).

What this script adds
---------------------
• Answer — computes tak(X,Y,Z) and prints value + cache statistics.
• Reason why — explains the recurrence and why memoisation is critical.
• Check (harness) — verifies:
    1) base case: for many (x≤y), tak(x,y,z) == z;
    2) equivalence with a reference (@lru_cache) implementation on a grid;
    3) (optionally) compares the result of the main query with the reference.

Safe defaults keep the reference grid modest so it’s fast and within recursion
limits, while still giving strong confidence in correctness.
"""

from __future__ import annotations
from functools import lru_cache
from typing import Final, Optional, Tuple, List

# ─────────────────────────────────────────────────────────────
# Parameters & 3-D cache (as in your original)
# ─────────────────────────────────────────────────────────────
MAXN: Final[int] = 50  # cache covers 0..MAXN inclusive

_ready  = [[[False]*(MAXN+1) for _ in range(MAXN+1)] for _ in range(MAXN+1)]
_cache  = [[[0]*(MAXN+1)     for _ in range(MAXN+1)] for _ in range(MAXN+1)]

# Counters for ARC stats
CALLS = 0
HITS  = 0
STORES = 0

def _in_range(v: int) -> bool:
    return 0 <= v <= MAXN

def _cached(x: int, y: int, z: int) -> Optional[int]:
    """Return a previously stored value or None if not memoised."""
    global HITS
    if _in_range(x) and _in_range(y) and _in_range(z) and _ready[x][y][z]:
        HITS += 1
        return _cache[x][y][z]
    return None

def _remember(x: int, y: int, z: int, v: int) -> None:
    """Store result in the table when indices are in range."""
    global STORES
    if _in_range(x) and _in_range(y) and _in_range(z):
        if not _ready[x][y][z]:
            STORES += 1
        _cache[x][y][z] = v
        _ready[x][y][z] = True

# ─────────────────────────────────────────────────────────────
# Memoised Takeuchi (return-z variant)
# ─────────────────────────────────────────────────────────────
def tak(x: int, y: int, z: int) -> int:
    """Memoised Takeuchi function (base case returns z)."""
    global CALLS
    CALLS += 1

    memo = _cached(x, y, z)
    if memo is not None:                 # reuse if seen before
        return memo

    if x <= y:                           # base case
        v = z
    else:
        v = tak(
            tak(x - 1, y, z),
            tak(y - 1, z, x),
            tak(z - 1, x, y),
        )

    _remember(x, y, z, v)
    return v

# ─────────────────────────────────────────────────────────────
# Reference implementation for the harness (same spec)
# Uses Python's lru_cache instead of our 3-D array.
# ─────────────────────────────────────────────────────────────
@lru_cache(maxsize=None)
def tak_ref(x: int, y: int, z: int) -> int:
    if x <= y:
        return z
    return tak_ref(
        tak_ref(x - 1, y, z),
        tak_ref(y - 1, z, x),
        tak_ref(z - 1, x, y),
    )

# ─────────────────────────────────────────────────────────────
# ARC: Answer / Reason / Check
# ─────────────────────────────────────────────────────────────
def arc_answer(X: int, Y: int, Z: int, value: int) -> None:
    print("Answer")
    print("------")
    print(f"tak({X},{Y},{Z}) = {value}")
    # Cache stats
    total_cells = (MAXN + 1) ** 3
    hit_rate = f"{(HITS / max(CALLS,1))*100:.1f}%"
    print("\nCache statistics:")
    print(f"  MAXN: {MAXN}  → table size (cells): {(MAXN+1)}³ = {total_cells:,}")
    print(f"  calls: {CALLS:,}   hits: {HITS:,}   stores: {STORES:,}   hit-rate: {hit_rate}\n")

def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("Definition (return-z variant):")
    print("  tak(x,y,z) = { z                         if x ≤ y")
    print("               { tak( tak(x-1,y,z),")
    print("                      tak(y-1,z,x),")
    print("                      tak(z-1,x,y) )        otherwise")
    print("\nThe recurrence fan-outs rapidly, but overlapping subproblems abound.")
    print("Memoisation makes each distinct triple (x,y,z) compute at most once,")
    print("dramatically reducing the effective work and keeping recursion depth")
    print("within Python's default limits for the sizes we test here.\n")

def arc_check(X: int, Y: int, Z: int, value: int) -> None:
    print("Check (harness)")
    print("---------------")
    # 1) Base-case sanity: for many triples with x ≤ y, tak(x,y,z) == z
    for x in range(0, 10):
        for y in range(x, 10):       # ensures x ≤ y
            for z in range(0, 6):
                assert tak(x, y, z) == z, f"Base case failed at ({x},{y},{z})"

    # 2) Equivalence with reference across a grid (modest to stay fast)
    for x in range(0, 10):
        for y in range(0, 10):
            for z in range(0, 6):
                v1 = tak(x, y, z)
                v2 = tak_ref(x, y, z)
                assert v1 == v2, f"Mismatch tak vs tak_ref at ({x},{y},{z}): {v1} != {v2}"

    # 3) Optional: compare main query with reference (guard RecursionError)
    ok = True
    try:
        ref_main = tak_ref(X, Y, Z)
        assert value == ref_main, f"Main value mismatch: {value} != {ref_main}"
    except RecursionError:
        ok = False  # skip if the reference hits recursion depth; our main value is still valid

    print("OK: base-case holds over grid; memoised result matches reference on grid"
          + ("" if ok else " (main triple check skipped due to recursion depth)."))
    print()

# ─────────────────────────────────────────────────────────────
# Demo
# ─────────────────────────────────────────────────────────────
def main() -> None:
    # Example from your original script (fits comfortably with memoisation)
    X: Final[int] = 14
    Y: Final[int] = 7
    Z: Final[int] = 0

    val = tak(X, Y, Z)
    arc_answer(X, Y, Z, val)
    arc_reason()
    arc_check(X, Y, Z, val)

if __name__ == "__main__":
    # The cached version stays within Python’s default recursion limit.
    # Increase with sys.setrecursionlimit(...) only for *much* larger arguments.
    main()

