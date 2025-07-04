"""
traveling_salesman.py
=====================

• brute_force_tsp – factorial-time exhaustive search
• held_karp_tsp   – O(n²·2ⁿ) Held–Karp dynamic programming

Deterministic across machines:

  – uses exact squared distances for comparison
  – explicit tie-breaker (length, then lexicographic order)
  – canonical orientation (tour vs. its reverse)
"""

from __future__ import annotations

import itertools
import math
from functools import lru_cache
from typing import List, Tuple

# --------------------------------------------------------------------------- #
#  type aliases                                                               #
# --------------------------------------------------------------------------- #

Point = Tuple[float, float]   # 2-D coordinate
Tour  = Tuple[int, ...]       # city indices in visiting order


# --------------------------------------------------------------------------- #
#  numeric helpers                                                            #
# --------------------------------------------------------------------------- #

def dist2(a: Point, b: Point) -> float:
    """Squared Euclidean distance (exact for integer coordinates)."""
    dx = a[0] - b[0]
    dy = a[1] - b[1]
    return dx * dx + dy * dy


def path_length(tour: Tour, coords: List[Point]) -> float:
    """True Euclidean length of an entire tour."""
    return sum(
        math.hypot(coords[a][0] - coords[b][0], coords[a][1] - coords[b][1])
        for a, b in zip(tour, tour[1:])
    )


EPS = 1e-12


def strictly_better(cand_len: float, cand_tour: Tour,
                    best_len: float | None, best_tour: Tour | None) -> bool:
    """
    Return True if the candidate should replace the incumbent.

    Primary key: shorter length.
    Secondary key: lexicographically smaller tour.
    """
    if best_len is None or best_tour is None:
        return True
    if cand_len + EPS < best_len:
        return True
    if abs(cand_len - best_len) <= EPS:
        return cand_tour < best_tour
    return False


def canonicalise(tour: Tour) -> Tour:
    """
    Return the lexicographically smaller of `tour`
    and its exact reverse.  Assumes tour[0] == tour[-1].
    """
    rev = tour[::-1]
    return tour if tour < rev else rev


# --------------------------------------------------------------------------- #
#  Algorithm 1 – Brute force                                                  #
# --------------------------------------------------------------------------- #

def brute_force_tsp(coords: List[Point], start: int = 0) -> Tuple[Tour, float]:
    """
    Exact TSP via exhaustive permutation search.
    """
    n = len(coords)
    best_order, best_len2 = None, None

    for perm in itertools.permutations(range(1, n)):     # keep `start` fixed
        order: Tour = (start,) + perm + (start,)
        total2 = sum(dist2(coords[order[i]], coords[order[i + 1]])
                     for i in range(n))                  # n edges

        if strictly_better(total2, order, best_len2, best_order):
            best_order, best_len2 = order, total2

    best_order = canonicalise(best_order)
    return best_order, path_length(best_order, coords)


# --------------------------------------------------------------------------- #
#  Algorithm 2 – Held–Karp dynamic programming                                #
# --------------------------------------------------------------------------- #

def held_karp_tsp(coords: List[Point], start: int = 0) -> Tuple[Tour, float]:
    """
    Exact TSP via Held–Karp DP (O(n²·2ⁿ) time, O(n·2ⁿ) memory).
    """
    n = len(coords)

    @lru_cache(maxsize=None)
    def dp(mask: int, last: int) -> Tuple[float, Tour]:
        """
        Minimum *squared* length to reach subset `mask` and stop at `last`.
        """
        if mask == (1 << n) - 1:                         # all cities visited
            return dist2(coords[last], coords[start]), (start,)

        best_len2, best_path = None, None
        for nxt in range(n):
            if mask & (1 << nxt):                        # already visited?
                continue
            cand_len2, cand_path = dp(mask | (1 << nxt), nxt)
            cand_len2 += dist2(coords[last], coords[nxt])

            if strictly_better(cand_len2, (nxt,) + cand_path,
                               best_len2, best_path):
                best_len2, best_path = cand_len2, (nxt,) + cand_path

        return best_len2, best_path

    len2, suffix = dp(1 << start, start)
    tour = (start,) + suffix
    tour = canonicalise(tour)
    return tour, path_length(tour, coords)


# --------------------------------------------------------------------------- #
#  Tiny demo                                                                  #
# --------------------------------------------------------------------------- #

if __name__ == "__main__":
    cities = [
        (0, 0),   # 0
        (1, 5),   # 1
        (5, 2),   # 2
        (6, 6),   # 3
        (8, 3)    # 4
    ]

    print("---- Brute force ----")
    tour, length = brute_force_tsp(cities)
    print("Tour :", tour)
    print("Cost :", round(length, 3))        # → 22.351

    print("\n---- Held–Karp ----")
    tour, length = held_karp_tsp(cities)
    print("Tour :", tour)
    print("Cost :", round(length, 3))        # → 22.351

