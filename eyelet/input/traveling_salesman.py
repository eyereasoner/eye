"""
traveling_salesman.py
=====================

Two reference implementations of the Traveling Salesman Problem (TSP):

* brute_force_tsp  – factorial-time exhaustive search
* held_karp_tsp    – O(n² · 2ⁿ) dynamic programming (Held–Karp)

Run the file directly to see both solvers in action on a tiny data set.
"""

from __future__ import annotations
import itertools
import math
from functools import lru_cache
from typing import List, Tuple


# --------------------------------------------------------------------------- #
#  helpers                                                                    #
# --------------------------------------------------------------------------- #

Point = Tuple[float, float]          # 2-D coordinate
Tour  = Tuple[int, ...]              # city indices in visiting order


def distance(a: Point, b: Point) -> float:
    """Euclidean distance between two points."""
    return math.hypot(a[0] - b[0], a[1] - b[1])


# --------------------------------------------------------------------------- #
#  Algorithm 1 – Brute force                                                  #
# --------------------------------------------------------------------------- #

def brute_force_tsp(coords: List[Point], start: int = 0) -> Tuple[Tour, float]:
    """
    Exhaustive TSP search.

    Parameters
    ----------
    coords : list[Point]
        Coordinates of every city (index == city id).
    start : int, default 0
        Starting city (also the final city because the tour is cyclic).

    Returns
    -------
    (tour, length) : (Tuple[int], float)
        `tour`  – order of cities, starting and ending with `start`.
        `length` – total Euclidean length of the tour.
    """
    n = len(coords)
    best_order, best_len = None, math.inf

    for perm in itertools.permutations(range(1, n)):     # keep start fixed
        order: Tour = (start, ) + perm + (start, )
        total = sum(distance(coords[order[i]], coords[order[i + 1]])
                    for i in range(n))
        if total < best_len:
            best_order, best_len = order, total

    return best_order, best_len


# --------------------------------------------------------------------------- #
#  Algorithm 2 – Held–Karp dynamic programming                                #
# --------------------------------------------------------------------------- #

def held_karp_tsp(coords: List[Point], start: int = 0) -> Tuple[Tour, float]:
    """
    Exact TSP via Held–Karp dynamic programming.

    O(n² · 2ⁿ) time and O(n · 2ⁿ) memory – practical up to ~22–24 cities on
    a modern laptop.

    Returns the same (tour, length) tuple as `brute_force_tsp`.
    """
    n = len(coords)

    @lru_cache(maxsize=None)
    def dp(mask: int, last: int) -> Tuple[float, Tour]:
        """
        Minimum length to reach subset `mask` and stop at `last`.

        `mask` is an n-bit bitmask where bit i == 1 means city i is included.
        """
        if mask == (1 << n) - 1:                         # all cities visited
            return distance(coords[last], coords[start]), (start,)

        best_len, best_path = math.inf, None
        for nxt in range(n):
            if mask & (1 << nxt):                        # already visited?
                continue
            cand_len, cand_path = dp(mask | (1 << nxt), nxt)
            cand_len += distance(coords[last], coords[nxt])
            if cand_len < best_len:
                best_len, best_path = cand_len, (nxt,) + cand_path
        return best_len, best_path

    length, suffix = dp(1 << start, start)
    return (start,) + suffix, length


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
    print("Cost :", round(length, 3))

    print("\n---- Held–Karp ----")
    tour, length = held_karp_tsp(cities)
    print("Tour :", tour)
    print("Cost :", round(length, 3))

