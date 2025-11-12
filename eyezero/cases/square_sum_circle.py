#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Self-contained, self-checking script (puzzle domain)
Case: Square-Sum Circle for numbers 1..N (default N=32)

Goal
----
Place 1..N around a circle so every adjacent pair sums to a perfect square.
Use each number exactly once; last and first are adjacent.

Squares considered
------------------
All perfect squares s with 4 <= s <= 2*N (max sum is (N-1)+N = 2N-1; include 2N if square).

Contract (P3 style)
-----------------------------
- Answer: Either a valid circular ordering (certificate) or a short impossibility witness.
- Reason Why: Transparent trace (squares used, degrees, connectivity, constructive search).
- Check (harness): Independent validations that must pass (or confirm witness).

Notes
-----
- No external packages. Pure Python 3 standard library only.
- Default strategy is tuned for speed: for even N (e.g., 32) it uses a **fixed-start fast solver**
  that finds a certificate in well under a second on typical machines.
- For harder odd N (e.g., 31), it uses a **bounded robust solver** (step budget) to avoid hanging.
"""

import sys
import argparse
from typing import Dict, List, Set, Tuple, Optional
from collections import deque

# ---------- Tiny helpers

def perfect_squares_up_to(m: int) -> Set[int]:
    """All squares >= 4 and <= m."""
    out = set()
    k = 2
    while k * k <= m:
        out.add(k * k)
        k += 1
    return out

def is_square(x: int) -> bool:
    r = int(x ** 0.5)
    return r * r == x

def print_header(title: str) -> None:
    print("=" * 72)
    print(title)
    print("=" * 72)

# ---------- Graph construction

def build_edges(N: int, squares: Set[int]) -> Dict[int, List[int]]:
    """
    Undirected adjacency for 1..N:
    edge a–b iff a+b ∈ squares; no self-loops.
    """
    edges: Dict[int, List[int]] = {n: [] for n in range(1, N + 1)}
    for a in range(1, N + 1):
        for s in squares:
            b = s - a
            if 1 <= b <= N and b != a:
                edges[a].append(b)
    # sort + dedupe for determinism
    for v in edges:
        edges[v] = sorted(set(edges[v]))
    return edges

def is_connected(edges: Dict[int, List[int]]) -> bool:
    """BFS over the undirected graph to ensure everything is reachable."""
    if not edges:
        return False
    start = next(iter(edges))
    seen = {start}
    q = deque([start])
    while q:
        v = q.popleft()
        for w in edges[v]:
            if w not in seen:
                seen.add(w)
                q.append(w)
    return len(seen) == len(edges)

# ---------- Step counter (for reporting & bounding)

class StepCounter:
    __slots__ = ("limit", "steps")
    def __init__(self, limit: Optional[int] = None):
        self.limit = None if limit is None else int(limit)
        self.steps = 0
    def tick(self, k: int = 1) -> bool:
        self.steps += k
        return (self.limit is None) or (self.steps <= self.limit)

# ---------- FAST solver (fixed start, single-end growth) — great for even N like 32

def fast_solver_fixed_start(edges: Dict[int, List[int]],
                            start: int = 1,
                            counter: Optional[StepCounter] = None) -> Optional[List[int]]:
    """
    Hamiltonian CYCLE search tuned for speed:
      - Fix start=1 (breaks rotational symmetry & avoids trying many starts).
      - Single-end extension (grow from 'last').
      - SOUND prune: no *unused* vertex may be completely isolated relative to
        (UNUSED ∪ {start,last}). (Requiring ≥2 is UNSAFE mid-search.)
      - MRV ordering: choose next neighbor with fewest *unused* neighbors.
    This is the fast path that finds N=32 in sub-second on typical machines.
    """
    n = len(edges)
    if start not in edges:
        return None
    used = {start}
    path: List[int] = [start]
    deg = {v: len(edges[v]) for v in edges}

    def has_zero_available(last: int) -> bool:
        endpoints = {start, last}
        for v in edges:
            if v in used:
                continue
            if not any((w not in used) or (w in endpoints) for w in edges[v]):
                return True
        return False

    def dfs(last: int) -> bool:
        if counter and not counter.tick():
            return False
        if has_zero_available(last):
            return False
        if len(path) == n:
            return start in edges[last]  # must close the cycle

        # Candidates from 'last'
        cand = [w for w in edges[last] if w not in used]
        if not cand:
            return False

        # MRV: fewest unused-neighbor count first, then static degree, then value
        cand.sort(key=lambda w: (sum(1 for x in edges[w] if x not in used), deg[w], w))
        for nxt in cand:
            used.add(nxt)
            path.append(nxt)
            if dfs(nxt):
                return True
            path.pop()
            used.remove(nxt)
        return False

    if dfs(start):
        return path
    return None

# ---------- ROBUST solver (two-ends, bounded search) — avoids hanging on hard odd N

def robust_solver_two_ends(edges: Dict[int, List[int]], counter: StepCounter) -> Optional[List[int]]:
    """
    Two-end path growth with safe prunes and a step budget:
      - Maintain a path; extend either end.
      - Forbid early end-to-end closure (short cycles) until final step.
      - SOUND prune: no *unused* vertex may be completely isolated relative to (UNUSED ∪ ends).
      - Forced-move propagation: if an end has exactly one candidate, take it greedily.
      - MRV ordering when branching; try both candidate orders to escape traps.
    """
    N = len(edges)
    deg = {v: len(edges[v]) for v in edges}
    start = min(edges.keys(), key=lambda v: (deg[v], v))
    used = {start}
    path: List[int] = [start]

    def available(end: int, other_end: int) -> List[int]:
        cand = [w for w in edges[end] if w not in used]
        if len(used) < N - 1:
            cand = [w for w in cand if w != other_end]  # avoid closing short cycle early
        return cand

    def zero_isolation() -> bool:
        ends = {path[0], path[-1]}
        for v in edges:
            if v in used:
                continue
            if not any((w not in used) or (w in ends) for w in edges[v]):
                return True
        return False

    def forced_extend() -> bool:
        while True:
            changed = False
            left, right = path[0], path[-1]
            left_c = available(left, right)
            right_c = available(right, left)

            # dead end on both sides
            if len(left_c) == 0 and len(right_c) == 0:
                return False

            if len(left_c) == 1:
                nxt = left_c[0]
                path.insert(0, nxt)
                used.add(nxt)
                changed = True

            left, right = path[0], path[-1]
            left_c = available(left, right)
            right_c = available(right, left)

            if len(right_c) == 1:
                nxt = right_c[0]
                path.append(nxt)
                used.add(nxt)
                changed = True

            if not changed:
                return True

    def dfs() -> bool:
        if not counter.tick():
            return False
        if zero_isolation():
            return False
        if not forced_extend():
            return False
        if len(path) == N:
            return path[0] in edges[path[-1]]

        left, right = path[0], path[-1]
        left_c = available(left, right)
        right_c = available(right, left)

        # choose end with fewer candidates
        if not left_c and not right_c:
            return False
        side = "left" if (not right_c or (left_c and len(left_c) <= len(right_c))) else "right"
        cands = left_c if side == "left" else right_c

        # MRV & robustness
        cands.sort(key=lambda w: (sum(1 for x in edges[w] if x not in used), deg[w], w))
        for order in (cands, list(reversed(cands))):
            for nxt in order:
                if side == "left":
                    path.insert(0, nxt)
                else:
                    path.append(nxt)
                used.add(nxt)
                if dfs():
                    return True
                used.remove(nxt)
                if side == "left":
                    path.pop(0)
                else:
                    path.pop()
        return False

    if dfs():
        return path
    return None

# ---------- Orchestration

def find_square_sum_cycle(N: int,
                          edges: Dict[int, List[int]],
                          strategy: str,
                          limit: int) -> Tuple[Optional[List[int]], int, str]:
    """
    Returns (cycle or None, steps_used, strategy_used).
    strategy ∈ {"auto","fast","robust"}.
    """
    if strategy not in {"auto","fast","robust"}:
        strategy = "auto"

    # AUTO: fast for even N, robust for odd N
    if strategy == "auto":
        strategy = "fast" if N % 2 == 0 else "robust"

    if strategy == "fast":
        counter = StepCounter(None)  # unbounded (still counted)
        res = fast_solver_fixed_start(edges, start=1, counter=counter)
        return res, counter.steps, "fast"
    else:
        counter = StepCounter(limit)
        res = robust_solver_two_ends(edges, counter)
        return res, counter.steps, "robust"

# ---------- Reason/Answer/Check builders

def build_reason(N: int, squares: Set[int], edges: Dict[int, List[int]],
                 cycle: Optional[List[int]] = None, witness: Optional[str] = None,
                 steps: Optional[int] = None, strategy: Optional[str] = None) -> str:
    lines: List[str] = []
    lines.append(f"Goal: Place 1–{N} in a circle so adjacent sums are perfect squares.")
    lines.append("")
    lines.append(f"Squares used: {sorted(squares)}")
    lines.append("")
    degs = {v: len(edges[v]) for v in sorted(edges)}
    lines.append("Degree by vertex (v:deg):")
    lines.append("  " + ", ".join(f"{v}:{degs[v]}" for v in sorted(degs)))
    lines.append("")
    if cycle is not None:
        lines.append("Found cycle (each edge annotated by its square sum):")
        pretty = []
        for i, a in enumerate(cycle):
            b = cycle[(i + 1) % len(cycle)]
            pretty.append(f"{a}–{b}={a+b}")
        lines.append("  " + " | ".join(pretty))
        lines.append("")
        lines.append("Each sum above is one of the permitted squares; last wraps to first.")
        if steps is not None and strategy:
            lines.append(f"(Constructed via {strategy} search; steps taken: {steps}.)")
    elif witness:
        lines.append("Impossibility witness:")
        lines.append(f"  {witness}")
    else:
        lines.append("No cycle found and no structural impossibility detected (inconclusive).")
        if steps is not None:
            lines.append(f"(Search steps explored: {steps}.)")
    return "\n".join(lines)

def run_checks(N: int, squares: Set[int], edges: Dict[int, List[int]],
               cycle: Optional[List[int]] = None, witness: Optional[str] = None) -> List[str]:
    results: List[str] = []
    assert all(4 <= s <= 2 * N for s in squares), "Squares out of expected range"
    results.append("✓ Squares set is within [4, 2N].")

    if cycle is not None:
        assert len(cycle) == N, "Cycle length is not N"
        assert len(set(cycle)) == N, "Cycle does not use each number exactly once"
        for i, a in enumerate(cycle):
            b = cycle[(i + 1) % N]
            s = a + b
            assert is_square(s) and s in squares, f"Non-square adjacent sum {a}+{b}={s}"
            assert b in edges[a] and a in edges[b], f"Missing edge {a}–{b}"
        results.append("✓ Certificate valid: every adjacent pair sums to a permitted square (including wrap-around).")
        assert is_connected(edges), "Underlying graph should be connected for a cycle to exist"
        results.append("✓ Underlying square-sum graph is connected.")
    else:
        assert witness is not None, "Impossibility path requires a witness"
        if "degree < 2" in witness:
            bad = [v for v in edges if len(edges[v]) < 2]
            assert bad, "Witness claims degree<2 but none found"
            results.append("✓ Witness confirmed: at least one vertex has degree < 2 → impossible.")
        elif "disconnected graph" in witness:
            assert not is_connected(edges), "Witness claims disconnection but graph is connected"
            results.append("✓ Witness confirmed: graph is disconnected → impossible.")
        else:
            results.append("✓ Recorded: search-based witness is non-contradictory (not a proof).")
    return results

# ---------- CLI + Main

def main() -> None:
    parser = argparse.ArgumentParser(description="Square-Sum Circle for 1..N (P3 style).")
    parser.add_argument("N", nargs="?", default="32", help="Upper bound N (default: 32)")
    parser.add_argument("--limit", type=int, default=10_000_000,
                        help="Search step budget for the robust solver (default: 10,000,000)")
    parser.add_argument("--strategy", choices=["auto","fast","robust"], default="auto",
                        help="Search strategy: auto (default), fast (even N), robust (bounded)")
    args = parser.parse_args()

    try:
        N = int(args.N)
    except ValueError:
        print("Usage: python square_sum_circle_anyN.py [N] [--strategy auto|fast|robust] [--limit STEPS]")
        sys.exit(2)
    if N < 2:
        print("N must be ≥ 2.")
        sys.exit(2)

    squares = perfect_squares_up_to(2 * N)
    edges = build_edges(N, squares)
    degrees = {v: len(edges[v]) for v in edges}

    # --- Quick structural witnesses (proofs of impossibility)
    witness = None
    deg_lt2 = [v for v, d in degrees.items() if d < 2]
    if deg_lt2:
        witness = f"At least one vertex has degree < 2 (e.g., {deg_lt2[:5]}). " \
                  f"In any 2-regular Hamiltonian cycle, every vertex must have degree ≥ 2."
    elif not is_connected(edges):
        witness = "Underlying square-sum graph is disconnected."

    cycle = None
    steps_used = 0
    strategy_used = args.strategy

    # If no immediate impossibility, run solver
    if witness is None:
        cycle, steps_used, strategy_used = find_square_sum_cycle(N, edges, args.strategy, args.limit)
        if cycle is None:
            witness = "Search found no Hamiltonian cycle (no certificate). " \
                      "Given all vertices have degree ≥ 2 and the graph is connected, " \
                      "this does not constitute a proof of impossibility, but no solution was found."

    # --- Answer
    print_header("ANSWER")
    if cycle is not None:
        print(f"A valid Square-Sum Circle for 1–{N} was found:")
        print("Order (clockwise):")
        print("  " + " ".join(map(str, cycle)))
        print("Adjacent sums:")
        pairs = [f"{cycle[i]}+{cycle[(i+1)%N]}={(cycle[i]+cycle[(i+1)%N])}" for i in range(N)]
        print("  " + ", ".join(pairs))
    else:
        if "no Hamiltonian cycle" in (witness or "") and "does not constitute a proof" in witness:
            print(f"No solution found for 1–{N} (search inconclusive).")
        else:
            print(f"No solution exists for 1–{N}.")
        print("Reason (witness):", witness)

    # --- Reason Why
    print_header("REASON WHY")
    print(build_reason(N, squares, edges, cycle=cycle, witness=witness,
                       steps=steps_used, strategy=strategy_used))

    # --- Check (harness)
    print_header("CHECK (HARNESS)")
    try:
        for line in run_checks(N, squares, edges, cycle=cycle, witness=witness):
            print(line)
        if cycle is not None:
            print("All checks PASSED ✅")
        else:
            print("Checks completed ✅")
    except AssertionError as e:
        print(f"Check FAILED ❌: {e}")
        raise

if __name__ == "__main__":
    main()

