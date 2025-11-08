#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Hamiltonian paths — ARC (Answer / Reason / Check), self-contained

Answer:
  Enumerates every Hamiltonian path in the given (undirected) graph and
  prints a tidy summary plus a small sample.

Reason why:
  Explains the backtracking search (safety checks) and the independent
  dynamic-programming (DP) count used as a cross-check.

Check (harness):
  Validates each reported path (adjacency + no repeats), uniqueness,
  reverse-closure (since the graph is undirected, every path’s reverse
  should be present), and that a DP count matches the enumerated total.
"""

from typing import List, Tuple, Iterable, Set

# ───────────────────────────── Problem instance ─────────────────────────────
# Same 6-vertex example as the original script (0..5).
GRAPH: List[List[int]] = [
    [0, 1, 1, 0, 0, 0],  # 0
    [1, 0, 1, 1, 0, 0],  # 1
    [1, 1, 0, 1, 1, 0],  # 2
    [0, 1, 1, 0, 1, 1],  # 3
    [0, 0, 1, 1, 0, 1],  # 4
    [0, 0, 0, 1, 1, 0],  # 5
]

PRINT_MAX = 40  # show at most this many paths in the "Answer" section


# ───────────────────────────── Core backtracking ─────────────────────────────
def all_hamiltonian_paths(graph: List[List[int]]) -> List[List[int]]:
    """Enumerate all Hamiltonian paths in an undirected graph (adjacency matrix)."""
    n = len(graph)
    path = [-1] * n
    out: List[List[int]] = []

    def is_safe(v: int, pos: int) -> bool:
        # Must be adjacent to predecessor
        if graph[path[pos - 1]][v] == 0:
            return False
        # Must not revisit a vertex
        if v in path[:pos]:
            return False
        return True

    def backtrack(pos: int) -> None:
        if pos == n:
            out.append(path.copy())
            return
        for v in range(n):
            if is_safe(v, pos):
                path[pos] = v
                backtrack(pos + 1)
                path[pos] = -1

    # Try every vertex as a starting point
    for s in range(n):
        path[0] = s
        backtrack(1)
        path[0] = -1

    return out


# ───────────────────────────── Pretty helpers ────────────────────────────────
def show_adjacency(graph: List[List[int]]) -> None:
    print("Adjacency Matrix:")
    for row in graph:
        print(" ", row)

def path_ok(graph: List[List[int]], p: List[int]) -> bool:
    """Check that 'p' is Hamiltonian in 'graph'."""
    n = len(graph)
    if len(p) != n or len(set(p)) != n:
        return False
    for a, b in zip(p, p[1:]):
        if graph[a][b] == 0:
            return False
    return True

def dedup_up_to_reversal(paths: Iterable[List[int]]) -> Set[Tuple[int, ...]]:
    """Return a set of canonical tuples where each path and its reverse map to the same key."""
    canon: Set[Tuple[int, ...]] = set()
    for p in paths:
        t = tuple(p)
        r = tuple(reversed(p))
        canon.add(min(t, r))
    return canon


# ────────────────────────────────── ARC: Answer ──────────────────────────────
def print_answer() -> None:
    print("Answer")
    print("======")
    n = len(GRAPH)
    print(f"Graph with {n} vertices (0..{n-1})")
    show_adjacency(GRAPH)

    paths = all_hamiltonian_paths(GRAPH)
    print(f"\nFound {len(paths)} Hamiltonian path(s). Showing up to {PRINT_MAX}:\n")
    for i, p in enumerate(paths[:PRINT_MAX], 1):
        print(f"  {i:2d}. {p}")

    if len(paths) > PRINT_MAX:
        print(f"... ({len(paths) - PRINT_MAX} more not shown)")

    # Small summary
    unique_up_to_rev = dedup_up_to_reversal(paths)
    print("\nSummary")
    print("-------")
    print(f"Total paths: {len(paths)}")
    print(f"Distinct up to reversal: {len(unique_up_to_rev)}")


# ───────────────────────────────── ARC: Reason why ───────────────────────────
def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We search depth-first with backtracking:")
    print("  • Fix a starting vertex s and extend a path one vertex at a time.")
    print("  • A candidate v is SAFE at position pos iff:")
    print("      – v is adjacent to the previous vertex; and")
    print("      – v hasn’t appeared earlier in the path.")
    print("  • When the path reaches length n (number of vertices), it is Hamiltonian.")
    print("\nTo cross-check completeness, the harness also counts Hamiltonian paths")
    print("using an independent bitmask DP (Held–Karp style). Both counts must agree.")


# ─────────────────────────────── ARC: Check (harness) ────────────────────────
def count_hamiltonian_paths_dp(graph: List[List[int]]) -> int:
    """
    Independent count via bitmask DP.
    dp[mask][v] = number of ways to start at the fixed start s (included in mask),
    visit exactly 'mask' and end at v. We sum over all starts s and endpoints v.
    """
    n = len(graph)
    ALL = (1 << n) - 1
    total = 0

    for s in range(n):
        dp = [[0] * n for _ in range(1 << n)]
        dp[1 << s][s] = 1
        for mask in range(1 << n):
            if not (mask & (1 << s)):
                continue
            for v in range(n):
                if not (mask & (1 << v)):
                    continue
                ways = dp[mask][v]
                if ways == 0:
                    continue
                # extend to a new vertex u ∉ mask
                nxt_mask_candidates = (~mask) & ALL
                u = 0
                while nxt_mask_candidates:
                    lsb = nxt_mask_candidates & -nxt_mask_candidates
                    u = (lsb.bit_length() - 1)
                    nxt_mask_candidates ^= lsb
                    if graph[v][u]:
                        dp[mask | (1 << u)][u] += ways
        total += sum(dp[ALL][v] for v in range(n))

    return total

def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # A) Basic graph sanity (simple undirected adjacency matrix)
    n = len(GRAPH)
    sym_ok = all(GRAPH[i][i] == 0 for i in range(n)) and all(
        GRAPH[i][j] == GRAPH[j][i] for i in range(n) for j in range(n)
    )
    print(f"Adjacency is symmetric with zero diagonal? {sym_ok}")
    ok_all &= sym_ok

    # B) Enumerate and validate each path
    paths = all_hamiltonian_paths(GRAPH)
    each_valid = all(path_ok(GRAPH, p) for p in paths)
    unique = (len({tuple(p) for p in paths}) == len(paths))
    print(f"Every reported path is valid Hamiltonian? {each_valid}")
    print(f"No duplicates in enumeration?            {unique}")
    ok_all &= each_valid and unique

    # C) Reverse-closure in an undirected graph
    as_set = {tuple(p) for p in paths}
    reverse_present = all(tuple(reversed(p)) in as_set for p in paths)
    print(f"Reverse of each path is also enumerated?  {reverse_present}")
    # In these data, no path equals its own reverse (open paths), so:
    pairs_ok = (len(dedup_up_to_reversal(paths)) * 2 == len(paths))
    print(f"Half as many paths up to reversal?        {pairs_ok}")
    ok_all &= reverse_present and pairs_ok

    # D) Independent DP count equals enumeration size
    dp_total = count_hamiltonian_paths_dp(GRAPH)
    print(f"DP count equals enumerated total?         {dp_total == len(paths)} "
          f"(DP={dp_total}, enumerated={len(paths)})")
    ok_all &= (dp_total == len(paths))

    print(f"\nAll checks passed? {ok_all}")


# ─────────────────────────────────── Main ────────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

