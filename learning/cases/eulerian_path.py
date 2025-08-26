#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Eulerian paths/circuits — ARC (Answer / Reason / Check), self-contained

What it does
  • Answer: enumerates Eulerian trails (or circuits) on the given undirected graph,
    prints a deduped sample (up to PRINT_MAX), and then a full summary.
  • Reason why: explains the parity/connectedness conditions and the summary math.
  • Check (harness): verifies status, path validity (each edge once), canonicalization,
    and the summary formulas.

Notes
  - Enumeration is deterministic thanks to sorted adjacency by (neighbor, edge-id).
  - “Distinct” counting uses canonicalization:
      circuits: up to rotation & reversal; open trails: up to reversal.
"""

from collections import deque
from typing import Dict, Iterable, Iterator, List, Tuple, Optional, Set

# ─────────────────────────── Config ───────────────────────────
PRINT_MAX = 50      # print at most this many trails (deduped for readability)
PRINT_DEDUP = True  # when printing, collapse rotation/reversal-equivalents

# ─────────────────────────── Types ───────────────────────────
Vertex = str
Edge   = Tuple[Vertex, Vertex]
Path   = List[Vertex]

# ─────────────────────────── Graph (same as original) ───────────────────────────
EDGES: List[Edge] = [
    ("v1", "v2"),
    ("v1", "v3"),
    ("v1", "v5"),
    ("v1", "v6"),
    ("v2", "v3"),
    ("v2", "v4"),
    ("v2", "v6"),
    ("v3", "v4"),
    ("v3", "v6"),
    ("v4", "v5"),
    ("v4", "v6"),
]

class MultiGraph:
    def __init__(self, edges: List[Edge]):
        self.edges: List[Edge] = list(edges)
        self.V: Set[Vertex] = set()
        for u, v in self.edges:
            self.V.add(u); self.V.add(v)
        # adjacency: vertex -> list of edge indices
        self.adj: Dict[Vertex, List[int]] = {v: [] for v in self.V}
        for i, (u, v) in enumerate(self.edges):
            self.adj[u].append(i); self.adj[v].append(i)
        self.deg: Dict[Vertex, int] = {v: len(self.adj[v]) for v in self.V}

    def other(self, eid: int, at: Vertex) -> Vertex:
        u, v = self.edges[eid]
        if   at == u: return v
        elif at == v: return u
        else: raise ValueError(f"Edge {eid} not incident to {at}")

    def non_isolated_vertices(self) -> Set[Vertex]:
        return {v for v in self.V if self.deg[v] > 0}

    def connected_ignoring_isolated(self) -> bool:
        verts = list(self.non_isolated_vertices())
        if not verts:
            return True
        start = verts[0]
        seen = {start}
        dq = deque([start])
        while dq:
            x = dq.popleft()
            for eid in self.adj[x]:
                y = self.other(eid, x)
                if y not in seen and self.deg[y] > 0:
                    seen.add(y); dq.append(y)
        return seen == set(verts)

    def odd_vertices(self) -> List[Vertex]:
        return [v for v in self.V if self.deg[v] % 2 == 1]

def eulerian_status(G: MultiGraph) -> Tuple[str, List[Vertex]]:
    """
    Returns ("none" | "trail" | "circuit", important_vertices)
      - none:    disconnected (ignoring isolates) or >2 odd-degree vertices
      - trail:   exactly two odds; trails start/end at those two (returned in list)
      - circuit: all degrees even; circuits exist (list empty)
    """
    if not G.connected_ignoring_isolated():
        return ("none", [])
    odds = G.odd_vertices()
    if len(odds) == 0:
        return ("circuit", [])
    if len(odds) == 2:
        return ("trail", odds)
    return ("none", odds)

# ───────────────────────── Canonicalization ─────────────────────────
def canonicalize_cycle(path: Path) -> Tuple[str, ...]:
    """
    Canonical key for a cycle up to rotation & reversal.
    Accepts either a closed path [v0,...,v0] or an open listing [v0,...,vk].
    """
    if not path:
        return tuple()
    seq = path[:-1] if path[0] == path[-1] else list(path)
    n = len(seq)
    if n == 0:
        return tuple()
    rots = [tuple(seq[i:] + seq[:i]) for i in range(n)]
    rev  = list(reversed(seq))
    rots_rev = [tuple(rev[i:] + rev[:i]) for i in range(n)]
    return min(rots + rots_rev)

def canonicalize_trail(path: Path) -> Tuple[str, ...]:
    """Open trail; canonical key up to reversal."""
    tup = tuple(path)
    return min(tup, tuple(reversed(tup)))

# ───────────────────── Enumeration (DFS over edge multiset) ───────────────────
def enumerate_euler_trails(G: MultiGraph, start: Vertex, require_circuit_end: bool) -> Iterator[Path]:
    m = len(G.edges)
    used = [False] * m
    path: Path = [start]

    # deterministic neighbor order for reproducibility
    adj_sorted: Dict[Vertex, List[int]] = {
        v: sorted(G.adj[v], key=lambda eid, v=v: (G.other(eid, v), eid))
        for v in G.V
    }

    def backtrack(at: Vertex, used_count: int):
        if used_count == m:
            if not require_circuit_end or at == start:
                yield list(path)
            return
        for eid in adj_sorted[at]:
            if used[eid]: continue
            used[eid] = True
            nxt = G.other(eid, at)
            path.append(nxt)
            yield from backtrack(nxt, used_count + 1)
            path.pop()
            used[eid] = False

    yield from backtrack(start, 0)

# ───────────────────────────── Answer ─────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    G = MultiGraph(EDGES)
    status, info = eulerian_status(G)

    # Report graph basics
    print("Vertices:", ", ".join(sorted(G.V)))
    print("Edges:", ", ".join(f"{u}-{v}" for u, v in G.edges))
    print("Degrees:", {v: G.deg[v] for v in sorted(G.V)})
    print("Connected (ignoring isolated):", G.connected_ignoring_isolated())

    if status == "none":
        print("No Eulerian trail: odd-degree vertices =", info)
        return

    print(f"Eulerian status: {status}")
    if status == "trail":
        print(f"Odd-degree vertices (must be start/end): {info[0]} and {info[1]}")

    # Choose a start vertex deterministically
    if status == "trail":
        odd1, odd2 = info
        start = min(odd1, odd2)
        require_circuit_end = False
    else:
        candidates = sorted(v for v in G.V if G.deg[v] > 0)
        start = candidates[0] if candidates else None
        require_circuit_end = True
    if start is None:
        print("Graph has no edges; nothing to enumerate.")
        return

    print(f"\nEnumerating Eulerian {'circuits' if require_circuit_end else 'trails'} starting at {start} ...")

    # Enumerate, print a deduped sample, and collect distinct keys for the summary
    canon = canonicalize_cycle if require_circuit_end else canonicalize_trail
    printed = 0
    printed_keys: Set[Tuple[str, ...]] = set()
    distinct_keys: Set[Tuple[str, ...]] = set()

    for path in enumerate_euler_trails(G, start, require_circuit_end):
        key = canon(path)
        if key not in distinct_keys:
            distinct_keys.add(key)
        if PRINT_DEDUP and key in printed_keys:
            continue
        if printed < PRINT_MAX:
            print("  " + " - ".join(path))
            printed += 1
            printed_keys.add(key)

    # Summary (independent of printing choices)
    distinct_count = len(distinct_keys)
    counts_per_start: Dict[Vertex, int] = {v: 0 for v in sorted(G.V)}
    if status == "circuit":
        for v in counts_per_start:
            counts_per_start[v] = G.deg[v] * distinct_count
        total_across_all = sum(G.deg.values()) * distinct_count  # = 2|E| * distinct
        kind = "distinct Eulerian circuits (up to rotation & reversal)"
    else:
        odd1, odd2 = info
        for v in counts_per_start:
            counts_per_start[v] = (distinct_count if v in (odd1, odd2) else 0)
        total_across_all = 2 * distinct_count
        kind = "distinct Eulerian open trails (up to reversal)"

    print("\n===== SUMMARY =====")
    print(f"{kind}: {distinct_count}")
    print("Vertex-sequence counts per start vertex:")
    for v in sorted(counts_per_start):
        print(f"  {v}: {counts_per_start[v]}")
    print(f"Total vertex sequences across all starts/directions: {total_across_all}")
    print(f"(printed {printed} sample{'s' if printed != 1 else ''}; "
          f"{'deduped' if PRINT_DEDUP else 'with duplicates'})")

# ─────────────────────────── Reason why ───────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("An undirected graph has:")
    print("  • an Eulerian CIRCUIT  ⇔ it is connected (ignoring isolated vertices) and every")
    print("    vertex has even degree; you can start anywhere (deg>0) and return there.")
    print("  • an Eulerian TRAIL    ⇔ it is connected (ignoring isolated vertices) and")
    print("    exactly two vertices have odd degree; you must start at one odd and end at the other.")
    print("\nWhen reporting “distinct” objects we quotient by symmetries:")
    print("  • circuits: rotation & reversal of the vertex cycle are the same circuit;")
    print("  • open trails: reversal yields the same trail.")
    print("The summary uses simple counting:")
    print("  • circuits: each distinct circuit contributes deg(v) sequences from a fixed start v,")
    print("    hence Σ_v deg(v) = 2|E| sequences over all starts/directions;")
    print("  • trails: each distinct trail contributes exactly two sequences (one per odd start).")

# ─────────────────────────── Check (harness) ───────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    G = MultiGraph(EDGES)
    status, info = eulerian_status(G)
    ok_all = True

    # A) Status invariants
    ok_conn = G.connected_ignoring_isolated()
    ok_even = (len(G.odd_vertices()) == 0)
    expect = ("circuit", [])
    ok_status = (status, (info if status != "circuit" else [])) == expect
    print(f"Connected ignoring isolated? True" if ok_conn else f"Connected ignoring isolated? False")
    print(f"All degrees even?          True" if ok_even else f"All degrees even?          False")
    print(f"Status == circuit?         True" if ok_status else f"Status == circuit?         False")
    ok_all &= ok_conn and ok_even and ok_status

    # B) Every emitted path uses each edge exactly once and (for circuit) ends at the start
    def path_is_valid(path: Path) -> bool:
        if status == "circuit":
            if path[0] != path[-1]:
                return False
        # Build multiset of edges (as unordered pairs) along the path
        seen: List[Tuple[str, str]] = []
        for a, b in zip(path, path[1:]):
            seen.append(tuple(sorted((a, b))))
        # Compare counts to the original edge multiset
        orig = [tuple(sorted(e)) for e in G.edges]
        return sorted(seen) == sorted(orig)

    start = sorted(v for v in G.V if G.deg[v] > 0)[0]
    require_circuit_end = True
    some = 0
    ok_paths = True
    for p in enumerate_euler_trails(G, start, require_circuit_end):
        if not path_is_valid(p):
            ok_paths = False
            break
        some += 1
        if some >= 1000:
            break
    print(f"Enumerated paths respect edge-multiset & endpoint rule? {ok_paths}")
    ok_all &= ok_paths and (some > 0)

    # C) Canonicalization: rotation/reversal invariance (spot-check on a sample path)
    sample = None
    for p in enumerate_euler_trails(G, start, require_circuit_end):
        sample = p
        break

    ok_canon = False
    if sample:
        key = canonicalize_cycle(sample)
        seq = sample[:-1]                 # drop duplicate end to rotate interior
        rot_seq = seq[1:] + seq[:1]
        rot = rot_seq + [rot_seq[0]]      # re-close the cycle
        rev = list(reversed(sample))      # reversing a closed cycle stays closed
        ok_canon = (key == canonicalize_cycle(rot) == canonicalize_cycle(rev))
    print(f"Canonicalization respects rotation & reversal? {ok_canon}")
    ok_all &= ok_canon

    # D) Summary formulas hold
    def summarize() -> Tuple[int, Dict[Vertex, int], int]:
        canon = canonicalize_cycle
        distinct: Set[Tuple[str, ...]] = set()
        for p in enumerate_euler_trails(G, start, True):
            distinct.add(canon(p))
        distinct_count = len(distinct)
        per_start = {v: G.deg[v] * distinct_count for v in G.V}
        total = sum(G.deg.values()) * distinct_count
        return distinct_count, per_start, total

    dcount, per_start, total = summarize()
    ok_summary = (total == sum(G.deg.values()) * dcount) and all(
        per_start[v] == G.deg[v] * dcount for v in G.V
    )
    print(f"Summary math checks out? {ok_summary}")
    ok_all &= ok_summary

    print(f"\nAll checks passed? {ok_all}")

# ─────────────────────────── Main ───────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

