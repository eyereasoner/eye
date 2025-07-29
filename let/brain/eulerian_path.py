#!/usr/bin/env python3
"""
Enumerate Eulerian paths/trails for an undirected graph and print a summary.

- Lists Eulerian trails starting from a chosen vertex (default: pick a reasonable one).
- Can deduplicate cycles (rotation & reversal) or open trails (reversal) for printing.
- Always prints a summary:
    * number of distinct circuits (or open trails) up to canonical equivalence
    * counts per starting vertex
    * total vertex sequences across all starts/directions

Usage examples:
  python euler_trails.py
  python euler_trails.py --start v1
  python euler_trails.py --dedup
  python euler_trails.py --max 20000 --outfile trails.txt
"""

from collections import defaultdict, deque
from typing import Dict, Iterable, Iterator, List, Tuple, Optional, Set
import argparse
import sys

Vertex = str
Edge = Tuple[Vertex, Vertex]
Path = List[Vertex]


# ---------- Input: your edges (undirected) ----------
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


# ---------- Graph utilities ----------
class MultiGraph:
    def __init__(self, edges: List[Edge]):
        self.edges: List[Edge] = list(edges)
        self.V: Set[Vertex] = set()
        for u, v in self.edges:
            self.V.add(u)
            self.V.add(v)

        # adjacency: vertex -> list of edge indices incident to it
        self.adj: Dict[Vertex, List[int]] = {v: [] for v in self.V}
        for i, (u, v) in enumerate(self.edges):
            self.adj[u].append(i)
            self.adj[v].append(i)

        # degrees (counting multiplicity)
        self.deg: Dict[Vertex, int] = {v: len(self.adj[v]) for v in self.V}

    def other(self, eid: int, at: Vertex) -> Vertex:
        u, v = self.edges[eid]
        if at == u:
            return v
        elif at == v:
            return u
        else:
            raise ValueError(f"Edge {eid} does not touch vertex {at}")

    def non_isolated_vertices(self) -> Set[Vertex]:
        return {v for v in self.V if self.deg[v] > 0}

    def connected_ignoring_isolated(self) -> bool:
        """Check connectivity among vertices with deg > 0."""
        verts = list(self.non_isolated_vertices())
        if not verts:
            return True  # empty/trivial
        start = verts[0]
        seen = set([start])
        dq = deque([start])
        while dq:
            x = dq.popleft()
            for eid in self.adj[x]:
                y = self.other(eid, x)
                if y not in seen and self.deg[y] > 0:
                    seen.add(y)
                    dq.append(y)
        return seen == set(verts)

    def odd_vertices(self) -> List[Vertex]:
        return [v for v in self.V if self.deg[v] % 2 == 1]


def eulerian_status(G: MultiGraph) -> Tuple[str, List[Vertex]]:
    """
    Returns ("none" | "trail" | "circuit", important_vertices)
    - "none": no Eulerian trail exists (disconnected or >2 odd vertices)
    - "trail": exactly two odd vertices; Eulerian trails start at one and end at the other
               important_vertices = [odd1, odd2]
    - "circuit": all degrees even; Eulerian circuits exist
                 important_vertices = [] (start anywhere with deg>0)
    """
    if not G.connected_ignoring_isolated():
        return ("none", [])
    odds = G.odd_vertices()
    if len(odds) == 0:
        return ("circuit", [])
    elif len(odds) == 2:
        return ("trail", odds)
    else:
        return ("none", odds)


# ---------- Canonicalization for deduplication ----------
def canonicalize_cycle(path: Path) -> Tuple[str, ...]:
    """
    Given a circuit path as a vertex list with path[0] == path[-1],
    return a canonical tuple invariant under rotation and reversal.
    """
    assert path[0] == path[-1], "canonicalize_cycle expects a closed cycle"
    seq = path[:-1]  # drop the repeated last vertex
    n = len(seq)
    rots = [tuple(seq[i:] + seq[:i]) for i in range(n)]
    rev = list(reversed(seq))
    rots_rev = [tuple(rev[i:] + rev[:i]) for i in range(n)]
    return min(rots + rots_rev)


def canonicalize_trail(path: Path) -> Tuple[str, ...]:
    """
    For open trails (two odd vertices), deduplicate up to reversal.
    """
    tup = tuple(path)
    return min(tup, tuple(reversed(tup)))


# ---------- Eulerian enumeration (backtracking) ----------
def enumerate_euler_trails(
    G: MultiGraph,
    start: Vertex,
    require_circuit_end: bool,
) -> Iterator[Path]:
    """
    Enumerate Eulerian trails by DFS over the multiset of edges.
    If require_circuit_end is True, only yield paths that end at the start vertex.
    """
    m = len(G.edges)
    used = [False] * m
    path: Path = [start]

    # Deterministic ordering (purely for reproducibility of output order)
    adj_sorted: Dict[Vertex, List[int]] = {}
    for v in G.V:
        adj_sorted[v] = sorted(
            G.adj[v],
            key=lambda eid: (G.other(eid, v), eid)
        )

    def backtrack(at: Vertex, used_count: int):
        if used_count == m:
            if not require_circuit_end or at == start:
                yield list(path)
            return
        for eid in adj_sorted[at]:
            if used[eid]:
                continue
            used[eid] = True
            nxt = G.other(eid, at)
            path.append(nxt)
            yield from backtrack(nxt, used_count + 1)
            path.pop()
            used[eid] = False

    yield from backtrack(start, 0)


# ---------- CLI / main ----------
def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Enumerate Eulerian trails for a given undirected graph.")
    p.add_argument("--start", type=str, default=None,
                   help="Start vertex (must be one of the odd-degree vertices for an open trail).")
    p.add_argument("--dedup", action="store_true",
                   help="Deduplicate output when printing (cycles: rotation+reversal; open trails: reversal).")
    p.add_argument("--max", type=int, default=None,
                   help="Stop after enumerating this many trails (safety cap).")
    p.add_argument("--outfile", type=str, default=None,
                   help="Write printed trails to this file (one per line).")
    return p.parse_args()


def format_path(path: Path) -> str:
    return " - ".join(path)


def main():
    G = MultiGraph(EDGES)
    status, info = eulerian_status(G)

    # Report basic info
    print("Vertices:", ", ".join(sorted(G.V)))
    print("Edges:", ", ".join(f"{u}-{v}" for u, v in G.edges))
    print("Degrees:", {v: G.deg[v] for v in sorted(G.V)})
    print("Connected (ignoring isolated):", G.connected_ignoring_isolated())

    if status == "none":
        print("No Eulerian trail: odd-degree vertices =", info)
        sys.exit(0)

    print(f"Eulerian status: {status}")
    if status == "trail":
        print(f"Odd-degree vertices (must be start/end): {info[0]} and {info[1]}")

    args = parse_args()

    # Choose start vertex
    start: Optional[Vertex] = args.start
    if status == "trail":
        odd1, odd2 = info
        if start is None:
            start = min(odd1, odd2)  # deterministic
        elif start not in (odd1, odd2):
            print(f"Warning: start='{start}' is not one of the odd vertices {odd1}, {odd2}. "
                  f"Starting at {odd1} instead.")
            start = odd1
        require_circuit_end = False
    else:  # circuit
        if start is None:
            # pick a vertex with deg>0 (lexicographically smallest)
            candidates = sorted(v for v in G.V if G.deg[v] > 0)
            start = candidates[0] if candidates else None
        require_circuit_end = True

    if start is None:
        print("Graph has no edges; nothing to enumerate.")
        sys.exit(0)

    # Enumerate from the chosen start, optionally dedup for printing,
    # but ALWAYS track distinct trails/circuits for the summary.
    print(f"Enumerating Eulerian {'circuits' if require_circuit_end else 'trails'} starting at {start} ...")
    printed_count = 0
    distinct_keys: Set[Tuple[str, ...]] = set()  # always track distinct
    printed_keys: Set[Tuple[str, ...]] = set()   # only for --dedup behavior when printing

    # Optional file output
    out_handle = open(args.outfile, "w") if args.outfile else None

    # Helper to get canonical key
    canon = canonicalize_cycle if require_circuit_end else canonicalize_trail

    try:
        for path in enumerate_euler_trails(G, start, require_circuit_end):
            key = canon(path)
            # Track distinct always
            if key not in distinct_keys:
                distinct_keys.add(key)

            # Decide whether to print this path
            if args.dedup:
                if key in printed_keys:
                    # Skip printing duplicate representation
                    continue
                printed_keys.add(key)

            line = format_path(path)
            print(line)
            if out_handle:
                out_handle.write(line + "\n")

            printed_count += 1
            if args.max is not None and printed_count >= args.max:
                print(f"Stopped after reaching the cap (--max {args.max}).")
                break
    finally:
        if out_handle:
            out_handle.close()

    # -------------------- SUMMARY --------------------
    # Distinct count:
    distinct_count = len(distinct_keys)

    # Counts per start vertex and grand total across all starts/directions:
    counts_per_start: Dict[Vertex, int] = {v: 0 for v in sorted(G.V)}
    if status == "circuit":
        # For every distinct circuit C:
        # - from a fixed start vertex v, the number of vertex sequences representing C is deg(v)
        # - grand total across all starts/directions is sum_v deg(v) = 2|E|
        for v in counts_per_start:
            counts_per_start[v] = G.deg[v] * distinct_count
        total_across_all_starts = sum(G.deg.values()) * distinct_count
        kind = "distinct Eulerian circuits (up to rotation & reversal)"
    else:
        # Open-trail case (exactly two odds): each distinct trail contributes
        # one sequence from each odd vertex and none from others.
        odd1, odd2 = info
        for v in counts_per_start:
            if v == odd1 or v == odd2:
                counts_per_start[v] = distinct_count
            else:
                counts_per_start[v] = 0
        total_across_all_starts = 2 * distinct_count
        kind = "distinct Eulerian open trails (up to reversal)"

    print("\n===== SUMMARY =====")
    print(f"{kind}: {distinct_count}")
    print("Vertex-sequence counts per start vertex:")
    for v in sorted(counts_per_start):
        print(f"  {v}: {counts_per_start[v]}")
    print(f"Total vertex sequences across all starts/directions: {total_across_all_starts}")

    # Also report how many paths we printed in this run (depends on flags and max)
    print(f"\nPrinted this run: {printed_count} "
          f"({'deduped' if args.dedup else 'with duplicates'})")


if __name__ == "__main__":
    main()

