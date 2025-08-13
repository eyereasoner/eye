#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
dijkstra.py — Dijkstra’s single-source shortest paths (ARC-ified)
─────────────────────────────────────────────────────────────────────

What you get
------------
• Answer
    – Runs Dijkstra on a demo weighted graph, prints shortest distances
      from 'A' and reconstructs one shortest path to each node.

• Reason why
    – Explains the greedy property (extract-min by distance; relax all out-edges)
      and why it proves optimality on graphs with non-negative weights.

• Check (harness)
    – Validates correctness and safety by:
        1) enforcing non-negative weights,
        2) verifying the predecessor tree consistency (each v has prev[v] and
           dist[v] = dist[prev[v]] + w(prev, v)),
        3) confirming triangle inequality on all edges,
        4) comparing distances against a Bellman–Ford relaxation (V−1 rounds),
        5) ensuring determinism across repeated runs.

API
---
    dijkstra(graph, source) -> (dist, prev)
        graph : Dict[node, List[(neighbor, weight)]], weights ≥ 0
        dist  : Dict[node, distance]
        prev  : Dict[node, predecessor on *a* shortest path)

    reconstruct(prev, s, t) -> List[node]   (empty if unreachable)
"""

from __future__ import annotations
import heapq
from typing import Dict, List, Tuple, Hashable, Iterable

Node   = Hashable
Weight = float
Graph  = Dict[Node, List[Tuple[Node, Weight]]]

# ─────────────────────────────────────────────────────────────
# Core algorithm
# ─────────────────────────────────────────────────────────────
def dijkstra(graph: Graph, source: Node) -> Tuple[Dict[Node, Weight], Dict[Node, Node]]:
    """
    Single-source shortest paths (non-negative weights).

    Parameters
    ----------
    graph  : adjacency list {u: [(v, w), ...], ...}
    source : start node

    Returns
    -------
    dist : {node: shortest distance from source}
    prev : {node: predecessor on a shortest path}
    """
    dist: Dict[Node, Weight] = {v: float("inf") for v in graph}
    prev: Dict[Node, Node]   = {}
    dist[source] = 0.0

    pq: List[Tuple[Weight, Node]] = [(0.0, source)]       # (d, v)
    while pq:
        d, u = heapq.heappop(pq)
        if d != dist[u]:                                  # stale queue entry
            continue
        for v, w in graph[u]:
            alt = d + w
            if alt < dist[v]:
                dist[v] = alt
                prev[v] = u
                heapq.heappush(pq, (alt, v))
    return dist, prev

# ─────────────────────────────────────────────────────────────
# Utilities: path reconstruction & a simple Bellman–Ford check
# ─────────────────────────────────────────────────────────────
def reconstruct(prev: Dict[Node, Node], s: Node, t: Node) -> List[Node]:
    """Return one shortest path s→t using the predecessor map (or [] if unreachable)."""
    if s == t:
        return [s]
    path: List[Node] = []
    cur = t
    while cur in prev:
        path.append(cur)
        cur = prev[cur]
        if cur == s:
            path.append(s)
            return path[::-1]
    return []  # no path

def edge_list(graph: Graph) -> List[Tuple[Node, Node, Weight]]:
    """Flatten adjacency into a list of directed edges (u, v, w)."""
    return [(u, v, w) for u, nbrs in graph.items() for v, w in nbrs]

def bellman_ford_verify(graph: Graph, source: Node) -> Dict[Node, Weight]:
    """
    Compute distances by V−1 rounds of relaxation (no negative edges assumed).
    Returns the distance dictionary (float('inf') if unreachable).
    """
    nodes = list(graph.keys())
    idx = {u: i for i, u in enumerate(nodes)}
    dist = {u: float("inf") for u in nodes}
    dist[source] = 0.0
    E = edge_list(graph)
    for _ in range(len(nodes) - 1):
        improved = False
        for u, v, w in E:
            if dist[u] + w < dist[v]:
                dist[v] = dist[u] + w
                improved = True
        if not improved:
            break
    # (Optional negative-cycle check omitted; inputs must be non-negative here.)
    return dist

def weight_lookup(graph: Graph, u: Node, v: Node) -> Weight | None:
    """Return the minimum w among parallel edges u→v, or None if absent."""
    ws = [w for x, w in graph[u] if x == v]
    return min(ws) if ws else None

# ─────────────────────────────────────────────────────────────
# ARC — Answer
# ─────────────────────────────────────────────────────────────
def arc_answer(graph: Graph, source: Node) -> Tuple[Dict[Node, Weight], Dict[Node, Node]]:
    print("Answer")
    print("------")
    dist, prev = dijkstra(graph, source)
    print(f"Source: {source}")
    print("Shortest distances:")
    for v in sorted(graph.keys(), key=str):
        d = dist[v]
        print(f"  {source} → {v}: {d if d < float('inf') else '∞'}")
    print("\nOne shortest path to each reachable node:")
    for v in sorted(graph.keys(), key=str):
        path = reconstruct(prev, source, v)
        if path:
            print(f"  {source} ⇒ {v}: {' -> '.join(map(str, path))}  (length {dist[v]})")
        else:
            print(f"  {source} ⇏ {v}: unreachable")
    print()
    return dist, prev

# ─────────────────────────────────────────────────────────────
# ARC — Reason why
# ─────────────────────────────────────────────────────────────
def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("Dijkstra maintains a set of settled nodes whose distances are final.")
    print("At each step it extracts the unsettled node with smallest tentative")
    print("distance (min-heap) and relaxes all outgoing edges. With non-negative")
    print("weights, once a node u is extracted, no other path can later reach u")
    print("with a shorter distance, so dist[u] is optimal. Repeating this proves")
    print("optimality for all reachable nodes.")
    print()

# ─────────────────────────────────────────────────────────────
# ARC — Check (harness)
# ─────────────────────────────────────────────────────────────
def arc_check(graph: Graph, source: Node, dist: Dict[Node, Weight], prev: Dict[Node, Node]) -> None:
    print("Check (harness)")
    print("---------------")

    # 0) basic graph sanity & non-negative weights
    assert source in graph, "Source not in graph."
    for u, nbrs in graph.items():
        for v, w in nbrs:
            assert w >= 0, f"Negative weight on edge {u}->{v}: {w}"

    # 1) dist[source] == 0, others ≥ 0 (or ∞ if unreachable)
    assert abs(dist[source]) < 1e-15, "Distance to source must be 0."
    for v, d in dist.items():
        assert d >= 0 or d == float("inf"), f"Negative distance found for {v}: {d}"

    # 2) predecessor consistency for reachable nodes
    for v, d in dist.items():
        if v == source or d == float("inf"):
            continue
        assert v in prev, f"Missing predecessor for reachable node {v}"
        u = prev[v]
        w = weight_lookup(graph, u, v)
        assert w is not None, f"prev edge {u}->{v} not found in graph"
        assert abs(d - (dist[u] + w)) < 1e-12, f"Broken invariant: dist[{v}] ≠ dist[{u}] + w({u},{v})"

    # 3) triangle inequality across all edges (no edge can improve settled dist)
    for u, nbrs in graph.items():
        for v, w in nbrs:
            if dist[u] + w < dist[v] - 1e-12:
                raise AssertionError(f"Triangle inequality violated via {u}->{v}: {dist[u]}+{w} < {dist[v]}")

    # 4) Cross-check with Bellman–Ford (V−1 relaxations)
    bf = bellman_ford_verify(graph, source)
    for v in graph:
        a, b = dist[v], bf[v]
        if a == float("inf") or b == float("inf"):
            assert a == b, f"Reachability mismatch for {v}"
        else:
            assert abs(a - b) < 1e-12, f"Mismatch at {v}: dijkstra={a}, bellman-ford={b}"

    # 5) Determinism: repeat run equals previous
    dist2, prev2 = dijkstra(graph, source)
    assert dist2 == dist and prev2 == prev, "Non-deterministic outputs on identical input."

    print("OK: non-negative weights, predecessor/tree invariants, edge-wise triangle check,")
    print("    Bellman–Ford agreement, and deterministic re-run.\n")

# ─────────────────────────────────────────────────────────────
# Demo
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    # Weighted undirected demo graph (edges listed in both directions)
    G: Graph = {
        'A': [('B', 7), ('C', 9), ('F', 14)],
        'B': [('A', 7), ('C', 10), ('D', 15)],
        'C': [('A', 9), ('B', 10), ('D', 11), ('F', 2)],
        'D': [('B', 15), ('C', 11), ('E', 6)],
        'E': [('D', 6), ('F', 9)],
        'F': [('A', 14), ('C', 2), ('E', 9)],
    }

    src = 'A'
    dist, prev = arc_answer(G, src)
    arc_reason()
    arc_check(G, src, dist, prev)

