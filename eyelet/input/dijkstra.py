#!/usr/bin/env python3
import heapq
from typing import Dict, List, Tuple, Hashable

Node   = Hashable
Weight = float
Graph  = Dict[Node, List[Tuple[Node, Weight]]]


def dijkstra(graph: Graph, source: Node):
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

    pq: List[Tuple[Weight, Node]] = [(0.0, source)]          # (d, v)
    while pq:
        d, u = heapq.heappop(pq)
        if d != dist[u]:                                     # stale entry
            continue
        for v, w in graph[u]:
            alt = d + w
            if alt < dist[v]:
                dist[v] = alt
                prev[v] = u
                heapq.heappush(pq, (alt, v))
    return dist, prev


if __name__ == "__main__":
    # Weighted undirected test graph
    G = {
        'A': [('B', 7), ('C', 9), ('F', 14)],
        'B': [('A', 7), ('C', 10), ('D', 15)],
        'C': [('A', 9), ('B', 10), ('D', 11), ('F', 2)],
        'D': [('B', 15), ('C', 11), ('E', 6)],
        'E': [('D', 6), ('F', 9)],
        'F': [('A', 14), ('C', 2), ('E', 9)],
    }

    dist, _ = dijkstra(G, 'A')
    print("Shortest distances from A:")
    for v in sorted(dist):
        print(f"  A → {v}: {dist[v]}")
