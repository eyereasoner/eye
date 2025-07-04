"""
Path discovery
==============
See https://aws.amazon.com/blogs/database/build-and-deploy-knowledge-graphs-faster-with-rdf-and-opencypher/

Goal-driven route proofs — *memory‑friendly strategies*
Large, dense graphs can cause a breadth‑first search to consume too much memory.
This module introduces two strategies to tackle that:

1. **Strategy switch** — Choose between:
   * `bfs`   – Standard Breadth-First Search (shortest paths first, high RAM).
   * `iddfs` – Iterative Deepening DFS (lower memory, O(depth) usage).
2. **K-shortest loop-free paths** — `--ksp N` uses Yen’s algorithm to return the N
   shortest simple paths, avoiding the combinatorial explosion of “all paths”.

These features preserve the goal-driven nature of route proofs.
"""

from __future__ import annotations

import argparse
import heapq
import re
import sys
import time
from collections import defaultdict, deque
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from typing import Dict, List, Tuple, Generator, Sequence, Iterable, Deque, Optional, Set

# ---------------------------------------------------------------------------
# 0. Aliases & exceptions
# ---------------------------------------------------------------------------
EdgeList = Dict[str, List[str]]

class RouteError(RuntimeError):
    """Raised when the start or goal airport is missing or unreachable."""

class SearchAborted(RuntimeError):
    """Raised when search halts due to resource limits (routes, frontier, nodes)."""

# ---------------------------------------------------------------------------
# 1. Graph parser
# ---------------------------------------------------------------------------

_PATTERN = re.compile(
    r"airroutes_hasOutboundRouteTo\(\s*(airroutes_AIRPORT_\w+)\s*,\s*(airroutes_AIRPORT_\w+)\s*\)\."
)

def parse_routes(raw: str, *, bidirectional: bool = False) -> EdgeList:
    """
    Parse graph edges from string input.

    Args:
        raw: String containing route facts.
        bidirectional: If True, adds reverse edge for each route.

    Returns:
        A graph in adjacency list form.
    """
    graph: EdgeList = defaultdict(list)
    for a, b in _PATTERN.findall(raw):
        graph[a].append(b)
        if bidirectional:
            graph[b].append(a)
    return graph

# ---------------------------------------------------------------------------
# 2. Tiny utilities
# ---------------------------------------------------------------------------

def exists(graph: EdgeList, node: str) -> bool:
    """Check if a node exists in the graph (as source or target)."""
    return node in graph or any(node in outs for outs in graph.values())

# ---------------------------------------------------------------------------
# 3. Proof object
# ---------------------------------------------------------------------------
@dataclass(slots=True)
class Proof:
    """Logical derivation of a path from start to goal."""
    goal: str
    rule: str
    sub: Tuple["Proof", ...] = ()

    def pretty(self, lvl: int = 0) -> str:
        pad = "  " * lvl
        out = [pad + self.rule]
        out.extend(s.pretty(lvl + 1) for s in self.sub)
        return "\n".join(out)

    def compact(self) -> str:
        """Compact Prolog-like output."""
        edges: List[str] = []
        node = self
        while node.sub:
            nxt = node.sub[0]
            edges.append(f"airroutes_hasOutboundRouteTo({node.goal}, {nxt.goal})")
            node = nxt
        return f"route({self.goal}, {node.goal}) :- " + ", ".join(edges) + "."

# ---------------------------------------------------------------------------
# 4. Proof reconstruction helpers
# ---------------------------------------------------------------------------

_GOAL_FACT = "route({0}, {0}).    % goal reached"

def _edge_rule(a: str, b: str, g: str) -> str:
    """Return Prolog-style rule for an edge in the proof path."""
    return f"route({a}, {g}) :- airroutes_hasOutboundRouteTo({a}, {b}), route({b}, {g})."

@lru_cache(maxsize=None)
def _build_proof(path: Tuple[str, ...], goal: str) -> Proof:
    """Recursively build a proof object from a path."""
    a, *rest = path
    if not rest:
        return Proof(goal=a, rule=_GOAL_FACT.format(a))
    return Proof(goal=a, rule=_edge_rule(a, rest[0], goal), sub=(_build_proof(tuple(rest), goal),))

# ---------------------------------------------------------------------------
# 5. Enumeration strategies
# ---------------------------------------------------------------------------

# -- 5.1 Breadth-First Search --

def _bfs_paths(
    graph: EdgeList,
    start: str,
    goal: str,
    max_depth: Optional[int],
    max_routes: Optional[int],
    frontier_limit: Optional[int],
    max_nodes_expanded: Optional[int],
) -> Iterable[Tuple[str, ...]]:
    """
    Generate paths using BFS with optional limits.
    """
    q: Deque[Tuple[str, Tuple[str, ...]]] = deque([(start, (start,))])
    yielded = expanded = 0

    while q:
        if frontier_limit is not None and len(q) > frontier_limit:
            raise SearchAborted(f"frontier_limit={frontier_limit} exceeded")

        node, path = q.popleft()

        if node == goal:
            yield path
            yielded += 1
            if max_routes is not None and yielded >= max_routes:
                raise SearchAborted(f"max_routes={max_routes} reached")
            continue

        if max_depth is not None and len(path) > max_depth:
            continue

        for nxt in graph.get(node, []):
            if nxt in path:
                continue  # avoid cycles
            q.append((nxt, path + (nxt,)))

        expanded += 1
        if max_nodes_expanded is not None and expanded >= max_nodes_expanded:
            raise SearchAborted(f"max_nodes_expanded={max_nodes_expanded} reached")

# -- 5.2 Iterative Deepening Depth-First Search --

def _dls(graph: EdgeList, node: str, goal: str, depth: int, path: Tuple[str, ...], visited: Set[str]) -> Iterable[Tuple[str, ...]]:
    """Depth-limited DFS from current node."""
    if depth == 0:
        if node == goal:
            yield path
        return
    for nxt in graph.get(node, []):
        if nxt in visited:
            continue
        yield from _dls(graph, nxt, goal, depth - 1, path + (nxt,), visited | {nxt})

def _iddfs_paths(graph: EdgeList, start: str, goal: str, max_depth: int, max_routes: Optional[int]) -> Iterable[Tuple[str, ...]]:
    """Iteratively deepen DFS up to a maximum depth."""
    yielded = 0
    seen: Set[Tuple[str, ...]] = set()
    for d in range(max_depth + 1):
        for p in _dls(graph, start, goal, d, (start,), {start}):
            if p in seen:
                continue
            seen.add(p)
            yield p
            yielded += 1
            if max_routes is not None and yielded >= max_routes:
                raise SearchAborted(f"max_routes={max_routes} reached")

# -- 5.3 Yen’s K-Shortest Loop-Free Paths --

def _dijkstra(graph: EdgeList, s: str, t: str) -> Optional[List[str]]:
    """Find the shortest path using BFS (unit weights)."""
    q = deque([(s, [s])])
    seen = {s}
    while q:
        v, path = q.popleft()
        if v == t:
            return path
        for w in graph.get(v, []):
            if w not in seen:
                seen.add(w)
                q.append((w, path + [w]))
    return None

def _yen_ksp(graph: EdgeList, s: str, t: str, K: int) -> Iterable[List[str]]:
    """
    Generate K shortest loop-free paths using Yen's algorithm.
    """
    A: List[List[str]] = []
    B: List[Tuple[int, List[str]]] = []

    first = _dijkstra(graph, s, t)
    if not first:
        return
    A.append(first)
    yield first

    for k in range(1, K):
        prev = A[-1]
        for i in range(len(prev) - 1):
            spur_node = prev[i]
            root_path = prev[: i + 1]
            removed: List[Tuple[str, str]] = []

            # Temporarily remove edges that replicate existing paths
            for p in A:
                if p[: i + 1] == root_path and i + 1 < len(p):
                    a, b = p[i], p[i + 1]
                    if b in graph.get(a, []):
                        graph[a].remove(b)
                        removed.append((a, b))

            spur = _dijkstra(graph, spur_node, t)
            if spur:
                total = root_path[:-1] + spur
                heapq.heappush(B, (len(total), total))

            # Restore removed edges
            for a, b in removed:
                graph[a].append(b)

        if not B:
            break
        _, next_path = heapq.heappop(B)
        A.append(next_path)
        yield next_path

# ---------------------------------------------------------------------------
# 6. Public API
# ---------------------------------------------------------------------------

def prove(
    graph: EdgeList,
    start: str,
    goal: str,
    *,
    max_stopovers: int = 6,
    strategy: str = "bfs",  # bfs | iddfs | ksp[:N]
    max_routes: Optional[int] = None,
    frontier_limit: Optional[int] = None,
    max_nodes_expanded: Optional[int] = None,
) -> Generator[Tuple[Sequence[str], Proof], None, None]:
    """
    Generate proof paths from start to goal using the specified strategy.
    """
    if not exists(graph, start):
        raise RouteError(f"unknown start airport {start}")
    if not exists(graph, goal):
        raise RouteError(f"unknown goal airport {goal}")

    depth_cap = max_stopovers + 1

    try:
        if strategy == "bfs":
            paths = _bfs_paths(graph, start, goal, depth_cap, max_routes, frontier_limit, max_nodes_expanded)
        elif strategy == "iddfs":
            paths = _iddfs_paths(graph, start, goal, depth_cap, max_routes)
        elif strategy.startswith("ksp"):
            k = int(strategy.split(":", 1)[1]) if ":" in strategy else (max_routes or 10)
            paths = _yen_ksp(graph, start, goal, k)
        else:
            raise ValueError(f"Unknown strategy {strategy}")

        for p in paths:
            yield p, _build_proof(tuple(p), goal)
    finally:
        _build_proof.cache_clear()

# ---------------------------------------------------------------------------
# 7. CLI helper
# ---------------------------------------------------------------------------

def _cli() -> None:
    ap = argparse.ArgumentParser(description="Enumerate routes with goal-driven proofs.")
    ap.add_argument("--kg", type=Path, default="input/path_discovery.txt")
    ap.add_argument("--start", default="airroutes_AIRPORT_4011")
    ap.add_argument("--goal", default="airroutes_AIRPORT_421")
    ap.add_argument("-k", "--max-stopovers", type=int, default=2)
    ap.add_argument("-n", "--max-routes", type=int, default=1000)
    ap.add_argument("--frontier", type=int, default=100_000)
    ap.add_argument("--nodes", type=int, default=1_000_000)
    ap.add_argument("-s", "--strategy", default="iddfs",
                    help="bfs | iddfs | ksp[:N] (N = number of shortest paths)")
    ap.add_argument("-c", "--compact", action="store_true")
    args = ap.parse_args()

    G = parse_routes(args.kg.read_text())

    t0 = time.perf_counter()
    total_routes = 0
    stopovers_list = []

    try:
        for i, (pth, prf) in enumerate(
            prove(
                G,
                args.start,
                args.goal,
                max_stopovers=args.max_stopovers,
                strategy=args.strategy,
                max_routes=args.max_routes,
                frontier_limit=args.frontier,
                max_nodes_expanded=args.nodes,
            ),
            1,
        ):
            hops = len(pth) - 2 if len(pth) > 2 else 0
            stopovers_list.append(hops)
            total_routes += 1
            print(f"[{i}] {hops} stopovers:")
            print(prf.compact() if args.compact else prf.pretty(), "\n")
    except (SearchAborted, RouteError) as exc:
        print("⚠️", exc, file=sys.stderr)
    finally:
        elapsed = time.perf_counter() - t0
        print(f"\n{'=' * 40}")
        print("Search Summary:")
        print(f"Total routes found:     {total_routes}")
        if stopovers_list:
            print(f"Min stopovers:          {min(stopovers_list)}")
            print(f"Max stopovers:          {max(stopovers_list)}")
            avg = sum(stopovers_list) / len(stopovers_list)
            print(f"Average stopovers:      {avg:.2f}")
        print(f"Strategy used:          {args.strategy}")
        print(f"Elapsed time:           {elapsed:.2f} seconds")
        print(f"{'=' * 40}")

if __name__ == "__main__":
    _cli()

