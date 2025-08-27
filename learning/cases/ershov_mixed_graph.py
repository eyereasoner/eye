#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — graph theory edition (reachability in a fixed digraph)

What this program shows
-----------------------
Ershov’s *mixed computation* (partial evaluation) lets us pre-compute all work
that depends only on *static* inputs and generate a smaller, faster *residual
program* that awaits only the *dynamic* inputs.

Graph theory example (reachability):
  - Generic task: given a directed graph G = (V, E) and two vertices (u, v),
    decide whether there is a path u ->* v (reachability).
  - Mixed computation: if the graph G is *static* (known now), we can compress
    it to its strongly connected components (SCCs), build the DAG of SCCs, and
    precompute the *transitive closure* on that DAG. The residual program is
    then a tiny function `reachable(u, v)` that does only:
       s_u = scc[u]; s_v = scc[v]; return closure[s_u][s_v]
    — i.e., a couple of table lookups and a bit-test. No BFS/DFS at run time.

The script prints three sections:

1) "Answer" — the generated residual code and a couple of sample evaluations,
2) "Reason why" — a step-by-step mix-time explanation (SCCs, DAG, closure),
3) "Check (harness)" — verification that the residual function matches a
   generic BFS-based evaluator on many vertex pairs.

Run with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import Dict, List, Tuple, Callable
from collections import deque
import random


# ---------- A fixed directed graph for our demo (you can change this) ----------

def build_graph() -> Dict[str, List[str]]:
    """
    A small digraph with non-trivial SCC structure:

      A -> B -> C -> A        (SCC #0)
      C -> D                  (to SCC #1)
      D <-> E                 (SCC #1: D <-> E)
      E -> F, F <-> G         (SCC #2: F <-> G)
      G -> H                  (H alone, SCC #3)

    So the SCC-DAG is:  [A,B,C] -> [D,E] -> [F,G] -> [H]
    """
    G = {
        "A": ["B"],
        "B": ["C"],
        "C": ["A", "D"],
        "D": ["E"],
        "E": ["D", "F"],
        "F": ["G"],
        "G": ["F", "H"],
        "H": [],
    }
    return G


# ---------- Generic (unspecialized) reachability via BFS ----------

def reachable_generic(G: Dict[str, List[str]], u: str, v: str) -> bool:
    if u not in G or v not in G:
        raise ValueError("Unknown vertex")
    if u == v:
        return True
    q = deque([u])
    seen = {u}
    while q:
        x = q.popleft()
        for y in G[x]:
            if y == v:
                return True
            if y not in seen:
                seen.add(y)
                q.append(y)
    return False


# ---------- Specialization artefact ----------

@dataclass
class SpecializationResult:
    func: Callable[[str, str], bool]  # residual: (u, v) -> reachability
    source: str                       # generated Python source code
    trace: List[str]                  # human-readable mix-time reasoning


# ---------- The "mixer": compress to SCCs, precompute transitive closure ----------

def specialize_reachability(G: Dict[str, List[str]]) -> SpecializationResult:
    # Map vertex names <-> indices
    names = sorted(G.keys())
    idx: Dict[str, int] = {name: i for i, name in enumerate(names)}
    N = len(names)

    # Build adjacency by indices + transpose
    adj: List[List[int]] = [[] for _ in range(N)]
    radj: List[List[int]] = [[] for _ in range(N)]
    M = 0
    for u_name, nbrs in G.items():
        u = idx[u_name]
        for v_name in nbrs:
            v = idx[v_name]
            adj[u].append(v)
            radj[v].append(u)
            M += 1

    # Kosaraju's algorithm for SCCs
    seen = [False] * N
    order: List[int] = []

    def dfs1(u: int) -> None:
        seen[u] = True
        for v in adj[u]:
            if not seen[v]:
                dfs1(v)
        order.append(u)

    for u in range(N):
        if not seen[u]:
            dfs1(u)

    comp = [-1] * N
    comp_lists: List[List[int]] = []
    def dfs2(u: int, cid: int, bucket: List[int]) -> None:
        comp[u] = cid
        bucket.append(u)
        for v in radj[u]:
            if comp[v] == -1:
                dfs2(v, cid, bucket)

    cid = 0
    for u in reversed(order):
        if comp[u] == -1:
            bucket: List[int] = []
            dfs2(u, cid, bucket)
            comp_lists.append(bucket)
            cid += 1
    C = cid  # number of components

    # Build SCC-DAG adjacency and topological order
    cadj: List[List[int]] = [[] for _ in range(C)]
    edge_set = set()
    for u in range(N):
        cu = comp[u]
        for v in adj[u]:
            cv = comp[v]
            if cu != cv and (cu, cv) not in edge_set:
                edge_set.add((cu, cv))
                cadj[cu].append(cv)

    # Topological order of SCC-DAG (Kahn)
    indeg = [0] * C
    for u in range(C):
        for v in cadj[u]:
            indeg[v] += 1
    q = deque([u for u in range(C) if indeg[u] == 0])
    topo: List[int] = []
    while q:
        u = q.popleft()
        topo.append(u)
        for v in cadj[u]:
            indeg[v] -= 1
            if indeg[v] == 0:
                q.append(v)

    # Transitive closure on SCC-DAG using bitsets, processed in reverse topo
    closure = [0] * C  # bit i set means reach that component
    for u in reversed(topo):
        mask = 1 << u
        for v in cadj[u]:
            mask |= closure[v]
        closure[u] = mask

    # ---------- Emit residual code ----------
    # Pack literals
    name_to_id = idx
    scc_list = comp[:]            # length N, maps vertex index -> component id
    closure_list = closure[:]     # length C, each an int bitmask

    lines: List[str] = []
    lines.append("def reachable(u, v):")
    lines.append('    """Residual: reachability in a fixed directed graph G. Accepts vertex names or indices."""')
    lines.append(f"    N = {N}")
    lines.append(f"    NAME_TO_ID = {repr(name_to_id)}")
    lines.append(f"    SCC = {repr(scc_list)}          # vertex index -> component id")
    lines.append(f"    CLOSURE = {repr(closure_list)}  # component id -> bitmask of reachable components (incl. itself)")
    lines.append("")
    lines.append("    def to_index(x):")
    lines.append("        if isinstance(x, int):")
    lines.append("            i = x")
    lines.append("        else:")
    lines.append("            i = NAME_TO_ID.get(x)")
    lines.append("        if i is None or not (0 <= i < N):")
    lines.append("            raise ValueError('Unknown vertex: %r' % (x,))")
    lines.append("        return i")
    lines.append("")
    lines.append("    iu, iv = to_index(u), to_index(v)")
    lines.append("    su, sv = SCC[iu], SCC[iv]")
    lines.append("    return ((CLOSURE[su] >> sv) & 1) == 1")
    source = "\n".join(lines)

    # Materialize the residual function
    ns: Dict[str, object] = {}
    exec(source, ns)  # safe: locally generated from constants
    residual_func = ns["reachable"]

    # ---------- Build reasoning trace ----------
    trace: List[str] = []
    trace.append("We treat the graph G as static and the query (u,v) as dynamic.")
    trace.append(f"Vertices (N={N}): {names}")
    trace.append(f"Edges (M={M}): {sorted([(a,b) for a,bs in G.items() for b in bs])}")
    trace.append("At mix time we compute SCCs (each listed by vertex names):")
    for k, bucket in enumerate(comp_lists):
        trace.append(f"  SCC {k}: {[names[i] for i in bucket]}")
    dag_edges = sorted(list(edge_set))
    trace.append(f"SCC-DAG edges: {dag_edges}")
    trace.append(f"Topological order on SCC-DAG: {topo}")
    for u in range(C):
        reach_ids = [v for v in range(C) if (closure[u] >> v) & 1]
        trace.append(f"Closure[{u}] covers components {reach_ids}")
    trace.append("Residual test does: su=SCC[u], sv=SCC[v], then checks bit sv in CLOSURE[su].")

    return SpecializationResult(func=residual_func, source=source, trace=trace)


# ---------- Pretty-printers for the three sections ----------

def print_answer(spec: SpecializationResult, examples: List[Tuple[str, str]]) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    for (u, v) in examples:
        print(f"Example evaluation: reachable({u!r}, {v!r}) = {spec.func(u, v)}")
    print()

def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()

def print_check(spec: SpecializationResult, G: Dict[str, List[str]], trials: int = 600) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(20250827)
    names = list(G.keys())
    pairs: List[Tuple[str, str]] = []
    # Deterministic probes: a few obvious truths/falses
    pairs += [("A","C"), ("B","H"), ("F","G"), ("G","F"), ("H","A"), ("C","E"), ("E","H")]
    # Random probes
    for _ in range(trials):
        u = random.choice(names)
        v = random.choice(names)
        pairs.append((u, v))

    mismatches: List[Tuple[str, str, bool, bool]] = []
    for (u, v) in pairs:
        want = reachable_generic(G, u, v)
        got = spec.func(u, v)
        if want != got:
            mismatches.append((u, v, want, got))
            break

    if not mismatches:
        print(f"PASS: residual reachable matches generic BFS on {len(pairs)} (u,v) queries.")
    else:
        u, v, want, got = mismatches[0]
        print("FAIL: mismatch found.")
        print(f"  query: ({u!r}, {v!r})")
        print(f"  expected (generic) = {want}, got (residual) = {got}")
    print()


# ---------- Main demo ----------

def main() -> None:
    # Static graph
    G = build_graph()

    # Build residual program specialized to G
    spec = specialize_reachability(G)

    # 1) Show residual code and a few sample evaluations
    examples = [("A","H"), ("F","B"), ("G","H"), ("E","C"), ("H","H")]
    print_answer(spec, examples)

    # 2) Explain the mix-time decisions
    print_reason(spec)

    # 3) Verify equivalence vs. the generic implementation
    print_check(spec, G, trials=800)


if __name__ == "__main__":
    main()

