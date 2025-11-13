#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
A small, self-contained “branch of insights” (in the spirit of
https://eyereasoner.github.io/eye/brains/) for **graph reachability**. It answers a
*typical* question on one concrete graph and explains the result in mathematical English.

Core idea (Hayes–Menzel)
------------------------
We model the graph’s **edge relation** as a *named* object (an intension, like "ex:edge").
Its **extension** is a set of pairs of nodes. We use a single fixed predicate:

    Holds₂(r, x, y)  ≡  the pair ⟨x,y⟩ is in the extension of relation-name r.

So we “quantify over” or refer to **relation names** (ordinary objects), not over predicates.
Reachability (reflexive–transitive closure) is computed via the monotone operator

    F(R) = Id ∪ E ∪ (R ∘ E)

and we find its least fixed point by **Kleene iteration** from ∅.

Typical question (what this program prints)
-------------------------------------------
Given the named graph ex:edge on nodes D = {A,B,C,D} with edges A→B, B→C, C→D:

1) Which nodes are reachable **from A**?
2) Is **D reachable from A**?

What the program prints
-----------------------
1) **Model**  — nodes, the named edge relation, and the operator F.
2) **Question** — the two items above.
3) **Answer** — the set ReachableFrom(A) and a Yes/No for “D reachable from A”.
4) **Reason why** — a short explanation: the Kleene chain and a path witness.
5) **Check (harness)** — 10 deterministic checks (fixed point, transitivity, etc.).

How to run
----------
    python3 holdsn_graph_reachability.py

No external dependencies; deterministic execution and output.
"""

from __future__ import annotations

from typing import Iterable, Tuple, Dict, Set, List, Optional
from collections import deque

# =========================================================
# Model: nodes D, relation names (intensions), Holds₂
# =========================================================

# Deterministic node order keeps printing stable
D: Tuple[str, ...] = ("A", "B", "C", "D")

# Relation-name namespace (URIs/strings as *intensions*)
EX = "ex:"

# Map each relation-name (intension) to its extension (a set of node pairs)
EXT2: Dict[str, Set[Tuple[str, str]]] = {}

def define_relation(name: str, pairs: Iterable[Tuple[str, str]]) -> str:
    """
    Register a named binary relation.
    Intension: `name` (a string).
    Extension: the set of ordered pairs provided.
    """
    EXT2[name] = {(a, b) for (a, b) in pairs}
    return name

def Holds2(rname: str, x: str, y: str) -> bool:
    """Holds₂(r, x, y) — ⟨x,y⟩ ∈ extension of the relation named by r."""
    return (x, y) in EXT2.get(rname, set())

# Identity relation on D (reflexive pairs)
ID: Set[Tuple[str, str]] = {(x, x) for x in D}

# One concrete graph: A→B→C→D (a simple line)
edge = define_relation(EX + "edge", [("A", "B"), ("B", "C"), ("C", "D")])

# =========================================================
# Reachability operator and fixed-point computation
# =========================================================

def rcomp(R: Set[Tuple[str, str]], E: Set[Tuple[str, str]]) -> Set[Tuple[str, str]]:
    """
    Relational composition: R ∘ E = { (x,z) | ∃y. (x,y)∈R and (y,z)∈E }.
    Deterministic iteration for stable output.
    """
    out: Set[Tuple[str, str]] = set()
    for (x, y1) in sorted(R):
        for (y2, z) in sorted(E):
            if y1 == y2:
                out.add((x, z))
    return out

def F(R: Set[Tuple[str, str]]) -> Set[Tuple[str, str]]:
    """Monotone operator for reflexive–transitive closure: F(R) = Id ∪ E ∪ (R ∘ E)."""
    return ID | set(EXT2[edge]) | rcomp(R, EXT2[edge])

def kleene_lfp(max_steps: int = 1 << len(D)) -> Tuple[Set[Tuple[str, str]], List[Set[Tuple[str, str]]]]:
    """
    Kleene iteration from ∅: R₀:=∅; R_{n+1}:=F(R_n).
    On a finite domain, this stabilizes. Return (LFP, full_chain).
    """
    chain: List[Set[Tuple[str, str]]] = [set()]
    R = set()
    for _ in range(max_steps):
        R_next = F(R)
        chain.append(R_next)
        if R_next == R:
            return R_next, chain
        R = R_next
    # Defensive (shouldn’t happen here)
    return R, chain

# =========================================================
# Typical queries and pretty-print helpers
# =========================================================

SOURCE: str = "A"
TARGET: str = "D"

def reachable_from(R: Set[Tuple[str, str]], src: str) -> Set[str]:
    """Return {y | (src,y) ∈ R} in deterministic order."""
    return {y for (x, y) in R if x == src}

def bfs_path(src: str, dst: str) -> Optional[List[str]]:
    """
    Shortest path witness using only the edge relation.
    Returns list like ['A','B','C','D'] or None if unreachable.
    """
    E = EXT2[edge]
    adj: Dict[str, List[str]] = {u: [] for u in D}
    for (a, b) in sorted(E):
        adj[a].append(b)
    for u in adj:
        adj[u].sort()
    q = deque([src])
    parent: Dict[str, Optional[str]] = {src: None}
    while q:
        u = q.popleft()
        if u == dst:
            # Reconstruct path
            path: List[str] = []
            v: Optional[str] = u
            while v is not None:
                path.append(v)
                v = parent[v]
            return list(reversed(path))
        for v in adj[u]:
            if v not in parent:
                parent[v] = u
                q.append(v)
    return None

def fmt_pairs(R: Iterable[Tuple[str, str]]) -> str:
    seq = list(sorted(R))
    return "∅" if not seq else "{" + ", ".join(f"⟨{a},{b}⟩" for (a, b) in seq) + "}"

def fmt_chain(chain: List[Set[Tuple[str, str]]], limit: int = 6) -> str:
    parts = [fmt_pairs(S) for S in chain[:limit]]
    if len(chain) > limit:
        parts.append("…")
    return " ⊆ ".join(parts)

def fmt_set(xs: Iterable[str]) -> str:
    seq = sorted(xs)
    return "∅" if not seq else "{" + ", ".join(seq) + "}"

# =========================================================
# The Branch: Model → Question → Answer → Reason why
# =========================================================

def print_model() -> None:
    print("Model")
    print("=====")
    print(f"Nodes D = {list(D)}")
    print()
    print("Signature")
    print("---------")
    print("• Holds₂(r, x, y): ⟨x,y⟩ ∈ extension of relation-name r (r is an *intension*).")
    print("• Named edge relation ex:edge with pairs", fmt_pairs(EXT2[edge]))
    print("• Operator F(R) = Id ∪ ex:edge ∪ (R ∘ ex:edge) (monotone).")
    print()

def print_question() -> None:
    print("Question")
    print("========")
    print(f"(1) Which nodes are reachable from {SOURCE}?")
    print(f"(2) Is {TARGET} reachable from {SOURCE}?")
    print()

def compute_answer():
    LFP, chain = kleene_lfp()
    from_source = reachable_from(LFP, SOURCE)
    is_target_reachable = (TARGET in from_source)
    return LFP, chain, from_source, is_target_reachable

def print_answer(LFP: Set[Tuple[str, str]], from_source: Set[str], is_target_reachable: bool) -> None:
    print("Answer")
    print("======")
    print(f"ReachableFrom({SOURCE}) = {fmt_set(from_source)}")
    print(f"{TARGET} reachable from {SOURCE}: {'Yes' if is_target_reachable else 'No'}")
    print()

def print_reason(LFP: Set[Tuple[str, str]], chain: List[Set[Tuple[str, str]]], is_target_reachable: bool) -> None:
    print("Reason why")
    print("==========")
    chain_txt = fmt_chain(chain, limit=6)
    print("Kleene iteration from ∅ with F(R) = Id ∪ E ∪ (R ∘ E) yields:")
    print(f"  {chain_txt}")
    print(f"which stabilizes at LFP = {fmt_pairs(LFP)}.")
    if is_target_reachable:
        path = bfs_path(SOURCE, TARGET)
        if path is not None:
            print(f"A path witness is: {SOURCE} → " + " → ".join(path[1:]) + ".")
        else:
            print("A path witness exists (by LFP), and the BFS would return one.")
    else:
        print(f"No path exists from {SOURCE} to {TARGET} in E; hence not in the closure.")
    print()

# =========================================================
# Check (harness): deterministic tests (≥ 10)
# =========================================================

class CheckFailure(AssertionError):
    pass

def check(cond: bool, msg: str) -> None:
    if not cond:
        raise CheckFailure(msg)

def run_checks(LFP: Set[Tuple[str, str]], chain: List[Set[Tuple[str, str]]], from_source: Set[str], is_target_reachable: bool) -> List[str]:
    notes: List[str] = []

    # 1) First Kleene step should add Id and edges
    check(chain[1] == ID | EXT2[edge], "First Kleene step must add Id ∪ edges.")
    notes.append("PASS 1: First Kleene step = Id ∪ edges.")

    # 2) Chain is ascending
    for i in range(len(chain) - 1):
        check(chain[i].issubset(chain[i+1]), "Kleene chain must be ascending.")
    notes.append("PASS 2: Kleene chain is ascending.")

    # 3) LFP is a fixed point
    check(F(LFP) == LFP, "LFP must satisfy F(LFP) = LFP.")
    notes.append("PASS 3: LFP is a fixed point of F.")

    # 4) LFP contains identity and all edges
    check(ID.issubset(LFP), "LFP must be reflexive (contain identity).")
    check(EXT2[edge].issubset(LFP), "LFP must contain all edges (be transitive closure over E with reflexive part).")
    notes.append("PASS 4: LFP contains Id and edges.")

    # 5) Transitivity of LFP (closure property)
    for (x, y1) in LFP:
        for (y2, z) in LFP:
            if y1 == y2:
                check((x, z) in LFP, "LFP must be transitive.")
    notes.append("PASS 5: LFP is transitive.")

    # 6) Expected reachability from A on this line graph
    check(from_source == {"A", "B", "C", "D"}, "ReachableFrom(A) must be {A,B,C,D}.")
    notes.append("PASS 6: ReachableFrom(A) = {A,B,C,D}.")

    # 7) D is reachable from A, and the BFS path is valid
    path = bfs_path(SOURCE, TARGET)
    check(is_target_reachable and path == ["A", "B", "C", "D"], "Expected path A→B→C→D.")
    # Validate each step is an edge
    for u, v in zip(path, path[1:]):
        check((u, v) in EXT2[edge], "BFS path must use actual edges.")
    notes.append("PASS 7: D is reachable from A with path A→B→C→D.")

    # 8) Some non-reachabilities (on the line graph)
    check(("D", "A") not in LFP and ("C", "A") not in LFP, "No backward reachability on a line.")
    notes.append("PASS 8: Non-reachabilities are correct for the line graph.")

    # 9) Deterministic pretty-print formatting
    s1 = fmt_pairs(LFP)
    s2 = fmt_pairs(set(sorted(LFP)))
    check(s1 == s2, "Pretty-printer must be deterministic.")
    notes.append("PASS 9: Deterministic formatting is stable.")

    # 10) Minimality w.r.t. first fixed point: LFP ⊆ any R with F(R) ⊆ R (checked on a small family)
    # We avoid full powerset; instead we verify against ∅, Id, Id∪E, and LFP itself.
    for R in [set(), ID, ID | EXT2[edge], LFP]:
        if F(R).issubset(R):
            check(LFP.issubset(R), "LFP must be subset of every pre-fixed relation (sampled family).")
    notes.append("PASS 10: LFP minimality holds on the sampled family (∅, Id, Id∪E, LFP).")

    return notes

# =========================================================
# Main orchestration
# =========================================================

def main() -> None:
    print_model()
    print_question()

    LFP, chain, from_source, is_target_reachable = compute_answer()
    print_answer(LFP, from_source, is_target_reachable)
    print_reason(LFP, chain, is_target_reachable)

    print("Check (harness)")
    print("===============")
    try:
        notes = run_checks(LFP, chain, from_source, is_target_reachable)
    except CheckFailure as e:
        print("FAIL:", e)
        raise
    else:
        for line in notes:
            print(line)

if __name__ == "__main__":
    main()

