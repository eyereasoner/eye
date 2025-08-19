#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Non Sequitur — ARC (Answer / Reason / Check), self-contained

Idea (toy detector)
  An argument A commits a non sequitur if its conclusion does NOT follow
  from its premises, given a small background of relevance links and rules.

Encoding
  premise(A, atom)      — an atomic proposition present among the premises
  conclusion(A, atom)   — the argument’s (single) conclusion
  support(X, Y)         — topic-level relevance link “X can support Y”
  class_property(K, P)  — all K have property/topic P
  is_a(X, K)            — X is a K
  has(X, P)             — X has property/topic P   (used in conclusions)

Derivation (sound but very small)
  • Topic chaining: if premise topic T is present and support(T, …, Ctopic) via
    zero-or-more support edges, then ('topic', Ctopic) is entailed.
  • Class property: if is_a(X, K) and class_property(K, P) are both premises,
    then has(X, P) is entailed.

We flag NON SEQUITUR when the argument’s conclusion atom is not derivable.
"""

from collections import deque, defaultdict
from typing import Dict, List, Set, Tuple, Optional

Atom = Tuple[str, ...]  # e.g., ('topic','rain'), ('is_a','whale','mammal'), ('has','whale','warm')

# ─────────────────────────── Example arguments ────────────────────────────
sentences: Dict[str, str] = {
    "Arg1": "It rained yesterday. Therefore, the stock market will rise.",
    "Arg2": "Company profits increased; therefore its stock may rise.",
    "Arg3": "All mammals are warm-blooded. A whale is a mammal. Therefore, a whale is warm-blooded.",
    "Arg4": "We must reduce traffic. Therefore, we should ban books.",
}

premises: Dict[str, List[Atom]] = {
    "Arg1": [
        ('topic', 'rain'),
    ],
    "Arg2": [
        ('topic', 'profits_up'),
    ],
    "Arg3": [
        ('class_property', 'mammal', 'warm_blooded'),
        ('is_a', 'whale', 'mammal'),
    ],
    "Arg4": [
        ('topic', 'reduce_traffic'),
    ],
}

conclusions: Dict[str, Atom] = {
    "Arg1": ('topic', 'stock_up'),
    "Arg2": ('topic', 'stock_up'),
    "Arg3": ('has', 'whale', 'warm_blooded'),
    "Arg4": ('ban', 'books'),
}

# Background: topic-level relevance links (minuscule and hand-picked)
support_edges: Set[Tuple[str, str]] = {
    ('profits_up', 'stock_up'),
    # You can add more domain links if needed; we keep it tiny on purpose.
}

# Background: universal class property
class_props: Set[Tuple[str, str]] = {
    ('mammal', 'warm_blooded'),
}

# ─────────────────────────── Derivation engine ─────────────────────────────

class Derivation:
    """Records derived atoms and textual reasons."""
    def __init__(self):
        self.reasons: Dict[Atom, str] = {}

    def add(self, atom: Atom, reason: str):
        if atom not in self.reasons:
            self.reasons[atom] = reason
            return True
        return False

def derive(aid: str, trace: bool = False) -> Derivation:
    """
    Compute a small forward closure for the given argument:
      - Topic chaining via support edges (BFS).
      - Class-property instantiation to has(X,P).
    """
    D = Derivation()
    Q: deque[Tuple[str, Optional[str]]] = deque()  # (topic, came_from)

    # Seed with premises
    for atom in premises.get(aid, []):
        D.add(atom, "premise")
        if atom and atom[0] == 'topic':
            Q.append((atom[1], None))

    # Topic chaining (BFS over support graph)
    # We derive ('topic', t) for every t reachable from some seed topic.
    # Also keep parent pointers for readable reasons.
    parent: Dict[str, Optional[str]] = {}
    seen_topics: Set[str] = set()
    while Q:
        t, par = Q.popleft()
        if t in seen_topics:
            continue
        seen_topics.add(t)
        parent[t] = par
        D.add(('topic', t), f"topic reachable via support (seed/par: {par})")
        # expand
        for (u, v) in support_edges:
            if u == t and v not in seen_topics:
                Q.append((v, t))

    # Class-property instantiation
    # If both ('is_a', X, K) and ('class_property', K, P) are present, add ('has', X, P).
    ips = {(a[1], a[2]) for a in premises.get(aid, []) if a and a[0] == 'is_a'}
    cps = {(a[1], a[2]) for a in premises.get(aid, []) if a and a[0] == 'class_property'}
    for (X, K) in ips:
        for (K2, P) in cps:
            if K == K2:
                D.add(('has', X, P), f"from is_a({X},{K}) and class_property({K},{P})")

    # Make class_property itself available as background (not strictly needed;
    # included for completeness/trace symmetry).
    for (K, P) in class_props:
        D.add(('class_property', K, P), "background")

    return D

def entails(aid: str, goal: Atom, trace: bool = False) -> Tuple[bool, List[str]]:
    D = derive(aid, trace=trace)
    ok = goal in D.reasons
    reasons: List[str] = []
    if ok:
        reasons.append(D.reasons[goal])
    return ok, reasons

def is_non_sequitur(aid: str) -> Tuple[bool, List[str]]:
    goal = conclusions[aid]
    ok, reasons = entails(aid, goal)
    return (not ok), ([] if ok else ["no derivation from premises to conclusion under background links"])

# ────────────────────────────────── ARC: Answer ─────────────────────────────
def print_answer() -> None:
    print("Answer")
    print("======")
    results: Dict[str, bool] = {}

    for aid, text in sentences.items():
        print(f"\n=== {aid}: {text}")
        print("Premises:")
        for p in premises[aid]:
            print("  •", p)
        print("Conclusion:")
        print("  →", conclusions[aid])

        ok, reasons = entails(aid, conclusions[aid])
        if ok:
            print("Result: follows (no non sequitur)")
            for r in reasons:
                print("  reason:", r)
            results[aid] = False
        else:
            print("Result: NON SEQUITUR")
            results[aid] = True

    print("\nSummary")
    for aid in sorted(sentences):
        print(f"  {aid}: {'non sequitur' if results[aid] else 'ok'}")

# ───────────────────────────────── ARC: Reason why ──────────────────────────
def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We derive conclusions using two small, transparent mechanisms:")
    print("  • Topic chaining over a tiny relevance graph support(T,U).")
    print("    If a premise provides topic T and T→…→C holds in that graph,")
    print("    then ('topic', C) follows.")
    print("  • Class property instantiation: from is_a(X,K) and class_property(K,P),")
    print("    we infer has(X,P).")
    print("If the argument’s conclusion is not derivable by those routes, we flag a non sequitur.")

# ─────────────────────────────── ARC: Check (harness) ───────────────────────
def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Expected classifications
    expected = {"Arg1": True, "Arg2": False, "Arg3": False, "Arg4": True}
    ok_cls = True
    for aid, want in expected.items():
        got, _ = is_non_sequitur(aid)
        if got != want:
            ok_cls = False
            print(f"  MISMATCH {aid}: got {got}, want {want}")
    print(f"Expected classifications hold? {ok_cls}")
    ok_all &= ok_cls

    # 2) Soundness: add the missing support edge to fix Arg1; it should then follow
    support_edges.add(('rain', 'stock_up'))
    fixed_now, _ = is_non_sequitur("Arg1")
    print(f"Arg1 stops being a non sequitur after adding support(rain, stock_up)? {not fixed_now}")
    ok_all &= (not fixed_now)
    support_edges.remove(('rain', 'stock_up'))

    # 3) Soundness: remove class_property from Arg3 premises; it should become non sequitur
    saved = list(premises["Arg3"])
    premises["Arg3"] = [p for p in premises["Arg3"] if p[0] != 'class_property']
    became_ns, _ = is_non_sequitur("Arg3")
    print(f"Arg3 becomes a non sequitur if class_property is removed? {became_ns}")
    ok_all &= became_ns
    premises["Arg3"] = saved

    # 4) Determinism/idempotence
    a = is_non_sequitur("Arg2")
    b = is_non_sequitur("Arg2")
    print(f"Deterministic (same inputs ⇒ same result)? {a == b}")
    ok_all &= (a == b)

    print(f"\nAll checks passed? {ok_all}")

# ─────────────────────────────────── Main ───────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

