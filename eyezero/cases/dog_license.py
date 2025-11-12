#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
dog_licence.py
Backward-chaining proof for the “more than four dogs → dog licence” rule.

Rule
-----
    mustHave(Person, dogLicense) :-
        hasDog(Person, d1) … hasDog(Person, dN)  and  N > 4.

The built-in `count_dogs` checks the cardinality at proof time and
records the evidence (the dog's list) used in the explanation.

ARC sections:
- Answer      : who must have a dog license and the evidence list
- Reason why  : the rule explained and per-owner proof traces
- Check       : a harness that re-verifies counts, threshold (>4), and derived triples
"""

from collections import defaultdict
from itertools import count
from typing import Dict, List, Tuple, Set, Optional

Triple = Tuple[str, str, str]     # (subject, predicate, object)

# ─────────────────────────────────────────────────────────────
# 1.  Minimal triple store
# ─────────────────────────────────────────────────────────────
class Graph:
    def __init__(self) -> None:
        self._triples: Set[Triple] = set()

    def add(self, triple: Triple) -> None:
        self._triples.add(triple)

    def facts(self) -> Set[Triple]:
        return self._triples

    def dogs_of(self, person: str) -> List[str]:
        return [o for s, p, o in self._triples if s == person and p == "hasDog"]

g = Graph()

# facts: 5 dogs for Alice, 2 dogs for Bob
for i in range(1, 6):
    g.add(("alice", "hasDog", f"dog{i}"))
g.add(("bob", "hasDog", "dog6"))
g.add(("bob", "hasDog", "dog7"))

# ─────────────────────────────────────────────────────────────
# 2.  Rule template (one rule handled by a built-in)
# ─────────────────────────────────────────────────────────────
rule = dict(
    id="R-licence",
    head=("?P", "mustHave", "dogLicense"),
    body=[],                    # body handled by built-in
    builtin="count_dogs",
)

# ─────────────────────────────────────────────────────────────
# 3.  Unification helpers
# ─────────────────────────────────────────────────────────────
def is_var(t): 
    return isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ=None):
    """
    Tuple-wise unification with variables allowed on the pattern side.
    Flat triples here, but works recursively if needed.
    """
    θ = dict(θ or {})
    if isinstance(pat, tuple) != isinstance(fact, tuple):
        return None
    if not isinstance(pat, tuple):
        if is_var(pat):
            if pat in θ and θ[pat] not in (pat, fact):
                return None
            θ[pat] = fact
            return θ
        return θ if pat == fact else None
    if len(pat) != len(fact):
        return None
    for p_elem, f_elem in zip(pat, fact):
        θ = unify(p_elem, f_elem, θ)
        if θ is None:
            return None
    return θ

def subst(term, θ):
    if isinstance(term, tuple):
        return tuple(subst(x, θ) for x in term)
    return θ.get(term, term)

# ─────────────────────────────────────────────────────────────
# 4.  Built-in: dog count (records evidence instead of printing)
# ─────────────────────────────────────────────────────────────
licence_evidence: Dict[str, List[str]] = {}

def count_dogs(θ, trace: List[str]) -> Optional[Dict]:
    """
    Built-in called after the rule head unifies:
      success iff the person owns >4 dogs.
    On success, returns θ unchanged and records evidence; else returns None.
    """
    person = θ.get("?P")
    if person is None:
        return None
    dogs = g.dogs_of(person)
    if len(dogs) > 4:
        licence_evidence[person] = list(dogs)
        trace.append(f" ↪ {person} owns {len(dogs)} dogs "
                     f"({', '.join(sorted(dogs))}) > 4 ✓")
        return θ
    trace.append(f" ↪ {person} owns only {len(dogs)} dogs ✗")
    return None

# ─────────────────────────────────────────────────────────────
# 5.  Backward-chaining prover (per-owner, first solution)
# ─────────────────────────────────────────────────────────────
def bc(goal: Tuple, θ: Dict, depth: int, step=count(1), trace: Optional[List[str]] = None):
    """
    Prove a single goal:
      (a) try matching existing facts (none for mustHave initially)
      (b) try the single rule, then run the built-in count_dogs
    Yields at most one θ (first success), mirroring the original script.
    """
    tr = trace if trace is not None else []
    g_inst = subst(goal, θ)
    indent = "  " * depth
    tr.append(f"{indent}Step {next(step):02}: prove {g_inst}")

    # (a) existing facts
    for f in sorted(g.facts()):
        θ2 = unify(g_inst, f, θ)
        if θ2:
            tr.append(indent + f"✓ fact {f}")
            yield θ2
            return

    # (b) apply the single rule
    θh = unify(rule["head"], g_inst, θ)
    if θh is None:
        return
    tr.append(indent + f"→ via {rule['id']}")

    θb = count_dogs(θh, tr)
    if θb:
        yield θb

def prove_owner(owner: str):
    """
    Try to derive (owner, mustHave, dogLicense).
    Returns (derived_bool, trace_lines).
    """
    trace: List[str] = []
    goal = (owner, "mustHave", "dogLicense")
    derived = any(bc(goal, {}, 0, count(1), trace))
    return derived, trace

# ─────────────────────────────────────────────────────────────
# 6.  Run proofs for all owners (don’t mutate store until after proving)
# ─────────────────────────────────────────────────────────────
owners = sorted({s for s, p, _ in g.facts() if p == "hasDog"})
derived_facts: Set[Triple] = set()
owner_traces: Dict[str, List[str]] = {}

for owner in owners:
    ok, tr = prove_owner(owner)
    owner_traces[owner] = tr
    if ok:
        fact = (owner, "mustHave", "dogLicense")
        derived_facts.add(fact)
        g.add(fact)  # persist the new fact

# ─────────────────────────────────────────────────────────────
# 7.  ARC-style sections
# ─────────────────────────────────────────────────────────────

def arc_answer():
    print("Answer")
    print("------")
    must = sorted([s for s, p, o in derived_facts if p == "mustHave" and o == "dogLicense"])
    if must:
        print("License obligations derived for:")
        for person in must:
            dogs = sorted(licence_evidence.get(person, []))
            print(f"  {person}: {len(dogs)} dogs → mustHave dogLicense")
            if dogs:
                print(f"    evidence: {', '.join(dogs)}")
    else:
        print("No licence obligations derived.")
    print()

def arc_reason(max_lines_per_owner: int = 12):
    print("Reason why")
    print("----------")
    print("We use a single rule with a built-in check:")
    print("  R-licence:")
    print("    mustHave(?P, dogLicense)  :-  count_dogs(?P) and  count > 4.")
    print("The built-in counts hasDog facts for ?P at proof time.")
    print()
    for owner in owners:
        print(f"[Trace for {owner}]")
        lines = owner_traces.get(owner, [])
        head = lines[:max_lines_per_owner]
        for line in head:
            print(line)
        if len(lines) > max_lines_per_owner:
            print(f"  … {len(lines)-max_lines_per_owner} more steps …")
        print()

def check_harness():
    """
    Verifications:
      1) For every derived (P mustHave dogLicense), g.dogs_of(P) > 4.
      2) For every non-derived owner, g.dogs_of(P) ≤ 4.
      3) Derived triples are stored in the graph.
      4) Re-running the proof yields the same outcome.
    """
    # 1 & 2
    derived_people = {s for s, p, o in derived_facts}
    for owner in owners:
        n = len(g.dogs_of(owner))
        if owner in derived_people:
            assert n > 4, f"{owner} derived but has only {n} dogs."
        else:
            assert n <= 4, f"{owner} not derived but has {n} dogs."

    # 3) Persistence in graph
    for fact in derived_facts:
        assert fact in g.facts(), "Derived fact not persisted."

    # 4) Idempotence: proving again doesn’t change outcomes
    again = set()
    for owner in owners:
        ok, _ = prove_owner(owner)
        if ok:
            again.add((owner, "mustHave", "dogLicense"))
    assert again == derived_facts, "Re-run produced different results."

def arc_check():
    print("Check (harness)")
    print("---------------")
    try:
        check_harness()
        print("OK: counts and threshold verified; derived triples persisted; re-run consistent.")
    except AssertionError as e:
        print("FAILED:", e)
        raise

def dump_triples():
    print("\nTriples (sorted)")
    print("----------------")
    for s, p, o in sorted(g.facts()):
        print(f"{s}  {p}  {o} .")

def main():
    arc_answer()
    arc_reason()
    arc_check()
    dump_triples()

if __name__ == "__main__":
    main()

