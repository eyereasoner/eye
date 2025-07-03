#!/usr/bin/env python3
"""
dog_licence.py
Backward-chaining proof for the “more than four dogs → dog licence” rule.

Rule
-----
    mustHave(Person, dogLicense) :-
        hasDog(Person, d1) … hasDog(Person, dN)  and  N > 4.

The built-in `count_dogs` checks the cardinality at proof time and
prints the evidence (list of dogs).
"""

from collections import defaultdict
from itertools import count
from typing import Dict, List, Tuple, Set

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
# 2.  Rule template
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
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ=None):
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

subst = lambda t, θ: (
    tuple(subst(x, θ) for x in t) if isinstance(t, tuple) else θ.get(t, t)
)

# ─────────────────────────────────────────────────────────────
# 4.  Built-in: dog count
# ─────────────────────────────────────────────────────────────
licence_evidence: Dict[str, List[str]] = {}

def count_dogs(θ):
    person = θ.get("?P")
    if person is None:
        return None
    dogs = g.dogs_of(person)
    if len(dogs) > 4:
        licence_evidence[person] = dogs
        print(f"      ↪ {person} owns {len(dogs)} dogs "
              f"({', '.join(sorted(dogs))}) > 4 ✓")
        return θ
    print(f"      ↪ {person} owns only {len(dogs)} dogs ✗")
    return None

# ─────────────────────────────────────────────────────────────
# 5.  Backward-chaining prover
# ─────────────────────────────────────────────────────────────
def bc(goal: Tuple, θ: Dict, depth: int, step=count(1)):
    g_inst = subst(goal, θ)
    indent = "  " * depth
    print(f"{indent}Step {next(step):02}: prove {g_inst}")

    # (a) existing facts (graph currently has none for mustHave)
    for f in g.facts():
        θ2 = unify(g_inst, f, θ)
        if θ2:
            print(indent + f"✓ fact {f}")
            yield θ2
            return

    # (b) apply the single rule
    θh = unify(rule["head"], g_inst, θ)
    if θh is None:
        return
    print(indent + f"→ via {rule['id']}")

    # built-in performs dog counting & binds nothing else
    θb = count_dogs(θh)
    if θb:
        yield θb

# ─────────────────────────────────────────────────────────────
# 6.  Run proofs for all owners in the graph
# ─────────────────────────────────────────────────────────────
owners = sorted({s for s, p, _ in g.facts() if p == "hasDog"})
derived: Set[Triple] = set()

for owner in owners:
    goal = (owner, "mustHave", "dogLicense")
    print(f"\n=== Proving licence obligation for {owner} ===")
    if any(bc(goal, {}, 0)):
        derived.add(goal)
        g.add(goal)          # store new fact

# ─────────────────────────────────────────────────────────────
# 7.  Output triples and explanations
# ─────────────────────────────────────────────────────────────
print("\n----- Triples (sorted) -----")
for s, p, o in sorted(g.facts()):
    print(f"{s}  {p}  {o} .")

if licence_evidence:
    print("\n----- Proof explanations -----")
    for person, dogs in licence_evidence.items():
        print(f"{person} has {len(dogs)} dogs "
              f"({', '.join(sorted(dogs))}) → mustHave dogLicense.")
else:
    print("\n(no licence obligations derived)")

