"""
beetle.py – minimal skeptical tableau for the Beetle disjunction‑elimination toy
=============================================================================

Logical content:

* Axiom:  `beetle a Car`.
* Disjunction: every car is **green _or_ blue**.
* Deterministic: green ⇒ beautiful, blue ⇒ beautiful.
* **No contradictions** – all branches are consistent.

Under skeptical semantics the only entailed fact is therefore
`beetle is beautiful` (because both branches make it true).

Run with:

```bash
python beetle.py             # show all entailed facts
python beetle.py --query beetle is beautiful
```
"""
from __future__ import annotations
import argparse
from dataclasses import dataclass, field
from typing import Dict, List, Set, Tuple

Triple = Tuple[str, str, str]

@dataclass(frozen=True)
class Derivation:
    rule: str
    premises: Tuple[Triple, ...]

@dataclass
class World:
    facts: Set[Triple] = field(default_factory=set)
    proofs: Dict[Triple, Derivation] = field(default_factory=dict)
    branched_on: Set[Triple] = field(default_factory=set)

    def clone(self) -> "World":
        return World(
            facts=set(self.facts),
            proofs=dict(self.proofs),
            branched_on=set(self.branched_on),
        )

# ----- rules ----------------------------------------------------------------
DETERMINISTIC_RULES: Dict[Tuple[str, str], Tuple[str, str, str]] = {
    ("is", "green"): ("is", "beautiful", "green→beautiful"),
    ("is", "blue"):  ("is", "beautiful", "blue→beautiful"),
}

DISJUNCTIVE_RULES: Dict[Tuple[str, str], List[Tuple[str, str, str]]] = {
    ("a", "Car"): [("is", "green", "car→green"),
                    ("is", "blue",  "car→blue")],
}

# No contradictions in this simple variant

# ----- deterministic propagation -------------------------------------------

def propagate(w: World) -> None:
    changed = True
    while changed:
        changed = False
        for s, p, o in sorted(list(w.facts)):
            key = (p, o)
            if key in DETERMINISTIC_RULES:
                np, no, name = DETERMINISTIC_RULES[key]
                new = (s, np, no)
                if new not in w.facts:
                    w.facts.add(new)
                    w.proofs[new] = Derivation(name, ((s, p, o),))
                    changed = True

# ----- branching ------------------------------------------------------------

def expand(w: World):
    for t in sorted(w.facts):
        if t in w.branched_on:
            continue
        key = (t[1], t[2])
        if key in DISJUNCTIVE_RULES:
            children = []
            for np, no, name in DISJUNCTIVE_RULES[key]:
                c = w.clone()
                new = (t[0], np, no)
                c.facts.add(new)
                c.proofs[new] = Derivation(name, (t,))
                c.branched_on.add(t)
                propagate(c)
                children.append(c)
            return children
    return []

# ----- tableau driver -------------------------------------------------------

def tableau(root: World):
    q = [root]
    finals = []
    while q:
        w = q.pop(0)
        kids = expand(w)
        if kids:
            q.extend(kids)
        else:
            finals.append(w)
    return finals

# ----- proof printer --------------------------------------------------------

def show(triple: Triple, proofs: Dict[Triple, Derivation], indent=0, seen=None):
    if seen is None:
        seen = set()
    lead = "  " * indent
    print(f"{lead}{triple[0]} {triple[1]} {triple[2]}.")
    if triple in seen:
        print(f"{lead}  (seen above)\n")
        return
    seen.add(triple)
    deriv = proofs.get(triple)
    if deriv is None or deriv.rule == "axiom":
        print(f"{lead}  └─ AXIOM\n")
        return
    print(f"{lead}  └─[Rule: {deriv.rule}]")
    for prem in deriv.premises:
        show(prem, proofs, indent + 2, seen)

# ----- main -----------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser("beetle_simple – skeptical demo")
    ap.add_argument("--query", nargs=3, metavar=("S", "P", "O"))
    args = ap.parse_args()

    root = World()
    axiom = ("beetle", "a", "Car")
    root.facts.add(axiom)
    root.proofs[axiom] = Derivation("axiom", tuple())
    propagate(root)

    finals = tableau(root)
    common = set.intersection(*(w.facts for w in finals))

    if args.query:
        q = tuple(args.query)
        if q in common:
            print("\nEntailed – proof:\n")
            show(q, finals[0].proofs)
        else:
            print("Not entailed skeptically.")
    else:
        print("\nSkeptically‑entailed facts:\n")
        for t in sorted(common):
            show(t, finals[0].proofs)

if __name__ == "__main__":
    main()

