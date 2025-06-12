"""
beetle6.py – RDF‑Surfaces tableau reasoner with **skeptical semantics**
======================================================================

This is a *toy* implementation that follows the logical reading of Pat Hayes’s
“RDF Surfaces” (a.k.a. Notation3 Negative Surface) rules used in the example.
The script:

1.  Starts with the single ground fact that **beetle is a Car**.
2.  Expands the knowledge base *non‑deterministically* wherever a rule
    generates a disjunction – each option spawns a *branch* (world).
3.  Propagates deterministic consequences within every branch.
4.  Prunes branches that become **inconsistent** (a car that is also
    beautiful violates the “cars are not beautiful” surface rule).
5.  When all branches are closed, computes the **intersection** of their
   facts – whatever remains true in **every** consistent world is
   *entailed* by the theory.
6.  Prints whether “beetle is nice” is entailed and, if so, shows a proof
   tree from one surviving branch.

Because the example is tiny, the tableau never grows beyond four
branches, so brute‑force is perfectly fine.

This version guarantees that only facts true in **all** consistent
branches are reported as entailed, mirroring the skeptical behaviour you
requested (identical to the updated *sequents_reasoner.py*).

Key changes
-----------
* **Deterministic branching:** branches are explored in a stable, sorted
  order so the proof output is reproducible.
* **Skeptical intersection:** after the tableau finishes we intersect
  the fact sets of *all* consistent worlds.
* **Query CLI:** run `python beetle6.py --query beetle is nice` to check
  a specific triple, or omit `--query` to list everything entailed.
"""
from __future__ import annotations
import argparse
from dataclasses import dataclass, field
from typing import Dict, List, Set, Tuple

# ---------------------------------------------------------------------------
# Basic structures
# ---------------------------------------------------------------------------
Triple = Tuple[str, str, str]  # (subject, predicate, object)

@dataclass(frozen=True)
class Derivation:
    rule: str
    premises: Tuple[Triple, ...]

@dataclass
class World:
    """One tableau branch (its facts + proofs)."""

    facts: Set[Triple] = field(default_factory=set)
    proofs: Dict[Triple, Derivation] = field(default_factory=dict)
    branched_on: Set[Triple] = field(default_factory=set)
    inconsistent: bool = False

    def clone(self) -> "World":
        return World(
            facts=set(self.facts),
            proofs=dict(self.proofs),
            branched_on=set(self.branched_on),
            inconsistent=self.inconsistent,
        )

# ---------------------------------------------------------------------------
# Rules
# ---------------------------------------------------------------------------
DETERMINISTIC_RULES: Dict[Tuple[str, str], Tuple[str, str, str]] = {
    ("is", "pretty"): ("is", "beautiful", "pretty→beautiful"),
    ("is", "blue"):   ("is", "beautiful", "blue→beautiful"),
}

DISJUNCTIVE_RULES: Dict[Tuple[str, str], List[Tuple[str, str, str]]] = {
    ("a", "Car"):      [("is", "green",  "car→green"),
                         ("is", "blue",   "car→blue")],
    ("is", "green"):   [("is", "nice",  "green→nice"),
                         ("is", "pretty", "green→pretty")],
}

# Contradiction pattern: ?S a Car.  &  ?S is beautiful.  ⇒  ⊥
CONTRADICTION_PAIRS: List[Tuple[Triple, Triple]] = [
    (("?S", "a", "Car"), ("?S", "is", "beautiful")),
]

# ---------------------------------------------------------------------------
# Helper – pattern matching utilities
# ---------------------------------------------------------------------------

def match_pattern(pattern: Triple, fact: Triple) -> Dict[str, str] | None:
    subs: Dict[str, str] = {}
    for p, f in zip(pattern, fact):
        if p.startswith("?"):
            if p in subs:
                if subs[p] != f:
                    return None
            else:
                subs[p] = f
        elif p != f:
            return None
    return subs


def instantiate(pattern: Triple, subs: Dict[str, str]) -> Triple:
    s, p, o = pattern
    return (subs.get(s, s), subs.get(p, p), subs.get(o, o))

# ---------------------------------------------------------------------------
# Deterministic propagation and contradiction check
# ---------------------------------------------------------------------------

def propagate(world: World) -> None:
    """Add deterministic consequences until fix‑point and mark contradictions."""
    changed = True
    while changed and not world.inconsistent:
        changed = False
        for s, p, o in sorted(list(world.facts)):
            key = (p, o)
            if key in DETERMINISTIC_RULES:
                np, no, name = DETERMINISTIC_RULES[key]
                new = (s, np, no)
                if new not in world.facts:
                    world.facts.add(new)
                    world.proofs[new] = Derivation(name, ((s, p, o),))
                    changed = True

        # check contradictions
        for patt1, patt2 in CONTRADICTION_PAIRS:
            for t1 in world.facts:
                subs = match_pattern(patt1, t1)
                if subs is None:
                    continue
                t2 = instantiate(patt2, subs)
                if t2 in world.facts:
                    world.inconsistent = True
                    return

# ---------------------------------------------------------------------------
# Branch expansion (first unapplied disjunction – deterministic order)
# ---------------------------------------------------------------------------

def expand(world: World) -> List[World]:
    for triple in sorted(world.facts):
        if triple in world.branched_on:
            continue
        key = (triple[1], triple[2])
        if key in DISJUNCTIVE_RULES:
            children: List[World] = []
            for np, no, name in DISJUNCTIVE_RULES[key]:
                child = world.clone()
                new = (triple[0], np, no)
                child.facts.add(new)
                child.proofs[new] = Derivation(name, (triple,))
                child.branched_on.add(triple)
                propagate(child)
                children.append(child)
            return children
    return []  # no disjunction available

# ---------------------------------------------------------------------------
# Tableau driver – returns **all** consistent finished worlds
# ---------------------------------------------------------------------------

def tableau(root: World) -> List[World]:
    queue: List[World] = [root]
    finals: List[World] = []

    while queue:
        w = queue.pop(0)  # FIFO for deterministic order
        if w.inconsistent:
            continue
        kids = expand(w)
        if kids:
            queue.extend(kids)
        else:
            finals.append(w)
    return finals

# ---------------------------------------------------------------------------
# Proof pretty‑printer (from first world – they all support entailed facts)
# ---------------------------------------------------------------------------

def print_proof(triple: Triple, proofs: Dict[Triple, Derivation],
                indent: int = 0, seen: Set[Triple] | None = None):
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
        print_proof(prem, proofs, indent + 2, seen)

# ---------------------------------------------------------------------------
# Main entry‑point
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser("beetle6 – skeptical RDF‑Surfaces reasoner")
    parser.add_argument("--query", nargs=3, metavar=("S", "P", "O"), help="triple to test")
    args = parser.parse_args()

    root = World()
    axiom = ("beetle", "a", "Car")
    root.facts.add(axiom)
    root.proofs[axiom] = Derivation("axiom", tuple())

    propagate(root)
    finals = tableau(root)

    if not finals:
        print("Theory inconsistent – no models.")
        return

    common: Set[Triple] = set.intersection(*(w.facts for w in finals))

    if args.query:
        q = tuple(args.query)
        if q in common:
            print("\nEntailed – proof (one branch):\n")
            print_proof(q, finals[0].proofs)
        else:
            print("Not entailed under skeptical semantics.")
    else:
        print(f"\n{len(finals)} consistent world(s).  Skeptical intersection facts:\n")
        for t in sorted(common):
            print_proof(t, finals[0].proofs)


if __name__ == "__main__":
    main()

