"""
beetle12.py – skeptical & deterministic proof generator
=====================================================

This version implements **skeptical semantics** for Jos De Roo’s
sequent‑style rules: every disjunction spawns a *branch* and a fact is
entailed only if it is true in **all** finished branches (intersection).

With the canonical Beetle example that means ‑‑ as you observed ‑‑ the
*only* entailed `:is` property is **`:beautiful`**.

Run with plain `python beetle12.py` or add
`--query "beetle is beautiful"` to focus on one triple.
"""
from __future__ import annotations
import argparse
from dataclasses import dataclass, field
from typing import Dict, List, Set, Tuple

# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------
Triple = Tuple[str, str, str]

@dataclass(frozen=True)
class Derivation:
    rule: str
    premises: Tuple[Triple, ...]

@dataclass
class World:
    """A tableau branch with its own fact set and proofs."""

    facts: Set[Triple] = field(default_factory=set)
    proofs: Dict[Triple, Derivation] = field(default_factory=dict)
    branched_on: Set[Triple] = field(default_factory=set)  # disjunctions used

    def clone(self) -> "World":
        return World(
            facts=set(self.facts),
            proofs=dict(self.proofs),
            branched_on=set(self.branched_on),
        )


# ---------------------------------------------------------------------------
# Rules (identical logical content – now separated into deterministic vs
# disjunctive to enable branching)
# ---------------------------------------------------------------------------
DISJUNCTIVE_RULES: Dict[Tuple[str, str], List[Tuple[str, str, str]]] = {
    ("a", "Car"):       [("is", "green",  "car→green"),
                          ("is", "blue",   "car→blue")],

    ("is", "green"):    [("is", "nice",   "green→nice"),
                          ("is", "pretty", "green→pretty")],

    ("is", "pretty"):   [("is", "pretty1", "pretty→1"),
                          ("is", "pretty2", "pretty→2")],

    ("is", "nice"):     [("is", "nice1", "nice→1"),
                          ("is", "nice2", "nice→2")],

    ("is", "pretty1"): [("is", "pretty11", "pretty1→11"),
                          ("is", "pretty12", "pretty1→12")],

    ("is", "pretty2"): [("is", "pretty21", "pretty2→21"),
                          ("is", "pretty22", "pretty2→22")],

    ("is", "nice1"):   [("is", "nice11", "nice1→11"),
                          ("is", "nice12", "nice1→12")],

    ("is", "nice2"):   [("is", "nice21", "nice2→21"),
                          ("is", "nice22", "nice2→22")],
}

BEAUTIFUL_LEAVES: Set[str] = {
    "blue", "pretty11", "pretty12", "pretty21", "pretty22",
    "nice11", "nice12", "nice21", "nice22",
}

# ---------------------------------------------------------------------------
# Tableau engine
# ---------------------------------------------------------------------------

def apply_deterministic(w: World) -> None:
    """Add all deterministic consequences until saturation."""
    changed = True
    while changed:
        changed = False
        for s, p, o in sorted(w.facts):  # sorted for determinism
            if p == "is" and o in BEAUTIFUL_LEAVES:
                new = (s, "is", "beautiful")
                if new not in w.facts:
                    w.facts.add(new)
                    w.proofs[new] = Derivation("leaf→beautiful", ((s, p, o),))
                    changed = True


def branch_on_first_disjunction(w: World) -> List[World]:
    """Return child worlds produced by the first available disjunction.
    If none, returns empty list (world is finished).
    """
    for triple in sorted(w.facts):  # deterministic order
        if triple in w.branched_on:
            continue
        key = (triple[1], triple[2])
        if key in DISJUNCTIVE_RULES:
            children: List[World] = []
            for np, no, name in DISJUNCTIVE_RULES[key]:
                child = w.clone()
                new = (triple[0], np, no)
                child.facts.add(new)
                child.proofs[new] = Derivation(name, (triple,))
                child.branched_on.add(triple)
                apply_deterministic(child)
                children.append(child)
            # original branch will not be used further
            return children
    return []


def tableau(start: World) -> List[World]:
    queue: List[World] = [start]
    finals: List[World] = []
    while queue:
        w = queue.pop(0)  # FIFO for determinism
        children = branch_on_first_disjunction(w)
        if children:
            queue.extend(children)
        else:
            finals.append(w)
    return finals


# ---------------------------------------------------------------------------
# Proof printing (deterministic – follows pointers in first world)
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
# CLI & entry‑point
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Skeptical sequents reasoner")
    parser.add_argument("--query", nargs=3, metavar=("S", "P", "O"), help="single triple to prove")
    args = parser.parse_args()

    # initial world with axiom
    root = World()
    axiom = ("beetle", "a", "Car")
    root.facts.add(axiom)
    root.proofs[axiom] = Derivation("axiom", tuple())

    apply_deterministic(root)
    finals = tableau(root)

    # Skeptical intersection
    entailed: Set[Triple] = set.intersection(*(w.facts for w in finals))

    if args.query:
        q = tuple(args.query)
        if q in entailed:
            print("\nEntailed – proof:\n")
            print_proof(q, finals[0].proofs)
        else:
            print("Not entailed.")
    else:
        print("\nAll entailed :is facts (skeptical)\n----------------------------------")
        for t in sorted(entailed):
            if t[1] == "is":
                print_proof(t, finals[0].proofs)


if __name__ == "__main__":
    main()

