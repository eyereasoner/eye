"""
beetle6.py – tiny tableau‑style reasoner for the RDF‑Surfaces example
===================================================================

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
"""
from __future__ import annotations
from dataclasses import dataclass, field
from typing import Dict, List, Set, Tuple
import itertools

# ---------------------------------------------------------------------------
# Basic structures
# ---------------------------------------------------------------------------
Triple = Tuple[str, str, str]  # (subject, predicate, object)

@dataclass
class Derivation:
    """One inference step that produced *fact* inside a branch."""

    rule: str
    premises: List[Triple]


@dataclass
class World:
    """A single tableau branch (set of ground facts plus proof info)."""

    facts: Set[Triple] = field(default_factory=set)
    proofs: Dict[Triple, Derivation] = field(default_factory=dict)
    applied_det: Set[Triple] = field(default_factory=set)      # deterministic
    applied_disj: Set[Triple] = field(default_factory=set)     # disjunctions
    inconsistent: bool = False

    def clone(self) -> "World":
        """Deep‑copy world so that child branch inherits everything."""
        return World(
            facts=set(self.facts),
            proofs=dict(self.proofs),
            applied_det=set(self.applied_det),
            applied_disj=set(self.applied_disj),
            inconsistent=self.inconsistent,
        )


# ---------------------------------------------------------------------------
# Rules encoded as Python data
# ---------------------------------------------------------------------------
# Deterministic: (trigger_pred, trigger_obj)  →  (new_pred, new_obj, name)
DETERMINISTIC_RULES: Dict[Tuple[str, str], Tuple[str, str, str]] = {
    ("is", "pretty"): ("is", "beautiful", "pretty→beautiful"),
    ("is", "blue"):   ("is", "beautiful", "blue→beautiful"),
}

# Disjunctive:  (trigger_pred, trigger_obj)  →  [branches]
# each branch = (new_pred, new_obj, name)
DISJUNCTIVE_RULES: Dict[Tuple[str, str], List[Tuple[str, str, str]]] = {
    ("a",  "Car"):    [("is", "green",  "car→green"),
                        ("is", "blue",   "car→blue")],

    ("is", "green"): [("is", "nice",   "green→nice"),
                        ("is", "pretty", "green→pretty")],
}

# Contradiction: both triples present → ⊥
CONTRADICTION_PAIRS: List[Tuple[Triple, Triple]] = [
    # pattern placeholders – the "?S" will be matched against *same* subject
    (("?S", "a", "Car"), ("?S", "is", "beautiful")),
]


# ---------------------------------------------------------------------------
# Helper matching utilities
# ---------------------------------------------------------------------------

def match_pattern(pattern: Triple, fact: Triple) -> Dict[str, str] | None:
    """Return substitution mapping for variables in *pattern* or None."""
    sub: Dict[str, str] = {}
    for p, f in zip(pattern, fact):
        if p.startswith("?"):
            var = p
            if var in sub:
                if sub[var] != f:
                    return None  # inconsistent binding
            else:
                sub[var] = f
        elif p != f:
            return None
    return sub


def instantiate(pattern: Triple, sub: Dict[str, str]) -> Triple:
    return tuple(sub.get(x, x) for x in pattern)  # type: ignore[arg-type]


# ---------------------------------------------------------------------------
# Reasoner
# ---------------------------------------------------------------------------

def process_deterministic(world: World) -> bool:
    """Add deterministic consequences until saturation.  Return *changed*."""
    changed = False
    while True:
        added = False
        for triple in list(world.facts):
            if triple in world.applied_det:
                continue
            key = (triple[1], triple[2])
            if key in DETERMINISTIC_RULES:
                np, no, name = DETERMINISTIC_RULES[key]
                new = (triple[0], np, no)
                if new not in world.facts:
                    world.facts.add(new)
                    world.proofs[new] = Derivation(name, [triple])
                    added = True
                world.applied_det.add(triple)
        if not added:
            break
        changed = True
    return changed


def check_contradictions(world: World) -> None:
    for patt1, patt2 in CONTRADICTION_PAIRS:
        for t1 in world.facts:
            sub = match_pattern(patt1, t1)
            if sub is None:
                continue
            t2 = instantiate(patt2, sub)
            if t2 in world.facts:
                world.inconsistent = True
                return


def expand_world(world: World) -> List[World]:
    """Return child worlds produced by FIRST unapplied disjunction (if any).
    If none, return empty list.
    """
    for triple in world.facts:
        if triple in world.applied_disj:
            continue
        key = (triple[1], triple[2])
        if key in DISJUNCTIVE_RULES:
            branches: List[World] = []
            for np, no, name in DISJUNCTIVE_RULES[key]:
                child = world.clone()
                new = (triple[0], np, no)
                child.facts.add(new)
                child.proofs[new] = Derivation(name, [triple])
                child.applied_disj.add(triple)
                branches.append(child)
            # mark the parent as branched so it will not appear again
            world.inconsistent = True  # effectively discard parent branch
            return branches
    return []  # no unapplied disjunctions


def tableau(start: World) -> List[World]:
    """Return **all consistent** saturated worlds reachable from *start*."""
    queue: List[World] = [start]
    finals: List[World] = []

    while queue:
        w = queue.pop()
        if w.inconsistent:
            continue

        changed = True
        while changed and not w.inconsistent:
            changed = process_deterministic(w)
            check_contradictions(w)

        if w.inconsistent:
            continue

        children = expand_world(w)
        if children:
            queue.extend(children)
        else:
            finals.append(w)  # no more branching; keep this world

    return finals


# ---------------------------------------------------------------------------
# Pretty‑print a proof tree for one world
# ---------------------------------------------------------------------------

def print_proof(triple: Triple, proofs: Dict[Triple, Derivation],
                indent: int = 0, seen: Set[Triple] | None = None) -> None:
    if seen is None:
        seen = set()
    lead = "  " * indent
    s, p, o = triple
    print(f"{lead}{s} {p} {o}.")
    if triple in seen:
        print(f"{lead}  (seen above)\n")
        return
    seen.add(triple)

    deriv = proofs.get(triple)
    if deriv is None:
        print(f"{lead}  (axiom)\n")
        return
    if deriv.rule == "axiom":
        print(f"{lead}  └─ AXIOM\n")
        return
    print(f"{lead}  └─[Rule: {deriv.rule}]")
    for prem in deriv.premises:
        print_proof(prem, proofs, indent + 2, seen)


# ---------------------------------------------------------------------------
# Main – build initial world, run tableau, answer query
# ---------------------------------------------------------------------------

def main() -> None:
    # build initial world with axiom
    initial = World()
    axiom = ("beetle", "a", "Car")
    initial.facts.add(axiom)
    initial.proofs[axiom] = Derivation("axiom", [])

    finals = tableau(initial)

    if not finals:
        print("No consistent world – theory is inconsistent.")
        return

    print(f"Found {len(finals)} consistent world(s).\n")

    # intersection of facts across all consistent worlds
    common: Set[Triple] = set.intersection(*(w.facts for w in finals))

    query = ("beetle", "is", "nice")
    entailed = query in common

    if entailed:
        print(":beetle :is :nice.  is **entailed**.\n")
        # Show proof from first world
        print("One proof:\n")
        print_proof(query, finals[0].proofs)
    else:
        print(":beetle :is :nice.  is **NOT** entailed.\n")


if __name__ == "__main__":
    main()
