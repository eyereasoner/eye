"""
beetle12.py – a minimal forward‑chaining reasoner **with proof trees**
=================================================================
This script materialises all logical consequences of Jos De Roo’s N3
"sequents" demo and **records every inference step** so that complete
proof trees can be displayed for any derived triple.

The N3 rules have been hard‑coded exactly as in the original example
(but feel free to extend them or load from a file).  Disjunctions written
`($ { … } { … } $)` are treated as *inclusive* – each branch is
independently asserted, which is sound because any particular branch is
already entailed by the rule.

Run it with plain `python beetle12.py` – you’ll see all `:is`
facts for `:beetle` **together with** the proof showing *why* each fact
holds.
"""
from __future__ import annotations
import argparse
from collections import defaultdict
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


# ---------------------------------------------------------------------------
# Data – unchanged logical rules
# ---------------------------------------------------------------------------
INITIAL_FACTS: List[Triple] = [
    ("beetle", "a", "Car"),
]

RULES_ORDER: List[Tuple[str, str]] = [
    ("a", "Car"),
    ("is", "green"),
    ("is", "pretty"),
    ("is", "nice"),
    ("is", "pretty1"),
    ("is", "pretty2"),
    ("is", "nice1"),
    ("is", "nice2"),
]

RULES: Dict[Tuple[str, str], List[Tuple[str, str, str]]] = {
    ("a", "Car"):       [("is", "green",  "car→green/blue"),
                          ("is", "blue",   "car→green/blue")],
    ("is", "green"):    [("is", "nice",   "green→nice/pretty"),
                          ("is", "pretty", "green→nice/pretty")],
    ("is", "pretty"):   [("is", "pretty1", "pretty→1/2"),
                          ("is", "pretty2", "pretty→1/2")],
    ("is", "nice"):     [("is", "nice1", "nice→1/2"),
                          ("is", "nice2", "nice→1/2")],
    ("is", "pretty1"): [("is", "pretty11", "pretty1→11/12"),
                          ("is", "pretty12", "pretty1→11/12")],
    ("is", "pretty2"): [("is", "pretty21", "pretty2→21/22"),
                          ("is", "pretty22", "pretty2→21/22")],
    ("is", "nice1"):   [("is", "nice11", "nice1→11/12"),
                          ("is", "nice12", "nice1→11/12")],
    ("is", "nice2"):   [("is", "nice21", "nice2→21/22"),
                          ("is", "nice22", "nice2→21/22")],
}

BEAUTIFUL_LEAVES: Set[str] = {
    "blue", "pretty11", "pretty12", "pretty21", "pretty22",
    "nice11", "nice12", "nice21", "nice22",
}

# ---------------------------------------------------------------------------
# Forward‑chaining with deterministic ordering
# ---------------------------------------------------------------------------

def forward_chain() -> tuple[Dict[Triple, List[Derivation]], List[Triple]]:
    facts: Dict[Triple, List[Derivation]] = {
        t: [Derivation("axiom", tuple())] for t in INITIAL_FACTS
    }

    agenda: List[Triple] = sorted(INITIAL_FACTS)  # queue to process

    while agenda:
        s, p, o = agenda.pop(0)  # FIFO for determinism
        # --- ordinary rules -------------------------------------------------
        for np, no, name in RULES.get((p, o), []):
            new = (s, np, no)
            if new not in facts:
                facts[new] = [Derivation(name, ((s, p, o),))]
                agenda.append(new)

        # --- leaf rule ------------------------------------------------------
        if p == "is" and o in BEAUTIFUL_LEAVES:
            new = (s, "is", "beautiful")
            if new not in facts:
                facts[new] = [Derivation("leaf→beautiful", ((s, p, o),))]
                agenda.append(new)

    # Return facts sorted lexicographically for stable output
    ordered = sorted(facts)
    return facts, ordered


# ---------------------------------------------------------------------------
# Proof printer – deterministic by sorted recursion
# ---------------------------------------------------------------------------

def print_proof(triple: Triple, db: Dict[Triple, List[Derivation]],
                indent: int = 0, seen: Set[Triple] | None = None):
    if seen is None:
        seen = set()
    lead = "  " * indent
    print(f"{lead}{triple[0]} {triple[1]} {triple[2]}.")

    if triple in seen:
        print(f"{lead}  (seen above)\n")
        return
    seen.add(triple)
    derivs = db[triple]
    deriv = derivs[0]  # first derivation only (deterministic)

    if deriv.rule == "axiom":
        print(f"{lead}  └─ AXIOM\n")
        return

    print(f"{lead}  └─[Rule: {deriv.rule}]")
    for prem in deriv.premises:
        print_proof(prem, db, indent + 2, seen)


# ---------------------------------------------------------------------------
# CLI & main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Deterministic proof generator")
    parser.add_argument("--query", nargs=3, metavar=("S", "P", "O"), help="single triple to prove")
    args = parser.parse_args()

    db, ordered = forward_chain()

    if args.query:
        q = tuple(args.query)
        if q in db:
            print("\nProof for", q, "\n")
            print_proof(q, db)
        else:
            print("Not entailed.")
    else:
        print("\nAll entailed :is facts\n---------------------")
        for t in ordered:
            if t[1] == "is":
                print_proof(t, db)


if __name__ == "__main__":
    main()
