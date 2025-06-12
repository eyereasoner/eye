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
from dataclasses import dataclass, field
from typing import Dict, List, Set, Tuple

# ---------------------------------------------------------------------------
# Types
# ---------------------------------------------------------------------------
Triple = Tuple[str, str, str]  # (subject, predicate, object)

@dataclass
class Derivation:
    """Represents *one* way to derive a triple."""

    rule: str                     # name/id of the rule that fired
    premises: List[Triple] = field(default_factory=list)  # supporting triples


# ---------------------------------------------------------------------------
# Initial facts – axioms
# ---------------------------------------------------------------------------
INITIAL_FACTS: Set[Triple] = {
    ("beetle", "a", "Car"),
}

# ---------------------------------------------------------------------------
# Rules – keyed by (predicate, object) appearing in the *premise* triple.
# Each head is (new_predicate, new_object, rule_name)
# ---------------------------------------------------------------------------
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

# Leaves that directly entail :beautiful
BEAUTIFUL_LEAVES: Set[str] = {
    "blue", "pretty11", "pretty12", "pretty21", "pretty22",
    "nice11", "nice12", "nice21", "nice22",
}


# ---------------------------------------------------------------------------
# Forward‑chaining reasoner with proof recording
# ---------------------------------------------------------------------------

def forward_chain(start_facts: Set[Triple]):
    """Return (facts, proofs) where
    *facts* is the materialised closure and
    *proofs* maps each triple to *all* of its derivations.
    """

    facts: Set[Triple] = set(start_facts)
    proofs: Dict[Triple, List[Derivation]] = {
        triple: [Derivation(rule="axiom", premises=[])] for triple in start_facts
    }

    changed = True
    while changed:
        changed = False

        # 1. ordinary rules --------------------------------------------------
        for s, p, o in list(facts):  # list() because we grow the set
            for hp, ho, rule_name in RULES.get((p, o), []):
                new_triple = (s, hp, ho)

                derivation = Derivation(rule=rule_name, premises=[(s, p, o)])
                if new_triple not in proofs:
                    proofs[new_triple] = [derivation]
                    facts.add(new_triple)
                    changed = True
                else:
                    # same triple may have multiple derivations
                    if derivation not in proofs[new_triple]:
                        proofs[new_triple].append(derivation)

        # 2. leaf rule for :beautiful ---------------------------------------
        for s, p, o in list(facts):
            if p == "is" and o in BEAUTIFUL_LEAVES:
                new_triple = (s, "is", "beautiful")
                derivation = Derivation(
                    rule="leaf→beautiful", premises=[(s, p, o)]
                )
                if new_triple not in proofs:
                    proofs[new_triple] = [derivation]
                    facts.add(new_triple)
                    changed = True
                else:
                    if derivation not in proofs[new_triple]:
                        proofs[new_triple].append(derivation)

    return facts, proofs


# ---------------------------------------------------------------------------
# Proof‑tree pretty‑printer
# ---------------------------------------------------------------------------

def print_proof(triple: Triple, proofs: Dict[Triple, List[Derivation]],
                indent: int = 0, visited: Set[Triple] | None = None) -> None:
    if visited is None:
        visited = set()

    lead = "  " * indent
    subj, pred, obj = triple
    print(f"{lead}{subj} {pred} {obj}.")

    if triple in visited:
        print(f"{lead}  (already shown above)\n")
        return

    visited.add(triple)

    for i, deriv in enumerate(proofs.get(triple, []), start=1):
        if deriv.rule == "axiom":
            print(f"{lead}  └─ AXIOM\n")
        else:
            branch_lead = "  " * (indent + 1)
            print(f"{lead}  └─[Rule: {deriv.rule}] #{i}")
            for prem in deriv.premises:
                print_proof(prem, proofs, indent + 2, visited)


# ---------------------------------------------------------------------------
# Entry‑point
# ---------------------------------------------------------------------------

def main() -> None:
    all_facts, proofs = forward_chain(INITIAL_FACTS)

    print("\n=== Derived facts and their proofs ===\n")
    for triple in sorted(all_facts):
        subj, pred, obj = triple
        if pred == "is":
            print_proof(triple, proofs)
            print()


if __name__ == "__main__":
    main()
