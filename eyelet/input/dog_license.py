#!/usr/bin/env python3
"""
Dog-licence example

Facts
  alice  hasDog  dog1 … dog5
  bob    hasDog  dog6, dog7

Rule
  If a person has >4 dogs, assert:
      <person>  mustHave  dogLicense

The script prints all triples (lexicographically sorted) followed by a
human-readable justification for every licence triple it added.
"""

from collections import defaultdict
from typing import List, Set, Tuple, Dict

Triple = Tuple[str, str, str]          # (subject, predicate, object)


class Graph:
    """Ultra-minimal triple store."""
    def __init__(self) -> None:
        self._triples: Set[Triple] = set()

    # ── modification ────────────────────────────────────────────
    def add(self, triple: Triple) -> None:
        self._triples.add(triple)

    # ── queries ────────────────────────────────────────────────
    def subjects(self, pred: str) -> Set[str]:
        return {s for s, p, _ in self._triples if p == pred}

    def objects(self, subj: str, pred: str) -> List[str]:
        return [o for s, p, o in self._triples if s == subj and p == pred]

    # ── iteration / size ───────────────────────────────────────
    def __iter__(self):
        return iter(self._triples)

    def __len__(self):
        return len(self._triples)


# ──────────────────────────────────────────────────────────────
# 1.  Initialise facts
# ──────────────────────────────────────────────────────────────
g = Graph()

for i in range(1, 6):                       # alice → dog1 … dog5
    g.add(("alice", "hasDog", f"dog{i}"))

g.add(("bob", "hasDog", "dog6"))            # bob → dog6, dog7
g.add(("bob", "hasDog", "dog7"))

# ──────────────────────────────────────────────────────────────
# 2.  Apply the rule and collect proof info
# ──────────────────────────────────────────────────────────────
def add_licence_triples(graph: Graph) -> Dict[str, List[str]]:
    """
    Add (person, mustHave, dogLicense) when person owns >4 dogs.

    Returns
    -------
    dict
        person → list of that person’s dogs  (only for persons who
        triggered the rule).  Used later for proof explanations.
    """
    proofs: Dict[str, List[str]] = {}

    for person in graph.subjects("hasDog"):
        dogs = graph.objects(person, "hasDog")
        if len(dogs) > 4:
            graph.add((person, "mustHave", "dogLicense"))
            proofs[person] = dogs               # remember for output
    return proofs


proof_data = add_licence_triples(g)

# ──────────────────────────────────────────────────────────────
# 3.  Deterministic output
# ──────────────────────────────────────────────────────────────
print("----- Triples -----")
for s, p, o in sorted(g):
    print(f"{s}  {p}  {o} .")

# ──────────────────────────────────────────────────────────────
# 4.  Proof explanations
# ──────────────────────────────────────────────────────────────
if proof_data:
    print("\n----- Proof explanations -----")
    for person, dogs in proof_data.items():
        dog_list = ", ".join(dogs)
        print(
            f"{person} has {len(dogs)} dogs ({', '.join(sorted(dogs))}), which "
            f"is more than the limit of 4 → asserted ({person}, mustHave, dogLicense)."
        )
else:
    print("\n(no licence obligations derived)")

