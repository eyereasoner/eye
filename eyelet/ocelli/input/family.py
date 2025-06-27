"""
family.py — a self‑contained family tree implementation in Python with
a small family knowledge‑base (father, mother, child, brother, sister, uncle,
aunt, grandfather, grandmother, …).

It’s a very typical Python approach:

* Data lives in ordered dict / lists.
* Each relation (father, mother, sibling, uncle …) is a plain function that
  returns a *list* of strings.
* Every query runs deterministic loops over the facts.

Run the file directly to see demo output, or import the functions elsewhere.
"""
from __future__ import annotations

# ---------------------------------------------------------------------------
# Raw, ordered facts
# ---------------------------------------------------------------------------

FATHER: list[tuple[str, str]] = [
    ("john", "linda"),
    ("john", "robert"),
    ("robert", "james"),
    ("robert", "patricia"),
    ("michael", "barbara"),
]
MOTHER: list[tuple[str, str]] = [
    ("mary", "linda"),
    ("mary", "robert"),
    ("linda", "jane"),
    ("linda", "patrick"),
    ("barbara", "anne"),
]
MALES   = ["john", "robert", "james", "michael", "patrick"]
FEMALES = ["mary", "linda", "patricia", "barbara", "anne", "jane"]

# Helper indices for fast lookup ------------------------------------------------
_children_of: dict[str, list[str]] = {}
_parents_of : dict[str, list[str]] = {}

for p,c in FATHER+MOTHER:
    _children_of.setdefault(p, []).append(c)
    _parents_of.setdefault(c, []).append(p)

# ---------------------------------------------------------------------------
# Primitive relations (use only the raw facts / indexes)
# ---------------------------------------------------------------------------

def father(person: str) -> list[str]:
    return [p for p,c in FATHER if p == person] or []  # silly but keeps API symmetric

def fathers_of(child: str) -> list[str]:
    return [p for p,c in FATHER if c == child]

def mother(person: str) -> list[str]:
    return [p for p,c in MOTHER if p == person]

def mothers_of(child: str) -> list[str]:
    return [p for p,c in MOTHER if c == child]

def parents_of(child: str) -> list[str]:
    return fathers_of(child) + mothers_of(child)

def children_of(parent: str) -> list[str]:
    return _children_of.get(parent, [])

# Gender helpers --------------------------------------------------------------

def is_male(p: str)   -> bool: return p in MALES

def is_female(p: str) -> bool: return p in FEMALES

# ---------------------------------------------------------------------------
# Derived relations
# ---------------------------------------------------------------------------

def siblings(person: str) -> list[str]:
    sibs: list[str] = []
    for parent in _parents_of.get(person, []):
        sibs.extend([c for c in _children_of.get(parent, []) if c != person])
    # preserve order, remove dupes
    seen: set[str] = set(); ordered = []
    for x in sibs:
        if x not in seen:
            seen.add(x)
            ordered.append(x)
    return ordered

def brothers(person: str) -> list[str]:
    return [s for s in siblings(person) if is_male(s)]

def sisters(person: str) -> list[str]:
    return [s for s in siblings(person) if is_female(s)]

def grandparents(person: str) -> list[str]:
    gps: list[str] = []
    for parent in _parents_of.get(person, []):
        gps.extend(_parents_of.get(parent, []))
    # unique preservation
    seen: set[str] = set(); ordered=[]
    for g in gps:
        if g not in seen:
            seen.add(g); ordered.append(g)
    return ordered

def grandfathers(person: str) -> list[str]:
    return [g for g in grandparents(person) if is_male(g)]

def grandmothers(person: str) -> list[str]:
    return [g for g in grandparents(person) if is_female(g)]

def all_grandparents() -> list[str]:
    """Return every person who has at least one grand-child, preserving fact order."""
    ordered_parents = [p for p, _ in FATHER + MOTHER]          # order that parents appear
    gps = []
    for p in ordered_parents:
        if any(gp for gp in children_of(p) if children_of(gp)):  # p ➜ child ➜ grand-child
            if p not in gps:
                gps.append(p)
    return gps

def uncles(person: str) -> list[str]:
    uncs: list[str] = []
    for parent in _parents_of.get(person, []):
        uncs.extend(brothers(parent))
    # unique + order
    seen=set(); ordered=[]
    for u in uncs:
        if u not in seen:
            seen.add(u); ordered.append(u)
    return ordered

def aunts(person: str) -> list[str]:
    ant: list[str] = []
    for parent in _parents_of.get(person, []):
        ant.extend(sisters(parent))
    seen=set(); ordered=[]
    for a in ant:
        if a not in seen:
            seen.add(a); ordered.append(a)
    return ordered

# ---------------------------------------------------------------------------
# Demo
# ---------------------------------------------------------------------------
if __name__ == "__main__":
    print("Children of robert:", children_of("robert"))
    print("Grandparents of patricia:", grandparents("patricia"))
    print("All grandparents:", all_grandparents())
    print("Sisters of james:", sisters("james"))
    print("Uncles of james:", uncles("james"))

    print("Sibling pairs (male first):")
    for p in MALES:
        for sib in siblings(p):
            print("  ", (p, sib))

