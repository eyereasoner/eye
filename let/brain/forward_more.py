#!/usr/bin/env python3
"""
forward_more.py – forward-chaining with full proof trace
"""

from typing import List, Set, Tuple, TypeVar, Dict

# ---------- FACTS ----------
facts = {
    (":A", ":score", 8),
    (":B", ":score", 5),
    (":B", ":moreInterestingThan", ":C"),
    (":D", ":hasAward", True),
    (":B", ":boring", True),
}

# ---------- HELPERS ----------
entities = sorted({s for s, _, _ in facts} |
                  {o for _, _, o in facts if isinstance(o, str)})

def score(e: str) -> int | None:
    return next((v for s, p, v in facts if s == e and p == ":score"), None)

def award(e: str) -> bool:
    return any(v is True for s, p, v in facts if s == e and p == ":hasAward")

def boring(e: str) -> bool:
    return any(v is True for s, p, v in facts if s == e and p == ":boring")

# ---------- DETERMINISTIC FORWARD CHAIN ----------
derived: Set[Tuple[str, str, str]] = set()
order: List[Tuple[str, str, str, str]] = []  # (a, b, rule, reason)

# 1. direct facts
for t in facts:
    if t[1] == ":moreInterestingThan":
        if t not in derived:
            derived.add(t)
            order.append((t[0], t[2], "fact", f"✓ fact {t}"))

# 2. score rule (lexicographic)
for a in entities:
    for b in entities:
        sa, sb = score(a), score(b)
        if sa is not None and sb is not None and sa > sb:
            triple = (a, ":moreInterestingThan", b)
            if triple not in derived:
                derived.add(triple)
                order.append((a, b, "score", f"✓ {a} > {b}  via score rule ({sa} > {sb})"))

# 3. award / boring rule (lexicographic)
for a in entities:
    for b in entities:
        if award(a) and boring(b):
            triple = (a, ":moreInterestingThan", b)
            if triple not in derived:
                derived.add(triple)
                order.append((a, b, "award", f"✓ {a} > {b}  via award/boring rule"))

# 4. transitive closure (fixed-point, lexicographic)
changed = True
while changed:
    changed = False
    for (a, _, b) in list(derived):
        for (b2, _, c) in list(derived):
            if b == b2:
                triple = (a, ":moreInterestingThan", c)
                if triple not in derived:
                    derived.add(triple)
                    order.append((a, c, "trans", f"✓ {a} > {c}  via transitivity ({a} > {b} and {b} > {c})"))
                    changed = True

# ---------- QUERY ANSWERER ----------
def prove(a: str, b: str) -> bool:
    return (a, ":moreInterestingThan", b) in derived

def run_all():
    # print derivations in fixed order
    for _, _, _, line in order:
        print(line)

    # queries
    queries = [(":A", ":B"), (":A", ":C"), (":D", ":B"), (":C", ":A")]
    for a, b in queries:
        print(f"\n=== Query ('{a}', ':moreInterestingThan', '{b}') ===")
        print("YES" if prove(a, b) else "NO")

    print("\n=== Who is more interesting than :C? ===")
    for e in entities:
        if prove(e, ":C"):
            print(e)

    print("\n=== All pairs ?X > ?Y ===")
    for x in entities:
        for y in entities:
            if prove(x, y):
                print(f"{x} > {y}")

if __name__ == "__main__":
    run_all()
