#!/usr/bin/env python3
"""
forward_more.py – forward-chaining with full proof trace
"""

from collections import deque
from typing import List, Tuple, Deque, Set

# ---------- FACTS ----------
facts = [
    (":A", ":score", 8),
    (":B", ":score", 5),
    (":B", ":moreInterestingThan", ":C"),
    (":D", ":hasAward", True),
    (":B", ":boring", True),
]

entities = sorted({s for s, _, _ in facts} |
                  {o for _, _, o in facts if isinstance(o, str)})

# ---------- HELPERS ----------
def score(e: str) -> int | None:
    return next((v for s, p, v in facts if s == e and p == ":score"), None)

def award(e: str) -> bool:
    return any(v is True for s, p, v in facts if s == e and p == ":hasAward")

def boring(e: str) -> bool:
    return any(v is True for s, p, v in facts if s == e and p == ":boring")

# ---------- PRE-COMPUTE ORDERED DERIVATIONS ----------
lines: List[str] = []
derived: Set[Tuple[str, str]] = set()

# 1. Direct facts
for s, p, o in facts:
    if p == ":moreInterestingThan":
        derived.add((s, o))
        lines.append(f"✓ fact ({s}, :moreInterestingThan, {o})")

# 2. Score rule (lexicographic)
for a in entities:
    for b in entities:
        sa, sb = score(a), score(b)
        if sa is not None and sb is not None and sa > sb:
            if (a, b) not in derived:
                derived.add((a, b))
                lines.append(f"✓ {a} > {b}  via score rule ({sa} > {sb})")

# 3. Award / boring rule (lexicographic)
for a in entities:
    for b in entities:
        if award(a) and boring(b):
            if (a, b) not in derived:
                derived.add((a, b))
                lines.append(f"✓ {a} > {b}  via award/boring rule")

# 4. Transitive closure (FIFO, lexicographic)
queue: Deque[Tuple[str, str]] = deque(sorted(derived))
while queue:
    a, b = queue.popleft()
    for c in sorted(entities):
        if (b, c) in derived and (a, c) not in derived:
            derived.add((a, c))
            queue.append((a, c))
            lines.append(f"✓ {a} > {c}  via transitivity ({a} > {b} and {b} > {c})")

# ---------- OUTPUT ----------
for line in lines:
    print(line)

# ---------- QUERIES ----------
def prove(a: str, b: str) -> bool:
    return (a, b) in derived

queries = [(":A", ":B"), (":A", ":C"), (":D", ":B"), (":C", ":A")]
for a, b in queries:
    print(f"\n=== Query ({a}, :moreInterestingThan, {b}) ===")
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
