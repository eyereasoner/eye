#!/usr/bin/env python3
"""
mmln_backward.py
──────────────────────────────────────────────────────────────
Generic backward-proof engine for the Greek-vase MMLN.
See https://github.com/eyereasoner/eye/tree/master/reasoning/mmln

• A *shape* has N weighted rules.
• Fired_weight = sum weights of satisfied rules.
• Inductivity  P = Fired / Total_for_shape.
• Backward proof prints every rule check; the goal succeeds
  when P > 0 (you can adopt a higher threshold if desired).

Three shapes are implemented as examples (Chalice, Neck-amphora,
Aryballos).  To add the rest, copy each rule list from the N3 into
`ShapeRules` exactly like the examples.
"""

from itertools import count
from typing import Dict, List, Tuple, Set

# ─────────────────────────────────────────────────────────────
# 0.  Fact base for ONE specimen (g1)
#     ↓ adapt or extend as needed
# ─────────────────────────────────────────────────────────────
facts: Set[Tuple[str, str, str]] = {
    ("g1", "Body",  "small"),
    ("g1", "Body",  "deep"),
    ("g1", "Handle","vertical"),
    ("g1", "Handle","horizontal"),
    ("g1", "Height","100"),      # mm
}

def has(s,p,o):   return (s,p,o) in facts
def height_between(lo,hi):
    h = int(next(o for s,p,o in facts if s=="g1" and p=="Height"))
    return lo <= h <= hi

# ─────────────────────────────────────────────────────────────
# 1.  Shape rule table  (shape → list of (label, weight, predicate))
#     Paste additional shapes here – no code change required.
# ─────────────────────────────────────────────────────────────
ShapeRules: Dict[str,List[Tuple[str,float, callable]]] = {

   "Chalice": [
        ("body bowl",           100, lambda: has("g1","Body","bowl")),
        ("body deep",           100, lambda: has("g1","Body","deep")),
        ("handle horizontal",   100, lambda: has("g1","Handle","horizontal")),
        ("handle up",           100, lambda: has("g1","Handle","extended_upward")),
        ("foot flaring",        100, lambda: has("g1","Foot","flaring")),
        ("height 150–200",      100, lambda: height_between(150,200)),
    ],

    "Neck_amphora": [
        ("body oval",           100, lambda: has("g1","Body","oval")),
        ("mouth thick",         100, lambda: has("g1","Mouth","thick")),
        ("handle vertical",     100, lambda: has("g1","Handle","vertical")),
        ("neck offset",         100, lambda: has("g1","Neck","offset")),
        ("foot heavy",          100, lambda: has("g1","Foot","heavy")),
        ("height >200",         100, lambda: int(next((o for s,p,o in facts if p=='Height'),"0"))>200),
    ],

    "Aryballos": [
        ("body round",          200, lambda: has("g1","Body","round")),
        ("mouth disk",          100, lambda: has("g1","Mouth","disk")),
        ("handle present",       50, lambda: any(has("g1","Handle",x) for x in ("vertical","horizontal","extended_upward"))),
        ("height 50–100",       100, lambda: height_between(50,100)),
    ],
}

total_weight = {sh: sum(w for _,w,_ in rules)
                for sh,rules in ShapeRules.items()}

# ─────────────────────────────────────────────────────────────
# 2.  Backward-proof engine
# ─────────────────────────────────────────────────────────────
step = count(1)

def prove_shape(shape: str) -> float:
    """Return probability P(shape|facts) with full proof trace."""
    indent = "  "
    fired = 0.0
    print(f"\n=== Proving shape {shape} for g1 ===")
    for label, wt, cond in ShapeRules[shape]:
        ok = cond()
        fired += wt if ok else 0
        mark = "✓" if ok else "✗"
        print(f"{indent}{mark} {label:<30}  w={wt}")
    total = total_weight[shape]
    prob = fired / total if fired else 0.0
    print(f"{indent}→ fired {fired} / {total}  =  {prob:.3f}")
    return prob

# ─────────────────────────────────────────────────────────────
# 3.  Run proof for every shape
# ─────────────────────────────────────────────────────────────
posterior: Dict[str,float] = {}
for shp in ShapeRules:
    posterior[shp] = prove_shape(shp)

print("\n=== Posterior probabilities ===")
for shp,p in sorted(posterior.items(), key=lambda kv: -kv[1]):
    print(f"{shp:<15}: {p:.3f}")

