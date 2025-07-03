#!/usr/bin/env python3
"""
control_backward.py
Backward-chaining proof for Jos De Roo’s “Control System” demo.
"""

import math
from itertools import count
from typing import List, Tuple, Optional, Dict

# ───────────────────────────────────────────────────────────────
# 1 ▸  FACTS  (verbatim from original Python)
# ───────────────────────────────────────────────────────────────
measurement1 = {
    "input1":       [6, 11],
    "disturbance2": [45, 39],
}
measurement2 = {"input2": True}
measurement3 = {"input3": 56967, "disturbance1": 35766}
measurement4 = {"output2": 24}

observation1 = {"state1": 80}
observation2 = {"state2": False}
observation3 = {"state3": 22}

target2 = {"output2": 29}

# ───────────────────────────────────────────────────────────────
# 2 ▸  Helper  measurement10/2  (two-clause rule)
# ───────────────────────────────────────────────────────────────
def measurement10(I: str) -> Optional[float]:
    pair = measurement1.get(I)
    if pair is None:
        return None
    m1, m2 = pair
    return math.sqrt(m2 - m1) if m1 < m2 else float(m1)

# ───────────────────────────────────────────────────────────────
# 3 ▸  Backward-chaining engine with trace
# ───────────────────────────────────────────────────────────────
counter = count(1)

def bc_control1() -> List[Tuple[str, float]]:
    """Prove all solutions for control1(A,C) with full trace."""
    results = []

    # ── Attempt clause #1  (actuator1) ─────────────────────────
    depth = 0
    print(f"Step {next(counter):02}: prove control1(_,_) using clause 1")
    m1 = measurement10("input1")

    if m1 is not None:
        print(f"  ✓ measurement10(input1) = {m1:.6f}")
    else:
        print("  ✗ measurement10(input1) fails")

    if m1 is not None and measurement2.get("input2") is True and "disturbance1" in measurement3:
        d1 = measurement3["disturbance1"]
        print("  ✓ measurement2(input2) = True")
        print("  ✓ measurement3 has disturbance1 =", d1)

        c1 = m1 * 19.6
        c2 = math.log10(d1)
        c  = c1 - c2
        print(f"  computed c1 = {c1:.6f}, c2 = {c2:.6f}, command = {c:.6f}")
        results.append(("actuator1", c))
    else:
        print("  ✗ clause 1 fails")

    # ── Attempt clause #2  (actuator2) ─────────────────────────
    depth = 0
    print(f"\nStep {next(counter):02}: prove control1(_,_) using clause 2")
    if "state3" in observation3 and "output2" in measurement4 and "output2" in target2:
        p3 = observation3["state3"]
        m4 = measurement4["output2"]
        t2 = target2["output2"]

        print(f"  ✓ state3 = {p3} , output2 = {m4} , target2 = {t2}")
        e = t2 - m4
        if e != 0:
            d  = p3 - m4
            c1 = 5.8 * e
            n  = 7.3 / e
            c2 = n * d
            c  = c1 + c2
            print(f"  computed e={e}, d={d}, c1={c1:.6f}, n={n:.6f}, c2={c2:.6f}, command = {c:.6f}")
            results.append(("actuator2", c))
        else:
            print("  ✗ error e == 0  → clause 2 fails")
    else:
        print("  ✗ missing facts  → clause 2 fails")

    return results

# ───────────────────────────────────────────────────────────────
# 4 ▸  Run the query
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    for a,c in bc_control1():
        print(f"\nAnswer:  {a}  command = {c:.4f}")


