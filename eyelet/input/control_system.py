"""
Python re-write of the Prolog “Control System” example.

Author:  Jos De Roo
Date  :  2025-06-13
"""

import math
from typing import List, Tuple, Optional

# ──────────────────────────────────────────────────────────────
# ❶  FACTS  (the stuff after the dots in Prolog)
# ──────────────────────────────────────────────────────────────
# measurements
measurement1 = {
    "input1":       [6, 11],
    "disturbance2": [45, 39],
}
measurement2 = {
    "input2": True,
}
measurement3 = {
    "input3":       56967,
    "disturbance1": 35766,
}
measurement4 = {
    "output2": 24,
}

# observations
observation1 = {"state1": 80}
observation2 = {"state2": False}
observation3 = {"state3": 22}

# targets
target2 = {"output2": 29}

# ──────────────────────────────────────────────────────────────
# ❷  RULES  (each Prolog clause → one Python function)
# ──────────────────────────────────────────────────────────────
def measurement10(I: str) -> Optional[float]:
    """
    Implements the two-clause Prolog rule:

        measurement10(I,M) :-
            measurement1(I,[M1,M2]), M1 < M2, M3 is M2-M1, M is sqrt(M3).

        measurement10(I,M1) :-
            measurement1(I,[M1,M2]), M1 >= M2.

    Returns None if no fact matches.
    """
    pair = measurement1.get(I)
    if pair is None:
        return None

    m1, m2 = pair
    if m1 < m2:
        return math.sqrt(m2 - m1)
    else:                       # m1 >= m2
        return float(m1)        # keep it a float so later math works


def control1() -> List[Tuple[str, float]]:
    """
    Collect every (Actuator, C) pair that satisfies the Prolog query
        ?- control1(_, _).
    """
    results: List[Tuple[str, float]] = []

    # ── Clause #1  (actuator1) ──────────────────────────────────
    m1 = measurement10("input1")
    if (
        m1 is not None
        and measurement2.get("input2") is True
        and "disturbance1" in measurement3
    ):
        d1 = measurement3["disturbance1"]
        c1 = m1 * 19.6                # proportional part
        c2 = math.log10(d1)           # compensation part
        c  = c1 - c2                  # feed-forward control
        results.append(("actuator1", c))

    # ── Clause #2  (actuator2) ──────────────────────────────────
    if (
        "state3"  in observation3
        and "output2" in measurement4
        and "output2" in target2
    ):
        p3 = observation3["state3"]
        m4 = measurement4["output2"]
        t2 = target2["output2"]

        e = t2 - m4                   # error
        if e:                         # avoid div-by-zero (Prolog would error out)
            d = p3 - m4               # differential error
            c1 = 5.8 * e              # proportional part
            n  = 7.3 / e              # nonlinear factor
            c2 = n * d                # nonlinear differential part
            c  = c1 + c2              # P-N-D feedback control
            results.append(("actuator2", c))

    return results


# ──────────────────────────────────────────────────────────────
# ❸  RUN THE SAME QUERY Prolog WOULD
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    for actuator, value in control1():
        print(f"{actuator}: {value:.4f}")

