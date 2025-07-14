"""
deontic_logic.py
================

Scenario recap
--------------
Light colour (independent choice)
    P(light = red)   = 0.5
    P(light = green) = 0.5

Driver behaviour (conditioned on the light)
    If red   :  P(stop) = 0.8 ,  P(go) = 0.2
    If green :  P(stop) = 0.1 ,  P(go) = 0.9

Norms (obligations)
    red   →  ought(stop)
    green →  ought(go)

Violation predicate
    violation ←  oblig(A) ∧ ¬action(A)

Queries
-------
    violation
    light(red)   , light(green)
    action(stop) , action(go)
"""

from typing import Dict, Tuple, List

# ───────────────────────────────────────────────────────────────
# 1 ▸  Enumerate the four possible worlds
# ───────────────────────────────────────────────────────────────
World = Tuple[str, str, float]     # (light_colour, action, probability)

WORLDS: List[World] = [
    ("red",   "stop", 0.5 * 0.8),   # 0.40
    ("red",   "go",   0.5 * 0.2),   # 0.10
    ("green", "stop", 0.5 * 0.1),   # 0.05
    ("green", "go",   0.5 * 0.9),   # 0.45
]


# ───────────────────────────────────────────────────────────────
# 2 ▸  Compute query probabilities
# ───────────────────────────────────────────────────────────────
def evaluate_queries() -> Dict[str, float]:
    probs = {
        "violation":      0.0,
        "light(red)":     0.0,
        "light(green)":   0.0,
        "action(stop)":   0.0,
        "action(go)":     0.0,
    }

    for colour, act, weight in WORLDS:
        # light colour marginals
        probs[f"light({colour})"] += weight
        # action marginals
        probs[f"action({act})"] += weight

        # obligation & violation
        obligated = "stop" if colour == "red" else "go"
        if act != obligated:        # violated the norm
            probs["violation"] += weight

    return probs


# ───────────────────────────────────────────────────────────────
# 3 ▸  Main
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    results = evaluate_queries()
    for q in (
        "violation",
        "light(red)",
        "light(green)",
        "action(stop)",
        "action(go)",
    ):
        print(f"{q}: {results[q]}")

