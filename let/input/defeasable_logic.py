"""
defeasible_logic.py
===================

Story recap
-----------
• Tweety is a bird.           → normally flies (prob. 0.9)
• Polly is a penguin.         → bird   +  abnormal (w.r.t. flying)
• Birds fly *unless* abnormal.
• Penguins are always abnormal, so their default flight rule is defeated.

Queries
-------
    flies(tweety)      → 0.9
    flies(polly)       → 0.0
    abnormal(polly)    → 1.0
"""

from typing import Dict, List, Tuple

# ───────────────────────────────────────────────────────────────
# 1 ▸  Ground facts & rules
# ───────────────────────────────────────────────────────────────
BIRDS = {"tweety"}
PENGUINS = {"polly"}          # penguins are birds too
BIRDS |= PENGUINS             # union

# defeasible default probability
P_FLY_IF_NORMAL = 0.9


# ───────────────────────────────────────────────────────────────
# 2 ▸  Enumerate the only random choice:  Tweety flies or not
# ───────────────────────────────────────────────────────────────
World = Tuple[bool, float]     # (flies_tweety?, probability weight)

WORLDS: List[World] = [
    (True,  P_FLY_IF_NORMAL),          # Tweety flies
    (False, 1 - P_FLY_IF_NORMAL),      # Tweety grounded
]


# ───────────────────────────────────────────────────────────────
# 3 ▸  Evaluate the queries
# ───────────────────────────────────────────────────────────────
def evaluate_queries() -> Dict[str, float]:
    probs = {
        "flies(tweety)":    0.0,
        "flies(polly)":     0.0,   # will stay zero
        "abnormal(polly)":  1.0,   # deterministic
    }

    for flies_tweety, weight in WORLDS:
        if flies_tweety:
            probs["flies(tweety)"] += weight

    return probs


# ───────────────────────────────────────────────────────────────
# 4 ▸  Main
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    results = evaluate_queries()
    for q in ("flies(tweety)", "flies(polly)", "abnormal(polly)"):
        print(f"{q}: {results[q]}")

