#!/usr/bin/env python3
"""
defeasible_logic.py
Purely logical version of the Tweety/Polly story.

Story facts & rules (deterministic)
-----------------------------------
• Tweety is a bird.
• Polly is a penguin; penguins are birds.
• Penguins are abnormal (w.r.t. flying).
• Default: birds fly unless abnormal.

We use a closed-world reading for the default: if X is a bird and we do
not know X to be abnormal, infer flies(X). Penguins *are* abnormal, so
their default flight rule is blocked.

Queries
-------
    flies(tweety)
    flies(polly)
    abnormal(polly)

For each query we print a proof trace over the (single) resulting world
and classify it as VALID / SATISFIABLE / UNSATISFIABLE.
"""

from typing import Dict, List, Tuple, Optional

# ───────────────────────────────────────────────────────────────
# 1) Ground facts (deterministic)
# ───────────────────────────────────────────────────────────────
BIRDS = {"tweety"}
PENGUINS = {"polly"}          # penguins are birds too
BIRDS |= PENGUINS             # union: {tweety, polly}

# Strict rule: penguins are abnormal w.r.t. flying
ABNORMAL = set(PENGUINS)      # {polly}

# Default (closed-world reading):
#   flies(X) ← bird(X) ∧ ¬abnormal(X)
FLIES = {x for x in BIRDS if x not in ABNORMAL}  # {tweety}


# ───────────────────────────────────────────────────────────────
# 2) Single (deterministic) world and truth assignment
# ───────────────────────────────────────────────────────────────
World = str
WORLDS: List[World] = ["default"]  # no exclusive branching remains

def atoms() -> Dict[str, Dict[str, bool]]:
    """
    Provide predicate truth tables for constants 'tweety' and 'polly'.
    """
    table: Dict[str, Dict[str, bool]] = {}
    for x in ("tweety", "polly"):
        table[x] = {
            "bird":     x in BIRDS,
            "penguin":  x in PENGUINS,
            "abnormal": x in ABNORMAL,
            "flies":    x in FLIES,
        }
    return table

ATOMS = atoms()


# ───────────────────────────────────────────────────────────────
# 3) Parsing and logical proof routine
# ───────────────────────────────────────────────────────────────
def parse_query(q: str) -> Tuple[str, str]:
    """
    Parse 'flies(tweety)' into ('flies', 'tweety').
    """
    q = q.strip()
    assert q.endswith(")") and "(" in q, f"Bad query: {q}"
    pred, inner = q[:-1].split("(", 1)
    return pred.strip(), inner.strip()

def holds(pred: str, arg: str) -> bool:
    if pred not in ATOMS[arg]:
        raise ValueError(f"Unknown predicate: {pred!r}")
    return ATOMS[arg][pred]

def prove(query: str) -> List[World]:
    """
    Print a proof trace for the ground query over the single world,
    and return the list (size 0 or 1) of worlds where it holds.
    """
    pred, arg = parse_query(query)
    print(f"\n=== Proving {pred}({arg}) ===")

    satisfied: List[World] = []
    for idx, w in enumerate(WORLDS, 1):
        truth = holds(pred, arg)

        # Explanations for transparency
        reason = ""
        if pred == "flies":
            if ATOMS[arg]["bird"] and not ATOMS[arg]["abnormal"]:
                reason = " (bird ∧ ¬abnormal ⇒ flies by default rule)"
            elif ATOMS[arg]["abnormal"]:
                reason = " (abnormal ⇒ default blocked)"
        elif pred == "abnormal":
            if arg in PENGUINS:
                reason = " (penguin ⇒ abnormal by strict rule)"

        print(f"World {idx}: {w:<7}  {'✓' if truth else '✗'} {pred}({arg}){reason}")

        if truth:
            satisfied.append(w)

    # Classification
    n = len(WORLDS)
    k = len(satisfied)
    if k == n:
        print(f"Result: VALID — {pred}({arg}) is true in all {n} worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — {pred}({arg}) is false in all {n} worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} worlds.")

    return satisfied


# ───────────────────────────────────────────────────────────────
# 4) Run the queries & summarise (logical status only)
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    queries = [
        "flies(tweety)",
        "flies(polly)",
        "abnormal(polly)",
    ]
    results = {q: prove(q) for q in queries}

    print("\n=== Summary ===")
    n = len(WORLDS)
    for q in queries:
        worlds_where = results[q]
        k = len(worlds_where)
        if k == n:
            status = "VALID"
        elif k == 0:
            status = "UNSATISFIABLE"
        else:
            status = f"SATISFIABLE in {k}/{n} worlds"
        print(f"{q}: {status}")

