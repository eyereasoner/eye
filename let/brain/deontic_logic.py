#!/usr/bin/env python3
"""
deontic_logic.py
Purely logical backward-proof version.

Scenario
--------
Light colour (exclusive choice):
    light ∈ {red, green}

Driver action (exclusive choice):
    action ∈ {stop, go}

Norms (obligations, deterministic):
    red   → ought(stop)
    green → ought(go)

Violation predicate:
    violation ← oblig(A) ∧ ¬action(A)

Queries
-------
    violation
    light(red)   , light(green)
    action(stop) , action(go)

For each query we print a trace over the four worlds and then classify the
result as VALID / SATISFIABLE / UNSATISFIABLE.
"""

from typing import List, Tuple, Optional

# ───────────────────────────────────────────────────────────────
# 1) Enumerate the four possible worlds (no weights)
# ───────────────────────────────────────────────────────────────
World = Tuple[str, str]   # (light_colour, action)
WORLDS: List[World] = [
    ("red",   "stop"),
    ("red",   "go"),
    ("green", "stop"),
    ("green", "go"),
]


# ───────────────────────────────────────────────────────────────
# 2) Norms and derived predicates (deterministic)
# ───────────────────────────────────────────────────────────────
def obligated_action(light_colour: str) -> str:
    """Return the (unique) action that is obligatory under the given light."""
    if light_colour == "red":
        return "stop"
    if light_colour == "green":
        return "go"
    raise ValueError(f"Unknown light colour: {light_colour!r}")

def holds(pred: str, arg: Optional[str], world: World) -> bool:
    """Truth of a (possibly nullary) predicate in a given world."""
    light, act = world

    if pred == "violation":
        # violation ← oblig(A) ∧ ¬action(A)
        ought = obligated_action(light)
        return act != ought

    if pred == "light":
        if arg not in ("red", "green"):
            raise ValueError(f"Unknown light argument: {arg!r}")
        return light == arg

    if pred == "action":
        if arg not in ("stop", "go"):
            raise ValueError(f"Unknown action argument: {arg!r}")
        return act == arg

    raise ValueError(f"Unknown predicate: {pred!r}")


# ───────────────────────────────────────────────────────────────
# 3) Parsing and proof routine (logical only)
# ───────────────────────────────────────────────────────────────
def parse_query(q: str) -> Tuple[str, Optional[str]]:
    """
    Accepts either a nullary predicate (e.g., "violation")
    or a unary form "light(red)" / "action(stop)".
    Returns (predicate, argument_or_None).
    """
    q = q.strip()
    if "(" not in q:
        return q, None
    assert q.endswith(")"), f"Bad query: {q}"
    pred, inner = q[:-1].split("(", 1)
    pred = pred.strip()
    arg = inner.strip()
    return pred, arg

def prove(query: str) -> List[World]:
    """
    Print a proof trace for `query` over all worlds,
    and return the list of worlds where it holds.
    """
    pred, arg = parse_query(query)
    print(f"\n=== Proving {pred if arg is None else f'{pred}({arg})'} ===")

    satisfied: List[World] = []
    for idx, w in enumerate(WORLDS, 1):
        light, act = w
        truth = holds(pred, arg, w)

        # Explanations / reasons (especially for 'violation')
        reason = ""
        if pred == "violation":
            ought = obligated_action(light)
            if truth:
                reason = f" (light={light} ⇒ ought({ought}); action={act} ≠ {ought} ⇒ violation)"
            else:
                reason = f" (light={light} ⇒ ought({ought}); action={act} = {ought} ⇒ no violation)"
        elif pred == "light":
            reason = f" (world has light={light})"
        elif pred == "action":
            reason = f" (world has action={act})"

        print(f"World {idx}: light={light:<5} action={act:<4}  "
              f"{'✓' if truth else '✗'} {pred if arg is None else pred + '(' + arg + ')'}{reason}")

        if truth:
            satisfied.append(w)

    # Classification
    n = len(WORLDS)
    k = len(satisfied)
    if k == n:
        print(f"Result: VALID — {pred if arg is None else f'{pred}({arg})'} is true in all {n} worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — {pred if arg is None else f'{pred}({arg})'} is false in all {n} worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} worlds.")

    return satisfied


# ───────────────────────────────────────────────────────────────
# 4) Run the queries & summarise (logical status only)
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    queries = [
        "violation",
        "light(red)",
        "light(green)",
        "action(stop)",
        "action(go)",
    ]
    results = {q: prove(q) for q in queries}

    print("\n=== Summary ===")
    n = len(WORLDS)
    for q in queries:
        worlds_where = results[q]
        k = len(worlds_where)
        if k == n:
            status = "VALID"
            extra = ""
        elif k == 0:
            status = "UNSATISFIABLE"
            extra = ""
        else:
            status = f"SATISFIABLE in {k}/{n} worlds"
            extra = " → " + ", ".join(f"(light={L},action={A})" for L, A in worlds_where)
        print(f"{q}: {status}{extra}")

