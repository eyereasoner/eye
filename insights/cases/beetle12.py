#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Beetle12 — ARC (Answer / Reason / Check), self-contained

Exclusive choice tree for *beetle* (no weights, just logical worlds):

  colour:  green | blue
  under green:        nice | pretty
  under nice/pretty:  1 | 2
  under x1/x2:        1 | 2         → leaves: nice11, nice12, nice21, nice22,
                                       pretty11, pretty12, pretty21, pretty22
  Plus the single leaf "blue".

Deterministic rules (no numeric aggregation):
  beautiful(X) ← blue(X)
  beautiful(X) ← leaf(X) with name starting "nice" or "pretty"

Queries:
  prop(beautiful,beetle), prop(green,beetle), prop(blue,beetle)
"""

from typing import Dict, List, Tuple

World = str  # a leaf atom naming an exclusive world

# ───────────────────────────────────────────
# 1) Enumerate the nine mutually-exclusive worlds
# ───────────────────────────────────────────
def worlds() -> List[World]:
    out: List[World] = ["blue"]
    for qual in ("nice", "pretty"):
        for i in ("1", "2"):
            for j in ("1", "2"):
                out.append(f"{qual}{i}{j}")
    return out

WORLDS: List[World] = worlds()  # ['blue', 'nice11', ..., 'pretty22'] (stable order)

# ───────────────────────────────────────────
# 2) Logical entailment per world (no probabilities)
# ───────────────────────────────────────────
def holds_in_world(goal: str, leaf: World) -> bool:
    """
    Return True iff `goal` holds in world `leaf` under the rules.
    Allowed goals: 'beautiful', 'green', 'blue'.
    """
    if goal == "blue":
        return leaf == "blue"
    if goal == "green":
        # 'green' means any non-blue leaf (nice*/pretty*)
        return leaf != "blue"
    if goal == "beautiful":
        # beauty rule: holds in 'blue' AND in any nice*/pretty* leaf
        if leaf == "blue":
            return True
        if leaf.startswith(("nice", "pretty")):
            return True
        return False
    raise ValueError(f"Unknown goal: {goal!r}")

def prove(goal: str) -> List[World]:
    """Print a short proof-like trace and return the worlds where goal holds."""
    print(f"\n--- Trace for prop({goal},beetle) ---")
    holds: List[World] = []
    for idx, leaf in enumerate(WORLDS, 1):
        reason = ""
        if goal == "beautiful":
            if leaf == "blue":
                reason = " (blue ⇒ beautiful)"
            elif leaf.startswith(("nice", "pretty")):
                reason = " (nice*/pretty* ⇒ beautiful)"
        if goal == "green" and leaf != "blue":
            reason = " (non-blue ⇒ green)"
        mark = "✓" if holds_in_world(goal, leaf) else "✗"
        print(f"World {idx:02} leaf={leaf:<8} {mark} {goal}{reason}")
        if mark == "✓":
            holds.append(leaf)

    n, k = len(WORLDS), len(holds)
    if k == n:
        print(f"Result: VALID — true in all {n} worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — false in all {n} worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} worlds.")
    return holds

def classify(goal: str) -> Tuple[str, List[World]]:
    worlds_where = [w for w in WORLDS if holds_in_world(goal, w)]
    n, k = len(WORLDS), len(worlds_where)
    if k == n:
        return "VALID", worlds_where
    if k == 0:
        return "UNSATISFIABLE", worlds_where
    return (f"SATISFIABLE in {k}/{n} worlds", worlds_where)

# ──────────────────────────────── ARC: Answer ────────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    queries = ("beautiful", "green", "blue")
    results = {q: prove(q) for q in queries}

    print("\nSummary")
    print("-------")
    n = len(WORLDS)
    for q in queries:
        worlds_where = results[q]
        k = len(worlds_where)
        if k == n:   status = "VALID"
        elif k == 0: status = "UNSATISFIABLE"
        else:        status = f"SATISFIABLE in {k}/{n} worlds"
        extra = "" if k in (0, n) else " → " + ", ".join(worlds_where)
        print(f"prop({q},beetle): {status}{extra}")

# ───────────────────────────── ARC: Reason why ───────────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("The choice tree yields 9 exclusive worlds: the single leaf 'blue',")
    print("and eight non-blue leaves starting with 'nice' or 'pretty'.")
    print("Rules make 'beautiful' true in *every* world (either blue, or nice*/pretty*),")
    print("so prop(beautiful,beetle) is VALID.")
    print("By definition, 'green' holds in every non-blue world, i.e., 8/9 worlds,")
    print("so prop(green,beetle) is SATISFIABLE (but not valid).")
    print("Finally, 'blue' holds in exactly 1/9 worlds, so it is SATISFIABLE (but not valid).")

# ──────────────────────────── ARC: Check (harness) ───────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) World set and count
    expected_worlds = [
        "blue",
        "nice11", "nice12", "nice21", "nice22",
        "pretty11", "pretty12", "pretty21", "pretty22",
    ]
    ok_worlds = (WORLDS == expected_worlds)
    print(f"Exactly 9 worlds in stable order? {ok_worlds}")
    ok_all &= ok_worlds

    # 2) Semantics: beautiful holds in all worlds; green ⇔ not blue
    ok_beauty = all(holds_in_world("beautiful", w) for w in WORLDS)
    ok_green  = all(holds_in_world("green", w) == (w != "blue") for w in WORLDS)
    print(f"'beautiful' true in every world? {ok_beauty}")
    print(f"'green' iff not 'blue'?          {ok_green}")
    ok_all &= ok_beauty and ok_green

    # 3) Classifications match expected counts and exemplars
    exp = {
        "beautiful": ("VALID", expected_worlds),
        "green":     ("SATISFIABLE in 8/9 worlds", [w for w in expected_worlds if w != "blue"]),
        "blue":      ("SATISFIABLE in 1/9 worlds", ["blue"]),
    }
    ok_class = True
    for atom, (status_exp, worlds_exp) in exp.items():
        status_got, worlds_got = classify(atom)
        ok = (status_got == status_exp and worlds_got == worlds_exp)
        if not ok:
            print(f"  Mismatch for {atom}: got ({status_got}, {worlds_got}), want ({status_exp}, {worlds_exp})")
        ok_class &= ok
    print(f"Classifications correct? {ok_class}")
    ok_all &= ok_class

    # 4) Determinism/idempotence
    idem = all(classify(a) == classify(a) for a in ("beautiful", "green", "blue"))
    print(f"Deterministic & idempotent classifications? {idem}")
    ok_all &= idem

    print(f"\nAll checks passed? {ok_all}")

# ────────────────────────────────── Main ────────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

