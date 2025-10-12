#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Beetle — ARC (Answer / Reason / Check), self-contained

Facts
  car(beetle).

Exclusive choice (worlds)
  W1: green(beetle)
  W2: blue(beetle)

Rule
  beautiful(X) ← green(X) ∨ blue(X)

Queries
  prop(beautiful, beetle), prop(green, beetle), prop(blue, beetle)
"""

from typing import Dict, List, Tuple

World = str                      # "green" or "blue" (the colour true in that world)
WORLDS: List[World] = ["green", "blue"]   # keep stable order: W1 = green, W2 = blue

# ────────────────────────────── World semantics ──────────────────────────────
def atoms_in(world: World) -> Dict[str, bool]:
    """Truth assignment for the fixed entity X = beetle in the given world."""
    is_green = (world == "green")
    is_blue  = (world == "blue")
    return {
        "car": True,                               # fact: car(beetle)
        "green": is_green,                         # exclusive choice per world
        "blue": is_blue,
        "beautiful": is_green or is_blue,          # rule: beautiful ← green ∨ blue
    }

def holds_in_world(atom: str, world: World) -> bool:
    m = atoms_in(world)
    if atom not in m:
        raise ValueError(f"Unknown atom: {atom!r}")
    return m[atom]

# ──────────────────────────────── Proving/printing ───────────────────────────
def prove(atom: str) -> List[World]:
    """Trace the query over both worlds; return the worlds where it holds."""
    print(f"\n--- Trace for prop({atom},beetle) ---")
    holds: List[World] = []
    for idx, w in enumerate(WORLDS, 1):
        truth = holds_in_world(atom, w)
        reason = ""
        if atom == "beautiful":
            if w == "green":
                reason = " (because green ⇒ beautiful)"
            elif w == "blue":
                reason = " (because blue ⇒ beautiful)"
        mark = "✓" if truth else "✗"
        print(f"World {idx}: colour={w:<5} {mark} {atom}{reason}")
        if truth:
            holds.append(w)

    n, k = len(WORLDS), len(holds)
    if k == n:
        print(f"Result: VALID — prop({atom},beetle) is true in all {n} worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — prop({atom},beetle) is false in all {n} worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} worlds.")
    return holds

def classify(atom: str) -> Tuple[str, List[World]]:
    worlds = [w for w in WORLDS if holds_in_world(atom, w)]
    n, k = len(WORLDS), len(worlds)
    if k == n:   status = "VALID"
    elif k == 0: status = "UNSATISFIABLE"
    else:        status = f"SATISFIABLE in {k}/{n} worlds"
    return status, worlds

# ─────────────────────────────────── ARC: Answer ─────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    queries = ("beautiful", "green", "blue")
    results = {q: prove(q) for q in queries}

    print("\nSummary")
    print("-------")
    n = len(WORLDS)
    for q in ("beautiful", "green", "blue"):
        worlds_where = results[q]
        k = len(worlds_where)
        if k == n:   status = "VALID"
        elif k == 0: status = "UNSATISFIABLE"
        else:        status = f"SATISFIABLE in {k}/{n} worlds"
        extra = "" if k in (0, n) else " → " + ", ".join(worlds_where)
        print(f"prop({q},beetle): {status}{extra}")

# ──────────────────────────────── ARC: Reason why ────────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("We model uncertainty as two mutually exclusive worlds for the beetle’s colour:")
    print("  • W1 asserts green(beetle)")
    print("  • W2 asserts blue(beetle)")
    print("The rule beautiful(X) ← green(X) ∨ blue(X) makes beautiful(beetle) true")
    print("in *both* worlds, hence it is VALID. Each colour atom is true in exactly")
    print("one world, so each is merely SATISFIABLE (but not valid).")

# ───────────────────────────── ARC: Check (harness) ──────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Exclusivity & facts
    ok_exclusive = True
    for w in WORLDS:
        m = atoms_in(w)
        ok_exclusive &= (m["car"] is True)
        if w == "green":
            ok_exclusive &= (m["green"] is True and m["blue"] is False)
        if w == "blue":
            ok_exclusive &= (m["blue"] is True and m["green"] is False)
    print(f"Worlds encode an exclusive choice and car(beetle) is always true? {ok_exclusive}")
    ok_all &= ok_exclusive

    # 2) Rule correctness in each world
    ok_rule = all(atoms_in(w)["beautiful"] == (atoms_in(w)["green"] or atoms_in(w)["blue"]) for w in WORLDS)
    print(f"beautiful ↔ (green ∨ blue) holds per world? {ok_rule}")
    ok_all &= ok_rule

    # 3) Classification results match expectations
    exp = {
        "beautiful": ("VALID", ["green", "blue"]),
        "green":     ("SATISFIABLE in 1/2 worlds", ["green"]),
        "blue":      ("SATISFIABLE in 1/2 worlds", ["blue"]),
    }
    ok_class = True
    for atom, (exp_status, exp_worlds) in exp.items():
        status, worlds = classify(atom)
        ok_class &= (status == exp_status and worlds == exp_worlds)
    print(f"Classifications (VALID / SAT / UNSAT) match expected? {ok_class}")
    ok_all &= ok_class

    # 4) Idempotence: no hidden state alters truth on re-evaluation
    ok_idem = all(classify(a) == classify(a) for a in ("beautiful", "green", "blue"))
    print(f"Classify() is deterministic/idempotent? {ok_idem}")
    ok_all &= ok_idem

    print(f"\nAll checks passed? {ok_all}")

# ───────────────────────────────────── Main ──────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

