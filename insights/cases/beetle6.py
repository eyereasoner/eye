#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Beetle6 — ARC (Answer / Reason / Check), self-contained

Worlds before evidence:
  W1: blue
  W2: green ∧ nice
  W3: green ∧ pretty

Rules:
  beautiful(X) ← blue(X) ∨ pretty(X)
  bad(X)       ← beautiful(X)

Evidence:
  evidence(bad,false)  ⇒ drop any world where beautiful holds
  ⇒ only W2 (“green ∧ nice”) survives.

Queries:
  prop(nice,beetle), prop(beautiful,beetle), prop(blue,beetle)
"""

from typing import Dict, List, Tuple

World = str

# ───────────────────────── 1) Worlds & per-world semantics ─────────────────────────
WORLDS_BEFORE: List[World] = ["blue", "nice", "pretty"]  # stable order: W1,W2,W3

def atoms_in(world: World) -> Dict[str, bool]:
    """
    Truth assignment for primitive atoms and rule-derived predicates,
    under the given world identifier.
    """
    is_blue   = (world == "blue")
    is_nice   = (world == "nice")
    is_pretty = (world == "pretty")
    is_green  = is_nice or is_pretty

    beautiful = is_blue or is_pretty         # beautiful ← blue ∨ pretty
    bad       = beautiful                    # bad ← beautiful

    return {
        "blue": is_blue,
        "green": is_green,
        "nice": is_nice,
        "pretty": is_pretty,
        "beautiful": beautiful,
        "bad": bad,
    }

# ───────────────────────── 2) Apply evidence (prune worlds) ─────────────────────────
def apply_evidence(worlds: List[World]) -> List[World]:
    print("Applying evidence: bad ← beautiful ; evidence(bad,false)")
    kept: List[World] = []
    for idx, w in enumerate(worlds, 1):
        is_beautiful = atoms_in(w)["beautiful"]
        if is_beautiful:
            print(f"W{idx}: {w:<7} beautiful holds ⇒ bad would be true ⇒ DROP")
        else:
            print(f"W{idx}: {w:<7} beautiful false ⇒ bad false ⇒ KEEP")
            kept.append(w)
    if kept:
        print("Surviving worlds:", ", ".join(kept))
    else:
        print("No worlds survive the evidence.")
    return kept

VALID_WORLDS: List[World] = apply_evidence(WORLDS_BEFORE)

# ───────────────────────── 3) Proving & classification helpers ─────────────────────
def holds_in_world(atom: str, world: World) -> bool:
    m = atoms_in(world)
    if atom not in m:
        raise ValueError(f"Unknown atom: {atom!r}")
    return m[atom]

def prove(atom: str) -> List[World]:
    """
    Print a short trace for prop(atom,beetle) over surviving worlds; return
    the list of worlds where it holds.
    """
    print(f"\n--- Trace for prop({atom},beetle) ---")
    if not VALID_WORLDS:
        print("No surviving worlds — UNSATISFIABLE by vacuity.")
        return []
    worlds_true: List[World] = []
    for idx, w in enumerate(VALID_WORLDS, 1):
        truth = holds_in_world(atom, w)
        reason = ""
        if atom == "beautiful":
            if w == "blue": reason = " (because blue ⇒ beautiful)"
            if w == "pretty": reason = " (because pretty ⇒ beautiful)"
        if atom == "green" and w in ("nice", "pretty"):
            reason = " (green holds under nice/pretty)"
        print(f"World {idx}: {w:<7} {'✓' if truth else '✗'} {atom}{reason}")
        if truth:
            worlds_true.append(w)

    n, k = len(VALID_WORLDS), len(worlds_true)
    if k == n:
        print(f"Result: VALID — true in all {n} surviving worlds.")
    elif k == 0:
        print(f"Result: UNSATISFIABLE — false in all {n} surviving worlds.")
    else:
        print(f"Result: SATISFIABLE (but not valid) — holds in {k}/{n} surviving worlds.")
    return worlds_true

def classify(atom: str) -> Tuple[str, List[World]]:
    worlds_where = [w for w in VALID_WORLDS if holds_in_world(atom, w)]
    n, k = len(VALID_WORLDS), len(worlds_where)
    if n == 0:
        return "UNSATISFIABLE (no surviving worlds)", worlds_where
    if k == n:
        return "VALID", worlds_where
    if k == 0:
        return "UNSATISFIABLE", worlds_where
    return (f"SATISFIABLE in {k}/{n} worlds", worlds_where)

# ───────────────────────────────────── ARC: Answer ───────────────────────────
def print_answer():
    print("Answer")
    print("======")
    queries = ("nice", "beautiful", "blue")
    results = {q: prove(q) for q in queries}

    print("\nSummary")
    print("-------")
    n = len(VALID_WORLDS)
    for q in ("nice", "beautiful", "blue"):
        worlds_where = results[q]
        k = len(worlds_where)
        if n == 0:
            status = "UNSATISFIABLE (no surviving worlds)"
        elif k == n:
            status = "VALID"
        elif k == 0:
            status = "UNSATISFIABLE"
        else:
            status = f"SATISFIABLE in {k}/{n} worlds"
        extra = "" if k in (0, n) else " → " + ", ".join(worlds_where)
        print(f"prop({q},beetle): {status}{extra}")

# ────────────────────────────────── ARC: Reason why ─────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("Three exclusive worlds encode uncertainty about the beetle:")
    print("  • W1 = blue")
    print("  • W2 = green ∧ nice")
    print("  • W3 = green ∧ pretty")
    print("Rules make beautiful true exactly in worlds with blue or pretty.")
    print("Since bad ← beautiful and the evidence requires bad=false, any")
    print("world with beautiful must be dropped. Only W2 (green ∧ nice) remains.")
    print("Therefore:")
    print("  • prop(nice,beetle) is VALID (true in the only surviving world).")
    print("  • prop(beautiful,beetle) is UNSATISFIABLE post-evidence.")
    print("  • prop(blue,beetle) is UNSATISFIABLE post-evidence.")

# ─────────────────────────────── ARC: Check (harness) ────────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Evidence pruning matches the intended outcome: only 'nice' survives.
    ok_survive = (VALID_WORLDS == ["nice"])
    print(f"Only W2 ('nice') survives? {ok_survive}")
    ok_all &= ok_survive

    # 2) In surviving worlds, beautiful and bad are false; nice is true.
    ok_vals = True
    for w in VALID_WORLDS:
        m = atoms_in(w)
        ok_vals &= (m["beautiful"] is False and m["bad"] is False and m["nice"] is True)
    print(f"In surviving worlds: ¬beautiful ∧ ¬bad ∧ nice ? {ok_vals}")
    ok_all &= ok_vals

    # 3) Classifications match expectations.
    exp = {
        "nice": ("VALID", ["nice"]),
        "beautiful": ("UNSATISFIABLE", []),
        "blue": ("UNSATISFIABLE", []),
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

    # 4) Idempotence / determinism
    idem = (apply_evidence(WORLDS_BEFORE) == VALID_WORLDS) and all(classify(a) == classify(a) for a in ("nice","beautiful","blue"))
    print(f"Evidence & classify() are deterministic/idempotent? {idem}")
    ok_all &= idem

    print(f"\nAll checks passed? {ok_all}")

# ───────────────────────────────────── Main ──────────────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

