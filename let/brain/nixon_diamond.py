#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Nixon Diamond with Priorities — ARC (Answer / Reason / Check), self-contained

Atoms about Nixon (one individual n):
  Q := Quaker(n)
  R := Republican(n)
  D := Dove(n)
  H := Hawk(n)

Hard facts:
  FACTS = {Q, R}

Defaults (defeasible, “normally” rules):
  δ1: from Q infer D      (Quakers are normally Doves)
  δ2: from R infer H      (Republicans are normally Hawks)

Constraint (background incompatibility):
  not (D and H)
"""

# ─────────────────────────── Knowledge ───────────────────────────
FACTS = {"Q", "R"}
DEFAULTS = {
    "δ1": ("Q", "D"),
    "δ2": ("R", "H"),
}
# Incompatible conclusions (both ways)
INCOMPAT = {("D", "H"), ("H", "D")}

# ─────────────────────────── Utilities ────────────────────────────
def consistent(atoms):
    """True iff 'atoms' contains no incompatible pair."""
    return not (("D" in atoms) and ("H" in atoms))

def applicable(current, default):
    """Default is applicable if precondition holds and adding cons stays consistent."""
    pre, cons = default
    if pre not in current:
        return False
    if cons in current:
        return False
    return consistent(current | {cons})

def prioritized_extension(facts, defaults, priority, trace=False):
    """
    Greedy prioritized construction:
      Scan defaults in the given priority order; whenever a default is applicable,
      add its conclusion. Repeat until a fixed point. Deterministic for a fixed
      priority order.
    """
    current = set(facts)
    step = 1
    changed = True
    while changed:
        changed = False
        for name in priority:
            pre, cons = defaults[name]
            if applicable(current, (pre, cons)):
                if trace:
                    print(f"  Step {step:02}: apply {name}: {pre} ⇒ {cons}")
                current.add(cons)
                step += 1
                changed = True
            else:
                if trace:
                    reason = []
                    if pre not in current:
                        reason.append(f"precondition '{pre}' not known")
                    elif cons in current:
                        reason.append(f"conclusion '{cons}' already present")
                    else:
                        reason.append(f"adding '{cons}' would violate constraint")
                    print(f"  Step {step:02}: block  {name} ({'; '.join(reason)})")
                    step += 1
    return current

# Priority orders
PRIO_A = ["δ1", "δ2"]  # Quaker-default outranks Republican-default
PRIO_B = ["δ2", "δ1"]  # Republican-default outranks Quaker-default

# ───────────────────────────── Answer ─────────────────────────────
def print_answer():
    print("Answer")
    print("======")
    ext_A = prioritized_extension(FACTS, DEFAULTS, PRIO_A)
    ext_B = prioritized_extension(FACTS, DEFAULTS, PRIO_B)
    print("With priorities δ1>δ2 (prefer Quaker-default):")
    print(f"  Extension = {sorted(ext_A)}   ⇒ Nixon is a Dove.")
    print("With priorities δ2>δ1 (prefer Republican-default):")
    print(f"  Extension = {sorted(ext_B)}   ⇒ Nixon is a Hawk.")

# ──────────────────────────── Reason why ──────────────────────────
def print_reason():
    print("\nReason why")
    print("==========")
    print("We have two applicable defaults whose conclusions (D vs H) conflict.")
    print("The priority order decides which conclusion survives:")
    print("  • δ1>δ2 — apply δ1 first (Q ⇒ D). Adding H would violate D∧H, so δ2 is blocked.")
    print("  • δ2>δ1 — apply δ2 first (R ⇒ H). Adding D would violate D∧H, so δ1 is blocked.")
    print("\nTrace with δ1>δ2:")
    prioritized_extension(FACTS, DEFAULTS, PRIO_A, trace=True)
    print("\nTrace with δ2>δ1:")
    prioritized_extension(FACTS, DEFAULTS, PRIO_B, trace=True)

# ─────────────────────────── Check (harness) ──────────────────────
def print_check():
    print("\nCheck (harness)")
    print("===============")
    ext_A = prioritized_extension(FACTS, DEFAULTS, PRIO_A)
    ext_B = prioritized_extension(FACTS, DEFAULTS, PRIO_B)

    expected_A = {"Q", "R", "D"}
    expected_B = {"Q", "R", "H"}

    tests = []

    # 1) Exact matches against expected prioritized outcomes
    tests.append(("δ1>δ2 yields {Q,R,D}", ext_A == expected_A))
    tests.append(("δ2>δ1 yields {Q,R,H}", ext_B == expected_B))

    # 2) Consistency under the incompatibility constraint
    tests.append(("Extension δ1>δ2 is consistent", consistent(ext_A)))
    tests.append(("Extension δ2>δ1 is consistent", consistent(ext_B)))

    # 3) Facts are preserved (monotonic in facts)
    tests.append(("Facts ⊆ extension (δ1>δ2)", {"Q","R"} <= ext_A))
    tests.append(("Facts ⊆ extension (δ2>δ1)", {"Q","R"} <= ext_B))

    # 4) Blocked opposite conclusion is indeed absent
    tests.append(("Under δ1>δ2, H is blocked", "H" not in ext_A))
    tests.append(("Under δ2>δ1, D is blocked", "D" not in ext_B))

    # 5) Idempotence: re-applying construction is a fixed point
    tests.append(("Fixed point (δ1>δ2)", prioritized_extension(ext_A, DEFAULTS, PRIO_A) == ext_A))
    tests.append(("Fixed point (δ2>δ1)", prioritized_extension(ext_B, DEFAULTS, PRIO_B) == ext_B))

    all_ok = True
    for label, ok in tests:
        print(f"{label}? {ok}")
        all_ok &= ok

    print(f"\nAll checks passed? {all_ok}")

# ───────────────────────────────── Main ───────────────────────────
if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

