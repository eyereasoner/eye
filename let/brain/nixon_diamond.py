# ============================================
# EXPLAIN-AND-CHECK (Logic): Nixon Diamond with Priorities
# ============================================
# Atoms about Nixon (one individual n):
#   Q := Quaker(n)
#   R := Republican(n)
#   D := Dove(n)
#   H := Hawk(n)
#
# Hard facts:
FACTS = {"Q", "R"}

# Defaults (defeasible rules; read “normally”):
#   δ1: from Q infer D     (Quakers are normally doves)
#   δ2: from R infer H     (Republicans are normally hawks)
DEFAULTS = {
    "δ1": ("Q", "D"),
    "δ2": ("R", "H"),
}

# Incompatibility (background constraint): cannot be both a dove and a hawk
INCOMPAT = {("D", "H"), ("H", "D")}

# -------------------------------------------------------
# The “reason why” with priorities (narrative)
# -------------------------------------------------------
print("============================================")
print("Nixon Diamond with Priorities — explain and check")
print("============================================\n")

print("Hard facts:")
print("  Q := Quaker(n),  R := Republican(n)")
print("Defaults:")
print("  δ1: Q : D      (normally, Quakers are Doves)")
print("  δ2: R : H      (normally, Republicans are Hawks)")
print("Constraint:")
print("  not (D and H)\n")

print("Priority idea:")
print("  When two applicable defaults would conflict, prefer the higher-priority one and")
print("  block the lower-priority conclusion if it would violate the constraint.\n")

print("Case A — Prioritize δ1 over δ2 (Quaker-default outranks Republican-default):")
print("  Facts give Q and R, so both defaults are applicable in principle.")
print("  Apply δ1 first ⇒ conclude D. Now δ2 would add H, but (D ∧ H) is forbidden,")
print("  so δ2 is blocked. Unique prioritized extension: {Q, R, D}.\n")

print("Case B — Prioritize δ2 over δ1 (Republican-default outranks Quaker-default):")
print("  Apply δ2 first ⇒ conclude H. Now δ1 would add D, but (D ∧ H) is forbidden,")
print("  so δ1 is blocked. Unique prioritized extension: {Q, R, H}.\n")

print("Conclusion:")
print("  Priorities collapse the two competing default conclusions into a single outcome.")
print("  With δ1>δ2 we get Dove; with δ2>δ1 we get Hawk. □\n")

# -------------------------------------------------------
# Minimal prioritized engine (no imports)
# -------------------------------------------------------

def consistent(atoms):
    """True iff 'atoms' contains no incompatible pair."""
    return not (("D" in atoms) and ("H" in atoms))

def applicable(current, default):
    """A default is applicable if its precondition holds and adding its conclusion stays consistent."""
    pre, cons = default
    if pre not in current:
        return False
    if cons in current:
        return False
    return consistent(current | {cons})

def prioritized_extension(facts, defaults, priority):
    """
    Greedy prioritized construction:
      Repeat scanning defaults in given priority order; whenever a default is
      applicable and consistent, add its conclusion. Stop when no new info is added.
    This yields a single prioritized extension (deterministic for a fixed priority).
    """
    current = set(facts)
    changed = True
    while changed:
        changed = False
        for name in priority:
            pre, cons = defaults[name]
            if applicable(current, (pre, cons)):
                # Accept higher-priority conclusion
                current.add(cons)
                changed = True
                # Once added, any conflicting lower-priority conclusion becomes inapplicable
    return current

# Compute both priority orders
PRIO_A = ["δ1", "δ2"]  # Quaker-default outranks Republican-default
PRIO_B = ["δ2", "δ1"]  # Republican-default outranks Quaker-default

ext_A = prioritized_extension(FACTS, DEFAULTS, PRIO_A)
ext_B = prioritized_extension(FACTS, DEFAULTS, PRIO_B)

print("Computed prioritized extensions:")
print(f"  With δ1>δ2: {sorted(ext_A)}")
print(f"  With δ2>δ1: {sorted(ext_B)}\n")

# -------------------------------------------------------
# Harness: sanity checks for both priority orders
# -------------------------------------------------------

# Expected unique extensions under each priority
expected_A = {"Q", "R", "D"}
expected_B = {"Q", "R", "H"}

# 1) Exact matches
assert ext_A == expected_A, f"With δ1>δ2 expected {expected_A}, got {ext_A}"
assert ext_B == expected_B, f"With δ2>δ1 expected {expected_B}, got {ext_B}"

# 2) Consistency
assert consistent(ext_A), "Extension under δ1>δ2 is inconsistent."
assert consistent(ext_B), "Extension under δ2>δ1 is inconsistent."

# 3) Monotonicity w.r.t. facts: both include Q and R
assert "Q" in ext_A and "R" in ext_A
assert "Q" in ext_B and "R" in ext_B

print("Harness:")
print("  • Priorities δ1>δ2 ⇒ {Q,R,D} (Dove).")
print("  • Priorities δ2>δ1 ⇒ {Q,R,H} (Hawk).")
print("  • Both extensions consistent; facts preserved. ✓")

