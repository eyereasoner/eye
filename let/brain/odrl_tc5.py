#!/usr/bin/env python3
"""
Testcase-5: Permission vs Prohibition with constraints (age == 18)
==================================================================

Scenario (from ODRL Test Conflicts, testcase-5):
  - policy5a: Permission to odrl:read ex:resourceX when ex:age = 18.
  - policy5b: Prohibition to odrl:read ex:resourceX when ex:age = 18.

Expected global activation state: Conflict
  Rationale: The policies’ preconditions are identical (age == 18). In every
  world satisfying those preconditions, the action is simultaneously permitted
  and prohibited. Therefore the combined policies are in Conflict.  [testcase-5]
"""

from dataclasses import dataclass
from typing import Optional, List

# --- Basic model for this testcase -------------------------------------------

@dataclass(frozen=True)
class World:
    age: int  # ex:age for Alice

    def __repr__(self) -> str:
        return f"(age={self.age})"

@dataclass(frozen=True)
class Constraint:
    left_operand: str   # "ex:age"
    operator: str       # "odrl:eq"
    right_operand: int  # 18

    def holds_in(self, w: World) -> bool:
        if self.left_operand != "ex:age":
            raise ValueError("This demo only supports ex:age as leftOperand.")
        if self.operator != "odrl:eq":
            raise ValueError("This demo only supports odrl:eq.")
        return w.age == self.right_operand

    def intersection(self, other: "Constraint") -> Optional["Constraint"]:
        """
        Intersection over the same variable with eq:
          eq(18) ∧ eq(18) = eq(18)
          eq(18) ∧ eq(21) = ⊥ (empty)
        """
        if (self.left_operand, self.operator) != (other.left_operand, other.operator):
            return None
        if self.right_operand == other.right_operand:
            return Constraint(self.left_operand, self.operator, self.right_operand)
        return None  # disjoint

@dataclass(frozen=True)
class Rule:
    kind: str          # "permission" or "prohibition"
    assignee: str      # "ex:alice"
    action: str        # "odrl:read"
    target: str        # "ex:resourceX"
    constraint: Constraint

# Instantiate the two rules exactly as described in the testcase
POLICY_5A = Rule(
    kind="permission",
    assignee="ex:alice",
    action="odrl:read",
    target="ex:resourceX",
    constraint=Constraint("ex:age", "odrl:eq", 18),
)
POLICY_5B = Rule(
    kind="prohibition",
    assignee="ex:alice",
    action="odrl:read",
    target="ex:resourceX",
    constraint=Constraint("ex:age", "odrl:eq", 18),
)

# --- Report URIs --------------------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

# --- Classification logic -----------------------------------------------------

def same_subject_action_target(a: Rule, b: Rule) -> bool:
    return (a.assignee == b.assignee and a.action == b.action and a.target == b.target)

def conflict_on_intersection(permission: Rule, prohibition: Rule) -> bool:
    """
    We restrict attention to the set of worlds where *both* rules can apply,
    i.e., the intersection of their preconditions. If that intersection is
    non-empty and on every world in that intersection both rules hold, we
    classify as Conflict (which matches testcase-5).
    """
    if not (permission.kind == "permission" and prohibition.kind == "prohibition"):
        return False
    if not same_subject_action_target(permission, prohibition):
        return False

    inter = permission.constraint.intersection(prohibition.constraint)
    if inter is None:
        # No world satisfies both constraints simultaneously -> no conflict.
        return False

    # For eq-constraints, the intersection describes exactly one world.
    witness_world = World(age=inter.right_operand)

    # Both rules apply in that world (by construction) -> immediate clash.
    perm_holds = permission.constraint.holds_in(witness_world)
    prohib_holds = prohibition.constraint.holds_in(witness_world)
    return perm_holds and prohib_holds

def classify_global(permission: Rule, prohibition: Rule) -> str:
    if conflict_on_intersection(permission, prohibition):
        return REPORT_URIS["Conflict"]
    # For completeness; not expected for this testcase.
    return REPORT_URIS["NoConflict"]

# --- Goal-oriented proof construction ----------------------------------------

def build_goal_oriented_proof(permission: Rule, prohibition: Rule) -> str:
    """
    Goal G:
      Show Conflict by exhibiting a world w in which both rules apply.

    Strategy:
      1) Observe the constraints are identical: ex:age == 18.
      2) Choose witness world w := (age = 18).
      3) In w, Permission(read) and Prohibition(read) both hold for the same
         subject/action/target.
      4) Since the intersection of preconditions is precisely {w}, *every*
         world compatible with both policies is a clash ⇒ Conflict.
    """
    inter = permission.constraint.intersection(prohibition.constraint)
    assert inter is not None
    w = World(age=inter.right_operand)

    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append(f"  (P1) Permission:  {permission.assignee} may {permission.action} {permission.target}")
    lines.append(f"       when {permission.constraint.left_operand} == {permission.constraint.right_operand}.")
    lines.append(f"  (F1) Prohibition: {prohibition.assignee} must-not {prohibition.action} {prohibition.target}")
    lines.append(f"       when {prohibition.constraint.left_operand} == {prohibition.constraint.right_operand}.")
    lines.append("")
    lines.append("Precondition analysis:")
    lines.append("  The constraints are identical, so their intersection is the singleton set {ex:age = 18}.")
    lines.append("")
    lines.append("Witness construction:")
    lines.append(f"  Choose world w := {w}. Then (P1) holds in w and (F1) holds in w.")
    lines.append("")
    lines.append("Derivation:")
    lines.append("  (S1) In w, Permission(odrl:read) holds (by P1).")
    lines.append("  (S2) In w, Prohibition(odrl:read) holds (by F1).")
    lines.append("  (C) Therefore, in every world that satisfies both policies’ preconditions")
    lines.append("      (i.e., the intersection {w}), Perm ∧ Prohib holds ⇒ Global state: Conflict.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    permission  = POLICY_5A
    prohibition = POLICY_5B

    result_uri = classify_global(permission, prohibition)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(permission, prohibition))

if __name__ == "__main__":
    main()

