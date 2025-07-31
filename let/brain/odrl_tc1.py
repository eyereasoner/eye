#!/usr/bin/env python3
"""
Testcase-1: Permission vs Prohibition (unconditional, same subject/action/target)
================================================================================

Scenario (from ODRL Test Conflicts, testcase-1):
  - policy1a: Alice is permitted to read resource X.
  - policy1b: Alice is prohibited to read resource X.
  - No constraints/refinements/conditions on either rule.

Expected global activation state: Conflict
  "The policies permit and prohibit the action for any possible state of the world."

This script:
  1) Encodes the two unconditional rules.
  2) Classifies the global activation state using simple, explicit criteria.
  3) Always prints:
       - The URI for the global result
       - A concise goal-oriented proof explanation

No external libraries required.
"""

from dataclasses import dataclass
from typing import Optional, List

# --- Policy modeling ----------------------------------------------------------

@dataclass(frozen=True)
class Rule:
    kind: str          # "permission" or "prohibition"
    assignee: str      # e.g., "ex:alice"
    action: str        # e.g., "odrl:read"
    target: str        # e.g., "ex:resourceX"
    refinement: Optional[str] = None  # None means unconditional

# Instantiate the two rules exactly as described
POLICY_1A = Rule(kind="permission",  assignee="ex:alice", action="odrl:read", target="ex:resourceX")
POLICY_1B = Rule(kind="prohibition", assignee="ex:alice", action="odrl:read", target="ex:resourceX")

# --- Classification logic -----------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

def conflict_always(permission: Rule, prohibition: Rule) -> bool:
    """
    Returns True iff:
      - one rule is a permission and the other is a prohibition,
      - same assignee, action, and target,
      - both are unconditional (no refinements),
      - therefore both apply in *every* state of the world.
    """
    same_triplet = (
        permission.assignee == prohibition.assignee and
        permission.action  == prohibition.action  and
        permission.target  == prohibition.target
    )
    unconditional = (permission.refinement is None and prohibition.refinement is None)
    return (permission.kind == "permission"
            and prohibition.kind == "prohibition"
            and same_triplet
            and unconditional)

def classify_global(permission: Rule, prohibition: Rule) -> str:
    if conflict_always(permission, prohibition):
        return REPORT_URIS["Conflict"]
    # The testcase is specifically the unconditional clash,
    # but we keep fallbacks for completeness.
    return REPORT_URIS["NoConflict"]

# --- Proof construction -------------------------------------------------------

def build_goal_oriented_proof(permission: Rule, prohibition: Rule) -> str:
    """
    Goal-oriented proof that the global activation state is Conflict.

    Goal G:
      Show that for every possible state of the world w,
      Perm(read) and Prohib(read) both hold for the same subject/action/target.
    """
    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append(f"  (P1) Permission:  {permission.assignee} may {permission.action} {permission.target}.")
    lines.append(f"  (F1) Prohibition: {prohibition.assignee} must-not {prohibition.action} {prohibition.target}.")
    lines.append("  Both rules have no conditions/refinements (they are unconditional).")
    lines.append("")
    lines.append("Derivation:")
    lines.append("  (S1) Because (P1) is unconditional, for any world w, Perm(read) holds in w.")
    lines.append("  (S2) Because (F1) is unconditional, for any world w, Prohib(read) holds in w.")
    lines.append("  (C) For arbitrary w, Perm(read) ∧ Prohib(read). Since w was arbitrary,")
    lines.append("      this holds for all worlds. Therefore the policies conflict in every")
    lines.append("      possible state of the world ⇒ Global state: Conflict.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    permission  = POLICY_1A
    prohibition = POLICY_1B

    result_uri = classify_global(permission, prohibition)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(permission, prohibition))

if __name__ == "__main__":
    main()

