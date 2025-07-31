#!/usr/bin/env python3
"""
Testcase-3: Obligation vs Prohibition (unconditional, same subject/action/target)
=================================================================================

Scenario (from ODRL Test Conflicts, testcase-3):
  - policy3a: Alice has the obligation to read resource X.
  - policy3b: Alice is prohibited to read resource X.
  - No constraints/refinements on either rule.

Expected global activation state: Conflict
  "The policies permit and prohibit the action for any possible state of the world."

This script:
  1) Encodes the two unconditional rules (Obligation vs Prohibition).
  2) Classifies the global activation state as Conflict, because both apply in *every* world.
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
    kind: str          # "obligation" or "prohibition"
    assignee: str      # e.g., "ex:alice"
    action: str        # e.g., "odrl:read"
    target: str        # e.g., "ex:resourceX"
    refinement: Optional[str] = None  # None means unconditional

# Instantiate the two rules exactly as described
POLICY_3A = Rule(kind="obligation",  assignee="ex:alice", action="odrl:read", target="ex:resourceX")
POLICY_3B = Rule(kind="prohibition", assignee="ex:alice", action="odrl:read", target="ex:resourceX")

# --- Classification logic -----------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

def conflict_always_obligation_vs_prohibition(obligation: Rule, prohibition: Rule) -> bool:
    """
    Returns True iff:
      - one rule is an obligation and the other is a prohibition,
      - same assignee, action, and target,
      - both unconditional (no refinements).
      Then both apply in *every* possible state of the world ⇒ Conflict.
    """
    same_triplet = (
        obligation.assignee == prohibition.assignee and
        obligation.action   == prohibition.action   and
        obligation.target   == prohibition.target
    )
    unconditional = (obligation.refinement is None and prohibition.refinement is None)
    return (obligation.kind == "obligation"
            and prohibition.kind == "prohibition"
            and same_triplet
            and unconditional)

def classify_global(obligation: Rule, prohibition: Rule) -> str:
    if conflict_always_obligation_vs_prohibition(obligation, prohibition):
        return REPORT_URIS["Conflict"]
    # Not expected for this testcase, but kept for completeness.
    return REPORT_URIS["NoConflict"]

# --- Proof construction -------------------------------------------------------

def build_goal_oriented_proof(obligation: Rule, prohibition: Rule) -> str:
    """
    Goal-oriented proof that the global activation state is Conflict.

    Goal G:
      Show that for every possible state of the world w,
      O(read) and F(read) both hold for the same subject/action/target.

    Optional deontic note:
      Common axiom O(a) ⇒ P(a). Thus O(read) + F(read) also entails P(read) ∧ F(read).
    """
    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append(f"  (O1) Obligation:  {obligation.assignee} must {obligation.action} {obligation.target}.")
    lines.append(f"  (F1) Prohibition: {prohibition.assignee} must-not {prohibition.action} {prohibition.target}.")
    lines.append("  Both rules are unconditional (no constraints/refinements).")
    lines.append("")
    lines.append("Derivation:")
    lines.append("  (S1) Because (O1) is unconditional, for any world w, O(read) holds in w.")
    lines.append("  (S2) Because (F1) is unconditional, for any world w, F(read) holds in w.")
    lines.append("  (C) For arbitrary w, O(read) ∧ F(read). Since w was arbitrary,")
    lines.append("      this holds for all worlds. Therefore the policies conflict in every")
    lines.append("      possible state of the world ⇒ Global state: Conflict.")
    lines.append("")
    lines.append("Optional deontic note:")
    lines.append("  From O(read) we may infer P(read). Hence P(read) ∧ F(read) also holds in all worlds,")
    lines.append("  matching the testcase phrasing “permit and prohibit … for any possible state of the world.”")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    obligation  = POLICY_3A
    prohibition = POLICY_3B

    result_uri = classify_global(obligation, prohibition)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(obligation, prohibition))

if __name__ == "__main__":
    main()

