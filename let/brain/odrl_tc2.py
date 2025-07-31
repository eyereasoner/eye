#!/usr/bin/env python3
"""
Testcase-2: Permission vs Prohibition — odrl:read ⊑ odrl:use
===========================================================

Scenario (from ODRL Test Conflicts, testcase-2):
  - policy2a: Alice is permitted to read resource X.
  - policy2b: Alice is prohibited to use resource X.
  - Action taxonomy: odrl:read is a subclass/narrower action of odrl:use.

Expected global activation state: Conflict
  "The policies permit and prohibit the action for any possible state of the world."

This script:
  1) Encodes a tiny action hierarchy with odrl:read ⊑ odrl:use.
  2) Interprets Prohibition(super-action) as applying to all of its sub-actions.
  3) Detects that unconditional Permission(read) clashes with Prohibition(use),
     hence with Prohibition(read) by subsumption.
  4) Always prints:
       - The URI for the global result
       - A concise goal-oriented proof explanation

No external libraries required.
"""

from dataclasses import dataclass
from typing import Dict, Set, Optional, List

# --- Action hierarchy ---------------------------------------------------------

class ActionHierarchy:
    """
    Minimal DAG-like hierarchy for ODRL actions.
    """
    def __init__(self) -> None:
        # parent map: child -> set of direct parents
        self.parents: Dict[str, Set[str]] = {}

    def add_subclass(self, child: str, parent: str) -> None:
        self.parents.setdefault(child, set()).add(parent)
        self.parents.setdefault(parent, set())  # ensure parent exists in dict

    def is_subaction_of(self, a: str, b: str) -> bool:
        """
        Return True iff action 'a' is 'b' or a (transitive) subclass of 'b'.
        """
        if a == b:
            return True
        visited: Set[str] = set()
        frontier: List[str] = [a]
        while frontier:
            cur = frontier.pop()
            if cur in visited:
                continue
            visited.add(cur)
            for p in self.parents.get(cur, ()):
                if p == b:
                    return True
                frontier.append(p)
        return False

# Build the hierarchy required by the testcase: read ⊑ use
ACTIONS = ActionHierarchy()
ACTIONS.add_subclass("odrl:read", "odrl:use")

# --- Policy modeling ----------------------------------------------------------

@dataclass(frozen=True)
class Rule:
    kind: str          # "permission" or "prohibition"
    assignee: str      # e.g., "ex:alice"
    action: str        # e.g., "odrl:read" or "odrl:use"
    target: str        # e.g., "ex:resourceX"
    refinement: Optional[str] = None  # None => unconditional

# Instantiate the two rules exactly as described
POLICY_2A = Rule(kind="permission",  assignee="ex:alice", action="odrl:read", target="ex:resourceX")
POLICY_2B = Rule(kind="prohibition", assignee="ex:alice", action="odrl:use",  target="ex:resourceX")

# --- Classification logic -----------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

def prohibition_covers(action_prohibited: str, action_checked: str) -> True:
    """
    A prohibition on a super-action covers all its sub-actions.
    """
    return ACTIONS.is_subaction_of(action_checked, action_prohibited)

def conflict_always(permission: Rule, prohibition: Rule) -> bool:
    """
    Returns True iff:
      - permission vs prohibition,
      - same assignee and target,
      - both unconditional,
      - and the prohibition's action covers (is a super-action of) the permission's action.
      In that case, both rules apply in every possible state: Conflict.
    """
    same_subject_target = (
        permission.assignee == prohibition.assignee and
        permission.target   == prohibition.target
    )
    unconditional = (permission.refinement is None and prohibition.refinement is None)
    covers = prohibition_covers(prohibition.action, permission.action)
    return (permission.kind == "permission"
            and prohibition.kind == "prohibition"
            and same_subject_target
            and unconditional
            and covers)

def classify_global(permission: Rule, prohibition: Rule) -> str:
    if conflict_always(permission, prohibition):
        return REPORT_URIS["Conflict"]
    # Not expected for this testcase, but kept for completeness.
    return REPORT_URIS["NoConflict"]

# --- Proof construction -------------------------------------------------------

def build_goal_oriented_proof(permission: Rule, prohibition: Rule) -> str:
    """
    Goal-oriented proof that the global activation state is Conflict.

    Goal G:
      Show that for every possible world w,
      Perm(read) and Prohib(read) both hold for the same subject and target.

    Strategy:
      1) From taxonomy: odrl:read ⊑ odrl:use.
      2) Prohibition(use) generalizes to all sub-actions ⇒ Prohibition(read).
      3) Permission(read) is explicit and unconditional.
      4) Therefore, in any world, both apply ⇒ Conflict.
    """
    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append(f"  (P1) Permission:  {permission.assignee} may {permission.action} {permission.target}.")
    lines.append(f"  (F1) Prohibition: {prohibition.assignee} must-not {prohibition.action} {prohibition.target}.")
    lines.append("  Both rules are unconditional (no constraints/refinements).")
    lines.append("")
    lines.append("Action taxonomy:")
    lines.append("  (T) odrl:read is a subclass (narrower action) of odrl:use (odrl:read ⊑ odrl:use).")
    lines.append("")
    lines.append("Derivation:")
    lines.append("  (S1) From (F1) and (T), Prohibition(odrl:use) ⇒ Prohibition(odrl:read).")
    lines.append("  (S2) From (P1), Permission(odrl:read) holds unconditionally.")
    lines.append("  (C) For any world w, Permission(odrl:read) ∧ Prohibition(odrl:read).")
    lines.append("      Since w was arbitrary, this holds for all worlds ⇒ Global state: Conflict.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    permission  = POLICY_2A
    prohibition = POLICY_2B

    result_uri = classify_global(permission, prohibition)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(permission, prohibition))

if __name__ == "__main__":
    main()

