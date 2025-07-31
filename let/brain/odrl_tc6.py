#!/usr/bin/env python3
"""
Testcase-6: Permission vs Prohibition in collection and partOf
==============================================================

Scenario (from ODRL Test Conflicts, testcase-6):
  - policy6a: Alice is permitted to odrl:read ex:collectionX (an odrl:AssetCollection).
  - policy6b: Alice is prohibited to odrl:read ex:document2.
  - Membership: ex:document2 odrl:partOf ex:collectionX.

Semantics used:
  A permission granted over a collection *lifts* to all of its members:
    Perm(read, Collection C)  and  partOf(a, C)   ⇒   Perm(read, a)

Expected global activation state: Conflict.
The permission on the collection implies permission on document2, which clashes
with the explicit prohibition on document2, unconditionally and in every world.
"""

from dataclasses import dataclass
from typing import Dict, Set, Optional, List

# --- Asset model (collections and partOf) ------------------------------------

class AssetModel:
    """
    Minimal model of collections and partOf. Supports transitive membership
    so that if a ∈ C1 and C1 ∈ C2, then a ∈ C2.
    """
    def __init__(self) -> None:
        # child -> set of direct parents (collections)
        self.part_of: Dict[str, Set[str]] = {}
        # for convenience, track known collections
        self.collections: Set[str] = set()

    def add_collection(self, cid: str) -> None:
        self.collections.add(cid)
        self.part_of.setdefault(cid, set())  # ensure key exists

    def add_part_of(self, child: str, parent_collection: str) -> None:
        self.part_of.setdefault(child, set()).add(parent_collection)
        self.add_collection(parent_collection)

    def is_member_of(self, asset: str, collection: str) -> bool:
        """
        True iff 'asset' is (transitively) partOf 'collection'.
        """
        if asset == collection:
            return True
        visited: Set[str] = set()
        frontier: List[str] = [asset]
        while frontier:
            cur = frontier.pop()
            if cur in visited:
                continue
            visited.add(cur)
            for parent in self.part_of.get(cur, ()):
                if parent == collection:
                    return True
                frontier.append(parent)
        return False

# Build the tiny asset universe from the testcase
ASSETS = AssetModel()
ASSETS.add_collection("ex:collectionX")
ASSETS.add_part_of("ex:document2", "ex:collectionX")  # document2 ∈ collectionX

# --- Policy modeling ----------------------------------------------------------

@dataclass(frozen=True)
class Rule:
    kind: str          # "permission" or "prohibition"
    assignee: str      # "ex:alice"
    action: str        # typically "odrl:read"
    target: str        # asset or collection IRI
    refinement: Optional[str] = None  # None => unconditional

# policy6a: permission to read the collection
POLICY_6A = Rule(kind="permission",  assignee="ex:alice", action="odrl:read", target="ex:collectionX")
# policy6b: prohibition to read document2
POLICY_6B = Rule(kind="prohibition", assignee="ex:alice", action="odrl:read", target="ex:document2")

# --- Report URIs --------------------------------------------------------------

REPORT_URIS = {
    "Conflict":    "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":   "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited":  "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":   "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict":  "https://w3id.org/force/compliance-report#NoConflict",
}

# --- Lifting semantics and conflict test -------------------------------------

def permission_covers_asset(permission: Rule, asset: str) -> bool:
    """
    A permission over a collection covers all its members; a permission over an
    asset covers that asset itself.
    """
    if permission.kind != "permission":
        return False
    return ASSETS.is_member_of(asset, permission.target)

def unconditional_conflict_due_to_collection(permission: Rule, prohibition: Rule) -> bool:
    """
    Returns True iff:
      - permission vs prohibition,
      - same assignee and action,
      - both unconditional,
      - and the permission's target (collection) covers the prohibition's asset.
      Then both rules apply (to the asset) in *every* possible world ⇒ Conflict.
    """
    same_subject_action = (
        permission.assignee == prohibition.assignee and
        permission.action   == prohibition.action
    )
    unconditional = (permission.refinement is None and prohibition.refinement is None)
    covers_asset = permission_covers_asset(permission, prohibition.target)
    return (permission.kind == "permission"
            and prohibition.kind == "prohibition"
            and same_subject_action
            and unconditional
            and covers_asset)

def classify_global(permission: Rule, prohibition: Rule) -> str:
    if unconditional_conflict_due_to_collection(permission, prohibition):
        return REPORT_URIS["Conflict"]
    return REPORT_URIS["NoConflict"]  # (not expected for this testcase)

# --- Goal-oriented proof construction ----------------------------------------

def build_goal_oriented_proof(permission: Rule, prohibition: Rule) -> str:
    """
    Goal G:
      Show that for every world w, Perm(read, document2) and Prohib(read, document2) both hold.

    Strategy:
      1) From asset structure: partOf(document2, collectionX).
      2) Lifting rule (collections): Perm(read, collectionX) ⇒ Perm(read, document2).
      3) Combine with explicit Prohib(read, document2).
      4) Both rules are unconditional ⇒ the clash holds in all worlds.
    """
    lines: List[str] = []
    lines.append("--- Goal-Oriented Proof Explanation ---\n")
    lines.append("Policies:")
    lines.append(f"  (P1) Permission:  {permission.assignee} may {permission.action} {permission.target}.")
    lines.append(f"  (F1) Prohibition: {prohibition.assignee} must-not {prohibition.action} {prohibition.target}.")
    lines.append("  Both rules are unconditional (no constraints/refinements).")
    lines.append("")
    lines.append("Asset structure:")
    lines.append("  (A) ex:document2 is partOf ex:collectionX.")
    lines.append("Collection lifting rule:")
    lines.append("  (L) If Perm(act, C) and partOf(a, C), then Perm(act, a).")
    lines.append("")
    lines.append("Derivation:")
    lines.append("  (S1) From (P1), Perm(read, ex:collectionX).")
    lines.append("  (S2) From (A) and (L), Perm(read, ex:document2).")
    lines.append("  (S3) From (F1), Prohib(read, ex:document2).")
    lines.append("  (C) Therefore, in any world w, Perm(read, ex:document2) ∧ Prohib(read, ex:document2).")
    lines.append("      Since w was arbitrary, the policies conflict in every possible world ⇒ Global state: Conflict.")
    return "\n".join(lines)

# --- Main ---------------------------------------------------------------------

def main() -> None:
    permission  = POLICY_6A
    prohibition = POLICY_6B

    result_uri = classify_global(permission, prohibition)

    # Always print the expected URI first (required by test harnesses)
    print(result_uri)

    # Then print a compact, goal-oriented proof explanation
    print()
    print(build_goal_oriented_proof(permission, prohibition))

if __name__ == "__main__":
    main()

