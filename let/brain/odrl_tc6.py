#!/usr/bin/env python3
"""
Generic explicit-step proof builder: Collection permission vs asset prohibition
==============================================================================

Testcase-6 summary:
  - Unconditional Permission: ex:alice may odrl:read ex:collectionX.
  - Unconditional Prohibition: ex:alice must-not odrl:read ex:document2.
  - Membership: ex:document2 odrl:partOf ex:collectionX (transitively).

Generic rule (Collection-Lifting):
  If Perm(act, C, w) and partOf(a, C) then Perm(act, a, w).

Therefore, in every world w:
  Perm(read, collectionX, w) and partOf(document2, collectionX)
  entail Perm(read, document2, w), which clashes with Proh(read, document2, w)
  ⇒ Global state: Conflict.

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set, Tuple

# -----------------------------------------------------------------------------
# Tiny logic: terms, formulas, pretty-printers (generic)
# -----------------------------------------------------------------------------

@dataclass(frozen=True)
class Term:
    name: str
    def __str__(self) -> str: return self.name

@dataclass(frozen=True)
class Atom:
    pred: str
    args: Tuple[Any, ...]  # e.g., (assignee, action, target, world)
    def pretty(self) -> str:
        def fmt(x: Any) -> str: return x if isinstance(x, str) else str(x)
        return f"{self.pred}(" + ", ".join(fmt(a) for a in self.args) + ")"

@dataclass(frozen=True)
class And:
    left: Any
    right: Any
    def pretty(self) -> str:
        L = self.left.pretty() if hasattr(self.left, "pretty") else str(self.left)
        R = self.right.pretty() if hasattr(self.right, "pretty") else str(self.right)
        return f"({L} ∧ {R})"

@dataclass(frozen=True)
class ForAll:
    var: Term
    body: Any  # usually an Atom referring to var
    def pretty(self) -> str:
        return f"∀{self.var}. {self.body.pretty()}"

# -----------------------------------------------------------------------------
# Proof kernel (pretty output only)
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        k, p = self.kind, self.payload
        if k == "formula":         return p.pretty()
        if k == "universe-def":    return f"{p[0]} := {p[1]}"
        if k == "choose-world":    return f"{p[0]} ∈ {p[1]} (arbitrary)"
        if k == "membership-fact": return f"partOf({p[0]}, {p[1]})"
        if k == "lifting-rule":    return f"Perm(act, C, w) ∧ partOf(a, C) ⇒ Perm(act, a, w)"
        if k == "classification":  return f"Global activation state = {p}"
        return str(p)

@dataclass
class Step:
    id: int
    rule: str
    premises: List[int]
    conclusion: Conclusion
    notes: Optional[str] = None

@dataclass
class Proof:
    steps: List[Step] = field(default_factory=list)
    def add(self, rule: str, premises: List[int], conclusion: Conclusion, notes: Optional[str] = None) -> int:
        sid = len(self.steps) + 1
        self.steps.append(Step(sid, rule, premises, conclusion, notes))
        return sid
    def pretty(self) -> str:
        lines = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f"  // {s.notes}" if s.notes else ""
            lines.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(lines)

# -----------------------------------------------------------------------------
# Asset/collection model with transitive partOf (generic, reusable)
# -----------------------------------------------------------------------------

class AssetModel:
    def __init__(self) -> None:
        # child -> set of direct parents (collections)
        self.parents: Dict[str, Set[str]] = {}

    def add_part_of(self, child: str, parent_collection: str) -> None:
        self.parents.setdefault(child, set()).add(parent_collection)
        self.parents.setdefault(parent_collection, set())  # ensure key exists

    def is_member_of(self, asset: str, collection: str) -> bool:
        """True iff asset is (transitively) partOf collection (reflexive for equality)."""
        if asset == collection:
            return True
        visited: Set[str] = set()
        stack: List[str] = [asset]
        while stack:
            cur = stack.pop()
            if cur in visited:
                continue
            visited.add(cur)
            for parent in self.parents.get(cur, ()):
                if parent == collection:
                    return True
                stack.append(parent)
        return False

# -----------------------------------------------------------------------------
# Reusable builders for unconditional premises
# -----------------------------------------------------------------------------

def forall_perm_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    # Unconditional: holds for all worlds
    return ForAll(var, Atom("Perm", (assignee, action, target, var)))

def forall_proh_unconditional(var: Term, assignee: str, action: str, target: str) -> ForAll:
    # Unconditional: holds for all worlds
    return ForAll(var, Atom("Proh", (assignee, action, target, var)))

# -----------------------------------------------------------------------------
# Report URIs
# -----------------------------------------------------------------------------

REPORT_URIS = {
    "Conflict":   "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":  "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited": "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":  "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict": "https://w3id.org/force/compliance-report#NoConflict",
}

# -----------------------------------------------------------------------------
# Generic proof builder: Permission on collection, Prohibition on member
# -----------------------------------------------------------------------------

def build_collection_lifting_conflict_proof(
    assignee: str,
    action: str,
    collection: str,
    asset: str,
    assets: AssetModel,
    universe_name: str = "W",
    witness_name: str = "w0"
) -> Tuple[Proof, str]:
    """
    Construct an explicit proof that:
      ∀w Perm(action, collection, w),
      ∀w Proh(action, asset, w),
      and partOf(asset, collection)
    entail Conflict via collection-lifting.
    """
    if not assets.is_member_of(asset, collection):
        raise ValueError(f"{asset} is not partOf {collection} in the asset model.")

    proof = Proof()
    w = Term("w")

    # [1] Unconditional Permission on the collection
    perm_prem = forall_perm_unconditional(w, assignee, action, collection)
    s1 = proof.add("Premise", [], Conclusion("formula", perm_prem),
                   notes=f"Unconditional Permission on collection {collection}")

    # [2] Unconditional Prohibition on the asset
    proh_prem = forall_proh_unconditional(w, assignee, action, asset)
    s2 = proof.add("Premise", [], Conclusion("formula", proh_prem),
                   notes=f"Unconditional Prohibition on asset {asset}")

    # [3] Membership fact: partOf(asset, collection)
    s3 = proof.add("Membership-Fact", [], Conclusion("membership-fact", (asset, collection)),
                   notes="Given by the asset/collection structure (transitive)")

    # [4] Collection-lifting rule
    s4 = proof.add("Collection-Lifting-Rule", [s3],
                   Conclusion("lifting-rule", None),
                   notes="From Perm(act, C, w) and partOf(a, C) infer Perm(act, a, w)")

    # [5] Universe of worlds (unrestricted)
    s5 = proof.add("Universe-Def", [], Conclusion("universe-def", (universe_name, "all possible worlds")),
                   notes="No constraints/refinements restrict applicability")

    # [6] Choose an arbitrary world w0 ∈ W
    s6 = proof.add("Arbitrary-World", [s5], Conclusion("choose-world", (witness_name, universe_name)),
                   notes="World chosen arbitrarily; argument must not depend on which one")

    # [7] UE: Perm(action, collection, w0)
    inst_perm_coll = Atom("Perm", (assignee, action, collection, witness_name))
    s7 = proof.add("Universal-Elim", [s1, s6], Conclusion("formula", inst_perm_coll),
                   notes=f"Instantiate collection permission at {witness_name}")

    # [8] Lifting-Apply: from [7] and partOf derive Perm(action, asset, w0)
    inst_perm_asset = Atom("Perm", (assignee, action, asset, witness_name))
    s8 = proof.add("Lifting-Apply", [s4, s7], Conclusion("formula", inst_perm_asset),
                   notes="Apply collection lifting to the member asset")

    # [9] UE: Proh(action, asset, w0)
    inst_proh_asset = Atom("Proh", (assignee, action, asset, witness_name))
    s9 = proof.add("Universal-Elim", [s2, s6], Conclusion("formula", inst_proh_asset),
                   notes="Instantiate asset prohibition at the same world")

    # [10] ∧-Introduction: Perm(asset) ∧ Proh(asset) at w0
    conj = And(inst_perm_asset, inst_proh_asset)
    s10 = proof.add("And-Intro", [s8, s9], Conclusion("formula", conj),
                    notes="Clash at the arbitrary world on the same asset")

    # [11] ∀-Introduction: generalize clash to all worlds
    general = ForAll(w, And(
        Atom("Perm", (assignee, action, asset, w)),
        Atom("Proh", (assignee, action, asset, w))
    ))
    s11 = proof.add("ForAll-Intro", [s10], Conclusion("formula", general),
                    notes="Since w0 was arbitrary, the conjunction holds in all worlds")

    # [12] Classification: Conflict
    result_uri = REPORT_URIS["Conflict"]
    proof.add("Classification", [s11], Conclusion("classification", result_uri),
              notes="Permission and Prohibition co-hold on the same asset in every world")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-6
# -----------------------------------------------------------------------------

def main() -> None:
    assignee   = "ex:alice"
    action     = "odrl:read"
    collection = "ex:collectionX"
    asset      = "ex:document2"

    # Build the asset model for the testcase
    AM = AssetModel()
    AM.add_part_of("ex:document2", "ex:collectionX")  # document2 ∈ collectionX

    proof, result_uri = build_collection_lifting_conflict_proof(
        assignee=assignee,
        action=action,
        collection=collection,
        asset=asset,
        assets=AM,
        universe_name="W",
        witness_name="w0"
    )

    # 1) Expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

