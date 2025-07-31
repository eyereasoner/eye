#!/usr/bin/env python3
"""
Generic explicit-step proof builder: Overlapping collections (testcase-7)
=======================================================================

Testcase-7 summary:
  - Unconditional Permission: ex:alice may odrl:read ex:collectionX.
  - Unconditional Prohibition: ex:alice must-not odrl:read ex:collectionY.
  - Overlap: every member of collectionX is also a member of collectionY
             (Y may have extra members).

Generic rules (Collection Lifting):
  (L1) Perm(act, C, w) ∧ partOf(a, C) ⇒ Perm(act, a, w)
  (L2) Proh(act, C, w) ∧ partOf(a, C) ⇒ Proh(act, a, w)

Therefore, pick any asset a ∈ Members(X) ∩ Members(Y). In every world w:
  Perm(read, a, w) (by L1) and Proh(read, a, w) (by L2) ⇒ Conflict.

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
        if k == "members-fact":    return f"Members({p[0]}) = [{', '.join(sorted(p[1]))}]"
        if k == "overlap-fact":    return f"Overlap = [{', '.join(sorted(p))}]"
        if k == "membership-fact": return f"partOf({p[0]}, {p[1]})"
        if k == "lifting-rule-Perm": return "Perm(act, C, w) ∧ partOf(a, C) ⇒ Perm(act, a, w)"
        if k == "lifting-rule-Proh": return "Proh(act, C, w) ∧ partOf(a, C) ⇒ Proh(act, a, w)"
        if k == "choose-asset":    return f"{p[0]} ∈ Overlap (witness asset)"
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
        self.nodes: Set[str] = set()

    def add_asset(self, aid: str) -> None:
        self.nodes.add(aid)
        self.parents.setdefault(aid, set())

    def add_part_of(self, child: str, parent_collection: str) -> None:
        self.add_asset(child)
        self.add_asset(parent_collection)
        self.parents[child].add(parent_collection)

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

    def members_of(self, collection: str) -> List[str]:
        """All nodes that are (transitively) partOf the given collection (excluding the collection itself)."""
        return sorted([n for n in self.nodes if n != collection and self.is_member_of(n, collection)])

    def overlap_members(self, c1: str, c2: str) -> List[str]:
        m1 = set(self.members_of(c1))
        m2 = set(self.members_of(c2))
        return sorted(m1.intersection(m2))

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
# Generic proof builder: Perm on X, Proh on Y, with Overlap(X, Y) ≠ ∅
# -----------------------------------------------------------------------------

def build_overlapping_collections_conflict_proof(
    assignee: str,
    action: str,
    collection_perm: str,
    collection_proh: str,
    assets: AssetModel,
    universe_name: str = "W",
    witness_name: str = "w0"
) -> Tuple[Proof, str]:
    """
    Construct an explicit proof that:
      ∀w Perm(action, X, w) and ∀w Proh(action, Y, w),
      with Overlap := Members(X) ∩ Members(Y) non-empty,
    entail Conflict via collection lifting on any witness asset a ∈ Overlap.
    """
    members_X = assets.members_of(collection_perm)
    members_Y = assets.members_of(collection_proh)
    overlap = assets.overlap_members(collection_perm, collection_proh)
    if not overlap:
        raise ValueError("Collections have no overlapping members; this builder expects a non-empty overlap.")

    proof = Proof()
    w = Term("w")

    # [1] Unconditional Permission on collection X
    perm_prem = forall_perm_unconditional(w, assignee, action, collection_perm)
    s1 = proof.add("Premise", [], Conclusion("formula", perm_prem),
                   notes=f"Unconditional Permission on {collection_perm}")

    # [2] Unconditional Prohibition on collection Y
    proh_prem = forall_proh_unconditional(w, assignee, action, collection_proh)
    s2 = proof.add("Premise", [], Conclusion("formula", proh_prem),
                   notes=f"Unconditional Prohibition on {collection_proh}")

    # [3] Members facts and Overlap
    s3 = proof.add("Members-Fact", [], Conclusion("members-fact", (collection_perm, members_X)),
                   notes="All assets in the permission collection")
    s4 = proof.add("Members-Fact", [], Conclusion("members-fact", (collection_proh, members_Y)),
                   notes="All assets in the prohibition collection")
    s5 = proof.add("Overlap-Fact", [s3, s4], Conclusion("overlap-fact", overlap),
                   notes="Non-empty intersection of members")

    # [4] Collection-lifting rules (Perm and Proh)
    s6 = proof.add("Collection-Lifting-Rule (Perm)", [],
                   Conclusion("lifting-rule-Perm", None),
                   notes="From Perm(act, C, w) and partOf(a, C) infer Perm(act, a, w)")
    s7 = proof.add("Collection-Lifting-Rule (Proh)", [],
                   Conclusion("lifting-rule-Proh", None),
                   notes="From Proh(act, C, w) and partOf(a, C) infer Proh(act, a, w)")

    # [5] Universe of worlds (unrestricted)
    s8 = proof.add("Universe-Def", [], Conclusion("universe-def", (universe_name, "all possible worlds")),
                   notes="No constraints/refinements restrict applicability")

    # [6] Choose an arbitrary world w0 ∈ W
    s9 = proof.add("Arbitrary-World", [s8], Conclusion("choose-world", (witness_name, universe_name)),
                   notes="World chosen arbitrarily; argument must not depend on which one")

    # [7] Choose a witness asset a ∈ Overlap
    a = overlap[0]
    s10 = proof.add("Choose-Asset", [s5], Conclusion("choose-asset", (a,)),
                    notes=f"Pick witness asset a = {a}")

    # [8] UE: Perm(action, X, w0)
    inst_perm_X = Atom("Perm", (assignee, action, collection_perm, witness_name))
    s11 = proof.add("Universal-Elim", [s1, s9], Conclusion("formula", inst_perm_X),
                    notes=f"Instantiate collection permission at {witness_name}")

    # [9] Lift Perm from X to asset a using partOf(a, X)
    inst_perm_a = Atom("Perm", (assignee, action, a, witness_name))
    s12 = proof.add("Lifting-Apply (Perm)", [s6, s11, s10],
                    Conclusion("formula", inst_perm_a),
                    notes=f"Apply lifting via partOf({a}, {collection_perm})")

    # [10] UE: Proh(action, Y, w0)
    inst_proh_Y = Atom("Proh", (assignee, action, collection_proh, witness_name))
    s13 = proof.add("Universal-Elim", [s2, s9], Conclusion("formula", inst_proh_Y),
                    notes=f"Instantiate collection prohibition at {witness_name}")

    # [11] Lift Proh from Y to asset a using partOf(a, Y)
    inst_proh_a = Atom("Proh", (assignee, action, a, witness_name))
    s14 = proof.add("Lifting-Apply (Proh)", [s7, s13, s10],
                    Conclusion("formula", inst_proh_a),
                    notes=f"Apply lifting via partOf({a}, {collection_proh})")

    # [12] ∧-Introduction: Perm(a) ∧ Proh(a) at w0
    conj = And(inst_perm_a, inst_proh_a)
    s15 = proof.add("And-Intro", [s12, s14], Conclusion("formula", conj),
                    notes="Clash at the arbitrary world on the same asset")

    # [13] ∀-Introduction: generalize clash to all worlds
    general = ForAll(w, And(
        Atom("Perm", (assignee, action, a, w)),
        Atom("Proh", (assignee, action, a, w))
    ))
    s16 = proof.add("ForAll-Intro", [s15], Conclusion("formula", general),
                    notes=f"Since {witness_name} was arbitrary, the clash holds in all worlds")

    # [14] Classification: Conflict
    result_uri = REPORT_URIS["Conflict"]
    proof.add("Classification", [s16], Conclusion("classification", result_uri),
              notes="Unconditional rules + factual overlap ⇒ standing clash in every world")

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-7
# -----------------------------------------------------------------------------

def main() -> None:
    assignee   = "ex:alice"
    action     = "odrl:read"
    coll_X     = "ex:collectionX"
    coll_Y     = "ex:collectionY"

    # Build an asset model where Members(X) ⊆ Members(Y) and Y has an extra member
    AM = AssetModel()
    # Members of X
    AM.add_part_of("ex:document1", coll_X)
    AM.add_part_of("ex:document2", coll_X)
    # Mirror those into Y + one extra
    AM.add_part_of("ex:document1", coll_Y)
    AM.add_part_of("ex:document2", coll_Y)
    AM.add_part_of("ex:document3", coll_Y)  # extra member in Y

    proof, result_uri = build_overlapping_collections_conflict_proof(
        assignee=assignee,
        action=action,
        collection_perm=coll_X,
        collection_proh=coll_Y,
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

