#!/usr/bin/env python3
"""
Generic explicit-step proof builder for numeric refinements (testcase-11)
=========================================================================

Testcase-11 summary:
  - Permission:  pay > 10 Euro   (unconditional apart from the numeric refinement)
  - Obligation:  pay < 10 Euro
  - Deontic reading: O(pay < T) ⇒ F(pay > T)
  - Therefore: Permission(pay > 10) clashes with Prohibition(pay > 10) in all worlds ⇒ Conflict.

This program:
  1) Provides generic scalar-interval algebra for numeric refinements (Decimal).
  2) Provides a tiny reusable proof kernel with explicit inference steps.
  3) Provides a reusable builder that:
       Premises (Perm, Obl) → Deontic Obl⇒Proh → Interval-Intersection →
       Choose witness → Universal-Elim → ∧-Intro → ∀-Intro → Classification.
  4) Instantiates the builder for testcase-11 and prints:
       - the expected URI, and
       - a pretty, numbered proof.

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from decimal import Decimal
from typing import Any, List, Optional, Tuple, Union

# -----------------------------------------------------------------------------
# Scalar (numeric) interval algebra over Decimal
# -----------------------------------------------------------------------------

Scalar = Decimal
Interval = Tuple[Optional[Scalar], bool, Optional[Scalar], bool]  # (lower, lower_closed, upper, upper_closed)

def sv(x: str) -> Scalar:
    """Scalar value from string for precision (e.g., sv('10.00'))."""
    return Decimal(x)

def iv_from_op(op: str, v: Scalar) -> Interval:
    """Build interval for comparison operator wrt scalar v."""
    if op == "lt":   return (None, False, v,     False)  # (-∞, v)
    if op == "le":   return (None, False, v,     True)   # (-∞, v]
    if op == "gt":   return (v,    False, None,  False)  # (v, +∞)
    if op == "ge":   return (v,    True,  None,  False)  # [v, +∞)
    if op == "eq":   return (v,    True,  v,     True)   # [v, v]
    raise ValueError(f"Unsupported operator: {op}")

def iv_intersection(a: Interval, b: Interval) -> Optional[Interval]:
    """Intersection of two scalar intervals; None if empty."""
    (al, alc, au, auc) = a
    (bl, blc, bu, buc) = b
    # lower
    if al is None: nl, nlc = bl, blc
    elif bl is None: nl, nlc = al, alc
    else:
        if al > bl: nl, nlc = al, alc
        elif bl > al: nl, nlc = bl, blc
        else: nl, nlc = al, alc and blc
    # upper
    if au is None: nu, nuc = bu, buc
    elif bu is None: nu, nuc = au, auc
    else:
        if au < bu: nu, nuc = au, auc
        elif bu < au: nu, nuc = bu, buc
        else: nu, nuc = au, auc and buc
    # emptiness
    if nl is not None and nu is not None:
        if nl > nu: return None
        if nl == nu and not (nlc and nuc): return None
    return (nl, nlc, nu, nuc)

def iv_equal(a: Interval, b: Interval) -> bool:
    return a == b

def iv_subset(sub: Interval, sup: Interval) -> bool:
    inter = iv_intersection(sub, sup)
    return inter is not None and iv_equal(inter, sub)

def iv_str(iv: Interval) -> str:
    (l, lc, u, uc) = iv
    ls = "-∞" if l is None else f"{l:.2f}"
    us = "+∞" if u is None else f"{u:.2f}"
    return f"{'[' if lc else '('}{ls}, {us}{']' if uc else ')'}"

def pick_witness(iv: Interval, step: Scalar = sv("0.01")) -> Scalar:
    """Deterministically pick a scalar witness inside a non-empty interval."""
    (l, lc, u, uc) = iv
    if l is not None:
        return l if lc else (l + step)
    # choose near upper if lower is -∞
    assert u is not None
    return u if uc else (u - step)

# -----------------------------------------------------------------------------
# Tiny logic: terms, formulas, pretty-printers
# -----------------------------------------------------------------------------

@dataclass(frozen=True)
class Term:
    name: str
    def __str__(self) -> str: return self.name

@dataclass(frozen=True)
class Atom:
    pred: str
    args: Tuple[Any, ...]
    def pretty(self) -> str:
        def fmt(x: Any) -> str:
            if isinstance(x, Term): return str(x)
            if isinstance(x, Decimal): return f"{x:.2f}"
            return str(x)
        return f"{self.pred}(" + ", ".join(fmt(a) for a in self.args) + ")"

@dataclass(frozen=True)
class And:
    left: Union['And', Atom]
    right: Union['And', Atom]
    def pretty(self) -> str:
        L = self.left.pretty() if hasattr(self.left, "pretty") else str(self.left)
        R = self.right.pretty() if hasattr(self.right, "pretty") else str(self.right)
        return f"({L} ∧ {R})"

@dataclass(frozen=True)
class ForAllInInterval:
    var: Term
    interval: Interval
    body: Atom
    def pretty(self) -> str:
        return f"∀{self.var}∈{iv_str(self.interval)} {self.body.pretty()}"

# -----------------------------------------------------------------------------
# Proof kernel (pretty output only)
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        k, p = self.kind, self.payload
        if k == "formula":       return p.pretty()
        if k == "interval-eq":   return f"{iv_str(p[0])} ∩ {iv_str(p[1])} = {iv_str(p[2])}"
        if k == "interval-def":  return f"{p[0]} := {iv_str(p[1])}"
        if k == "membership":    return f"{p[0]:.2f} ∈ {iv_str(p[1])}"
        if k == "classification":return f"Global activation state = {p}"
        if k == "deontic-map":   return f"O({p['obl']}) ⇒ F({p['proh']})"
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
# Reusable builders
# -----------------------------------------------------------------------------

def forall_perm_amount(var: Term, interval: Interval, action: str, target: str, unit: str) -> ForAllInInterval:
    return ForAllInInterval(var, interval, Atom("Perm", (action, target, var, unit)))

def forall_obl_amount(var: Term, interval: Interval, action: str, target: str, unit: str) -> ForAllInInterval:
    return ForAllInInterval(var, interval, Atom("Obl", (action, target, var, unit)))

def forall_proh_amount(var: Term, interval: Interval, action: str, target: str, unit: str) -> ForAllInInterval:
    return ForAllInInterval(var, interval, Atom("Proh", (action, target, var, unit)))

def deontic_obl_to_proh_interval(ob_op: str, ob_value: Scalar) -> Interval:
    """
    Deontic mapping on numeric refinements (generic):
      O(< T)  ⇒ F(> T)
      O(≤ T)  ⇒ F(> T)
      O(> T)  ⇒ F(< T)
      O(≥ T)  ⇒ F(< T)
      O(= T)  ⇒ F(≠ T)  (we approximate by F(> T) for single-interval reasoning)
    """
    if ob_op in ("lt", "le"): return iv_from_op("gt", ob_value)
    if ob_op in ("gt", "ge"): return iv_from_op("lt", ob_value)
    if ob_op == "eq":         return iv_from_op("gt", ob_value)  # simplification for single-interval engine
    raise ValueError(f"Unsupported obligation operator for deontic map: {ob_op}")

REPORT_URIS = {
    "Conflict":   "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":  "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited": "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":  "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict": "https://w3id.org/force/compliance-report#NoConflict",
}

def classify_overlap(perm_iv: Interval, proh_iv: Interval) -> Tuple[str, Optional[Interval], str]:
    """Classification for numeric-refinement clashes based on interval overlap."""
    inter = iv_intersection(perm_iv, proh_iv)
    if inter is None:
        return (REPORT_URIS["NoConflict"], None, "empty")
    if iv_subset(perm_iv, proh_iv):
        return (REPORT_URIS["Conflict"], inter, "subset_conflict")
    if iv_subset(proh_iv, perm_iv):
        return (REPORT_URIS["Ambiguous"], inter, "proh_subset_ambiguous")
    return (REPORT_URIS["Ambiguous"], inter, "partial_ambiguous")

def build_obligation_conflict_proof(
    action: str, target: str, unit: str,
    perm_op: str, perm_value: Scalar,
    obl_op: str,  obl_value: Scalar,
    var_name: str = "a", overlap_name: str = "I"
) -> Tuple[Proof, str]:
    """
    Generic builder for: Permission(op₁, V₁) vs Obligation(op₂, V₂),
    with deontic Obl⇒Proh mapping and interval overlap proof.
    """
    proof = Proof()
    a = Term(var_name)

    # Permission premise
    perm_iv = iv_from_op(perm_op, perm_value)
    perm_prem = forall_perm_amount(a, perm_iv, action, target, unit)
    s1 = proof.add("Premise", [], Conclusion("formula", perm_prem),
                   notes=f"Permission over {iv_str(perm_iv)}")

    # Obligation premise
    obl_iv = iv_from_op(obl_op, obl_value)
    obl_prem = forall_obl_amount(a, obl_iv, action, target, unit)
    s2 = proof.add("Premise", [], Conclusion("formula", obl_prem),
                   notes=f"Obligation over {iv_str(obl_iv)}")

    # Deontic: Obl ⇒ Proh (as a derived universal)
    proh_iv = deontic_obl_to_proh_interval(obl_op, obl_value)
    proh_prem = forall_proh_amount(a, proh_iv, action, target, unit)
    s3 = proof.add("Deontic Obl⇒Proh", [s2],
                   Conclusion("deontic-map", {"obl": f"{action} ∈ {iv_str(obl_iv)}",
                                              "proh": f"{action} ∈ {iv_str(proh_iv)}"}),
                   notes="From obligation refinement derive a prohibition refinement")
    s4 = proof.add("Derive-Universal", [s3], Conclusion("formula", proh_prem),
                   notes="General prohibition premise induced by the obligation")

    # Overlap of permission vs (derived) prohibition
    inter = iv_intersection(perm_iv, proh_iv)
    assert inter is not None, "Testcase-11 expects a non-empty overlap"
    s5 = proof.add("Interval-Intersection", [], Conclusion("interval-eq", (perm_iv, proh_iv, inter)),
                   notes="Overlap of permission and derived prohibition refinements")

    # Name the overlap
    s6 = proof.add("Definition", [s5], Conclusion("interval-def", (overlap_name, inter)),
                   notes=f"Name the overlap interval as {overlap_name}")

    # Choose witness amount in overlap
    w = pick_witness(inter)
    s7 = proof.add("Choose", [s6], Conclusion("membership", (w, inter)),
                   notes=f"Witness amount a0 = {w:.2f} {unit}")

    # UE: Permission at witness
    inst_perm = Atom("Perm", (action, target, w, unit))
    s8 = proof.add("Universal-Elim", [s1, s7], Conclusion("formula", inst_perm),
                   notes="Instantiate permission at the witness")

    # UE: Prohibition at witness
    inst_proh = Atom("Proh", (action, target, w, unit))
    s9 = proof.add("Universal-Elim", [s4, s7], Conclusion("formula", inst_proh),
                   notes="Instantiate prohibition at the witness")

    # ∧-Intro
    conj = And(inst_perm, inst_proh)
    s10 = proof.add("And-Intro", [s8, s9], Conclusion("formula", conj),
                    notes="Clash at the witness amount")

    # ∀-Intro over overlap
    general = ForAllInInterval(a, inter, And(
        Atom("Perm", (action, target, a, unit)),
        Atom("Proh", (action, target, a, unit))
    ))
    s11 = proof.add("ForAll-Intro", [s10], Conclusion("formula", general),
                    notes=f"Generalize the clash to all amounts in {overlap_name}")

    # Classification
    result_uri, _, mode = classify_overlap(perm_iv, proh_iv)
    reasoning = {
        "subset_conflict":       f"{overlap_name} equals the entire permission refinement ⇒ standing clash",
        "partial_ambiguous":     f"{overlap_name} is a strict partial overlap ⇒ some amounts permit, some prohibit",
        "proh_subset_ambiguous": f"Prohibition ⊆ Permission ⇒ both clash (inside {overlap_name}) and permit-only amounts (outside)",
        "empty":                 "No shared amounts ⇒ no direct clash"
    }[mode]
    proof.add("Classification", [s11], Conclusion("classification", result_uri), notes=reasoning)

    return proof, result_uri

# -----------------------------------------------------------------------------
# Demonstration: instantiate for testcase-11
# -----------------------------------------------------------------------------

def main() -> None:
    # Action/target/unit labels
    action = "odrl:pay"
    target = "ex:resourceX"
    unit   = "dbpedia:Euro"

    # Refinements from the testcase:
    #   Permission: pay > 10.00
    #   Obligation: pay < 10.00  ⇒ (Deontic) Prohibition: pay > 10.00
    perm_op, perm_val = "gt", sv("10.00")
    obl_op,  obl_val  = "lt", sv("10.00")

    proof, result_uri = build_obligation_conflict_proof(
        action=action, target=target, unit=unit,
        perm_op=perm_op, perm_value=perm_val,
        obl_op=obl_op,  obl_value=obl_val,
        var_name="a", overlap_name="I"
    )

    # 1) Expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

