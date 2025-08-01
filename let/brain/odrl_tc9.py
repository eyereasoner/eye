#!/usr/bin/env python3
"""
Generic explicit-step proof builder for interval-overlap conflicts
==================================================================

This program demonstrates the generic proof builder on ODRL Test Conflicts
*testcase-9*:

  - Permission:  odrl:read ex:resourceX for all dates d in [2025-01-01, 2025-12-31].
  - Prohibition: odrl:read ex:resourceX for all dates d in (-∞, 2026-01-01).

Expected global activation state: Conflict
  Because the permission window is fully contained in the prohibition window.

The proof builder is reusable:
  - Provide any two activation intervals (permission & prohibition), plus action/target labels.
  - It constructs a general proof:
      Premises  → Interval-Intersection → Choose witness → Universal-Elim (both)
      → And-Intro → ForAll-Intro over the overlap → Classification
  - Classification is generic:
      * NoConflict  if overlap is empty
      * Conflict    if Permission ⊆ Prohibition (standing clash on all permitted points)
      * Ambiguous   otherwise (there exist permitted-only and/or prohibited-only points)

No external libraries required.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from datetime import date, timedelta
from typing import List, Optional, Tuple, Union, Any

# -----------------------------------------------------------------------------
# Interval algebra (generic and reusable)
# -----------------------------------------------------------------------------

DateInterval = Tuple[Optional[date], bool, Optional[date], bool]  # (lower, lower_closed, upper, upper_closed)

def iv(op: str, v: date) -> DateInterval:
    """Construct a half-open/closed interval from a comparison operator."""
    if op == "lt":   return (None, False, v,     False)  # (-∞, v)
    if op == "lteq": return (None, False, v,     True)   # (-∞, v]
    if op == "gt":   return (v,    False, None,  False)  # (v, +∞)
    if op == "gteq": return (v,    True,  None,  False)  # [v, +∞)
    if op == "eq":   return (v,    True,  v,     True)   # [v, v]
    raise ValueError(f"Unsupported op: {op}")

def iv_intersection(a: DateInterval, b: DateInterval) -> Optional[DateInterval]:
    (al, alc, au, auc) = a
    (bl, blc, bu, buc) = b
    # Lower
    if al is None: nl, nlc = bl, blc
    elif bl is None: nl, nlc = al, alc
    else:
        if al > bl: nl, nlc = al, alc
        elif bl > al: nl, nlc = bl, blc
        else: nl, nlc = al, alc and blc
    # Upper
    if au is None: nu, nuc = bu, buc
    elif bu is None: nu, nuc = au, auc
    else:
        if au < bu: nu, nuc = au, auc
        elif bu < au: nu, nuc = bu, buc
        else: nu, nuc = au, auc and buc
    # Empty?
    if nl is not None and nu is not None:
        if nl > nu: return None
        if nl == nu and not (nlc and nuc): return None
    return (nl, nlc, nu, nuc)

def iv_equal(a: DateInterval, b: DateInterval) -> bool:
    return a == b  # tuple equality (including open/closed flags)

def iv_subset(sub: DateInterval, sup: DateInterval) -> bool:
    """Return True iff 'sub ⊆ sup' (using set-wise equality via intersection)."""
    inter = iv_intersection(sub, sup)
    return inter is not None and iv_equal(inter, sub)

def iv_str(ivl: DateInterval) -> str:
    (l, lc, u, uc) = ivl
    ls = "-∞" if l is None else l.isoformat()
    us = "+∞" if u is None else u.isoformat()
    return f"{'[' if lc else '('}{ls}, {us}{']' if uc else ')'}"

def pick_witness(ivl: DateInterval) -> date:
    """Pick a deterministic witness point inside a non-empty interval."""
    (l, lc, u, uc) = ivl
    if l is not None:
        return l if lc else (l + timedelta(days=1))
    # l = -∞, so choose near upper
    assert u is not None
    return u if uc else (u - timedelta(days=1))

# -----------------------------------------------------------------------------
# Tiny logic: terms, formulas, and pretty-printer (generic)
# -----------------------------------------------------------------------------

@dataclass(frozen=True)
class Term:
    name: str
    def __str__(self) -> str: return self.name

@dataclass(frozen=True)
class Atom:
    pred: str
    args: Tuple[Union[str, Term, date], ...]
    def pretty(self) -> str:
        def fmt(a: Union[str, Term, date]) -> str:
            if isinstance(a, Term): return str(a)
            if isinstance(a, date): return a.isoformat()
            return a
        return f"{self.pred}(" + ", ".join(fmt(x) for x in self.args) + ")"

@dataclass(frozen=True)
class And:
    left: Union['And', Atom]
    right: Union['And', Atom]
    def pretty(self) -> str:
        L = self.left.pretty() if isinstance(self.left, And) else self.left.pretty()
        R = self.right.pretty() if isinstance(self.right, And) else self.right.pretty()
        return f"({L} ∧ {R})"

@dataclass(frozen=True)
class ForAllInInterval:
    var: Term
    interval: DateInterval
    body: Atom
    def pretty(self) -> str:
        return f"∀{self.var}∈{iv_str(self.interval)} {self.body.pretty()}"

# -----------------------------------------------------------------------------
# Proof kernel (generic, reusable)
# -----------------------------------------------------------------------------

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        k, p = self.kind, self.payload
        if k == "formula":       return p.pretty()
        if k == "interval-eq":   return f"{iv_str(p[0])} ∩ {iv_str(p[1])} = {iv_str(p[2])}"
        if k == "membership":    return f"{(p[0].isoformat() if isinstance(p[0], date) else str(p[0]))} ∈ {iv_str(p[1])}"
        if k == "interval-def":  return f"{p[0]} := {iv_str(p[1])}"
        if k == "classification":return f"Global activation state = {p}"
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
# Reusable builders for permission/prohibition premises over an interval
# -----------------------------------------------------------------------------

def forall_perm(var: Term, interval: DateInterval, action: str, target: str) -> ForAllInInterval:
    return ForAllInInterval(var, interval, Atom("Perm", (action, target, var)))

def forall_proh(var: Term, interval: DateInterval, action: str, target: str) -> ForAllInInterval:
    return ForAllInInterval(var, interval, Atom("Proh", (action, target, var)))

# -----------------------------------------------------------------------------
# Generic overlap proof & classification
# -----------------------------------------------------------------------------

REPORT_URIS = {
    "Conflict":   "https://w3id.org/force/compliance-report#Conflict",
    "Ambiguous":  "https://w3id.org/force/compliance-report#Ambiguous",
    "Prohibited": "https://w3id.org/force/compliance-report#Prohibited",
    "Permitted":  "https://w3id.org/force/compliance-report#Permitted",
    "NoConflict": "https://w3id.org/force/compliance-report#NoConflict",
}

@dataclass
class OverlapResult:
    uri: str
    overlap: Optional[DateInterval]
    mode: str  # "empty", "subset_conflict", "partial_ambiguous", "proh_subset_ambiguous"

def classify_overlap(perm_iv: DateInterval, proh_iv: DateInterval) -> OverlapResult:
    inter = iv_intersection(perm_iv, proh_iv)
    if inter is None:
        return OverlapResult(REPORT_URIS["NoConflict"], None, "empty")
    if iv_subset(perm_iv, proh_iv):
        # Every permitted point is also prohibited → standing clash over all permitted points
        return OverlapResult(REPORT_URIS["Conflict"], inter, "subset_conflict")
    if iv_subset(proh_iv, perm_iv):
        # Prohibition is narrower than permission: there are points with Perm∧Proh (in the overlap)
        # and points with Perm-only (outside). Globally: some worlds permit, some prohibit ⇒ Ambiguous.
        return OverlapResult(REPORT_URIS["Ambiguous"], inter, "proh_subset_ambiguous")
    # Partial overlap each way: there exist Perm-only, Proh-only, and Perm∧Proh points ⇒ Ambiguous.
    return OverlapResult(REPORT_URIS["Ambiguous"], inter, "partial_ambiguous")

def build_overlap_proof(perm_prem: ForAllInInterval,
                        proh_prem: ForAllInInterval,
                        action: str,
                        target: str,
                        interval_name: str = "I",
                        witness_label: str = "d0") -> Tuple[Proof, str]:
    """
    Generic proof builder for any Permission/Prohibition over intervals.
    Produces: Premises → Intersection → Definition → Choose → UE/UE → ∧-Intro → ∀-Intro → Classification.
    """
    proof = Proof()

    # [1] Premise: ∀ var ∈ PermIV  Perm(action, target, var)
    s1 = proof.add("Premise", [], Conclusion("formula", perm_prem),
                   notes="Permission premise over an activation interval")

    # [2] Premise: ∀ var ∈ ProhIV  Proh(action, target, var)
    s2 = proof.add("Premise", [], Conclusion("formula", proh_prem),
                   notes="Prohibition premise over an activation interval")

    perm_iv = perm_prem.interval
    proh_iv = proh_prem.interval

    # [3] Interval-Intersection
    inter = iv_intersection(perm_iv, proh_iv)
    if inter is None:
        s3 = proof.add("Interval-Intersection", [], Conclusion("interval-eq", (perm_iv, proh_iv, iv("eq", date.min))),  # dummy [min,min] just to pretty-print
                       notes="Overlap is empty")
        # [4] Classification: NoConflict
        res = classify_overlap(perm_iv, proh_iv)
        proof.add("Classification", [s3], Conclusion("classification", res.uri),
                  notes="No shared activation dates ⇒ no direct clash")
        return proof, res.uri

    s3 = proof.add("Interval-Intersection", [], Conclusion("interval-eq", (perm_iv, proh_iv, inter)),
                   notes="Compute overlap of activation windows")

    # [4] Definition: interval_name := inter
    s4 = proof.add("Definition", [s3], Conclusion("interval-def", (interval_name, inter)),
                   notes=f"Name the overlap interval as {interval_name}")

    # [5] Choose witness in the overlap
    w = pick_witness(inter)
    s5 = proof.add("Choose", [s4], Conclusion("membership", (w, inter)),
                   notes=f"Witness {witness_label} = {w.isoformat()} ∈ {interval_name}")

    # [6] Universal-Elim on permission premise with witness
    inst_perm = Atom("Perm", (action, target, w))
    s6 = proof.add("Universal-Elim", [s1, s5], Conclusion("formula", inst_perm),
                   notes="Instantiate permission at the witness")

    # [7] Universal-Elim on prohibition premise with witness
    inst_proh = Atom("Proh", (action, target, w))
    s7 = proof.add("Universal-Elim", [s2, s5], Conclusion("formula", inst_proh),
                   notes="Instantiate prohibition at the witness")

    # [8] And-Intro: Perm(...) ∧ Proh(...)
    conj = And(inst_perm, inst_proh)
    s8 = proof.add("And-Intro", [s6, s7], Conclusion("formula", conj),
                   notes="Clash at the witness point")

    # [9] ForAll-Intro over the overlap
    var = perm_prem.var  # they share the same var symbol by construction
    generalized = ForAllInInterval(var, inter, And(Atom("Perm", (action, target, var)),
                                                   Atom("Proh", (action, target, var))))
    s9 = proof.add("ForAll-Intro", [s8], Conclusion("formula", generalized),
                   notes=f"Generalize the clash over all {interval_name}")

    # [10] Classification
    res = classify_overlap(perm_iv, proh_iv)
    reasoning = {
        "subset_conflict":       f"{interval_name} equals the entire permission window ⇒ standing clash",
        "partial_ambiguous":     f"{interval_name} is a strict partial overlap ⇒ some worlds permit, some prohibit",
        "proh_subset_ambiguous": f"Prohibition ⊆ Permission ⇒ both clash (inside {interval_name}) and permit-only worlds (outside)"
    }.get(res.mode, "Classification by overlap relation")
    proof.add("Classification", [s9], Conclusion("classification", res.uri), notes=reasoning)

    return proof, res.uri

# -----------------------------------------------------------------------------
# Demonstration: testcase-9 plugged into the generic builder
# -----------------------------------------------------------------------------

def main() -> None:
    # Testcase-9 intervals
    PERM_IV = iv_intersection(iv("gteq", date.fromisoformat("2025-01-01")),
                              iv("lteq", date.fromisoformat("2025-12-31")))
    assert PERM_IV is not None
    PROH_IV = iv("lt", date.fromisoformat("2024-01-01"))

    # Build universal premises generically
    d = Term("d")
    perm_prem = forall_perm(d, PERM_IV, "odrl:read", "ex:resourceX")
    proh_prem = forall_proh(d, PROH_IV, "odrl:read", "ex:resourceX")

    # Build proof with the generic engine
    proof, result_uri = build_overlap_proof(perm_prem, proh_prem, action="odrl:read", target="ex:resourceX")

    # 1) Expected URI first
    print(result_uri)

    # 2) Pretty, numbered proof
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

