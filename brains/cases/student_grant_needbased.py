#!/usr/bin/env python3
# =============================================================================
# Education — Need-based Student Grant (single-file P3 case with 4 specs)
# =============================================================================
# Problem (short): Decide if a submitted need-based student grant application
# should be Approved (and a payment issued) or Rejected based on:
# full-time study (ECTS ≥ 30), per-capita household income ≤ 18,000, and tuition > 0.
#
# This file is completely self-contained and uses no third-party packages.
#
# It demonstrates a use case that follows “four types of specification artefacts” [1]:
#
#   1) Vocabulary (RDFS/SKOS)
#   2) Application Profile (SHACL)
#   3) Interaction Pattern / Rules (N3; triple patterns only)
#   4) Implementation Guide (human-readable composition/integration notes)
#
# and implements the P3 style:
#   - Answer      : final result
#   - Reason why  : brief trace of rule firings
#   - Check       : concise harness verifying behavior
#
# Run:
#   python student_grant_needbased.py
#
# Optional:
#   python student_grant_needbased.py --show-spec   # print the 4 artefacts
#   python student_grant_needbased.py --show-data   # print FACTS as Turtle
#
# References:
#   [1] https://pietercolpaert.be/interoperability/2025/09/03/four-types-specification-artefacts
# =============================================================================

from dataclasses import dataclass, field
from typing import List, Tuple, Dict, Any, Callable
import sys

Triple = Tuple[str, str, Any]  # (subject, predicate, object) — object may be str|int|float|bool

# -----------------------------------------------------------------------------
# 1) Vocabulary (RDFS/SKOS)
# -----------------------------------------------------------------------------
VOCAB_TTL = """@prefix ex: <https://example.org/edu#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Student                 a rdfs:Class ; rdfs:label "Student" ; rdfs:subClassOf foaf:Person .
ex:GrantApplication        a rdfs:Class ; rdfs:label "Need-based Grant Application" .
ex:GrantPayment            a rdfs:Class ; rdfs:label "Grant Payment" .

ex:NeedBasedGrant a skos:Concept ; skos:prefLabel "Need-based Student Grant" .

ex:hasApplicant     a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:range ex:Student ; rdfs:label "has applicant" .
ex:appliesFor       a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:range skos:Concept ; rdfs:label "applies for" .
ex:status           a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:label "status" .
ex:eligible         a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:label "eligible" .
ex:issuedPayment    a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:range ex:GrantPayment ; rdfs:label "issued payment" .

ex:householdIncome  a rdf:Property ; rdfs:domain ex:Student ; rdfs:label "household income (annual)" .
ex:householdSize    a rdf:Property ; rdfs:domain ex:Student ; rdfs:label "household size" .
ex:enrolledECTS     a rdf:Property ; rdfs:domain ex:Student ; rdfs:label "enrolled ECTS credits" .
ex:tuitionAmount    a rdf:Property ; rdfs:domain ex:Student ; rdfs:label "tuition amount" .

ex:hasEvidence a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:label "has evidence" .
ex:IncomeEvidence a rdfs:Class ; rdfs:label "Income Evidence" .
ex:EnrollmentEvidence a rdfs:Class ; rdfs:label "Enrollment Evidence" .
ex:HouseholdEvidence a rdfs:Class ; rdfs:label "Household Evidence" .

# Minimal statuses as SKOS concepts
ex:Submitted a skos:Concept ; skos:prefLabel "Submitted" .
ex:Approved  a skos:Concept ; skos:prefLabel "Approved" .
ex:Rejected  a skos:Concept ; skos:prefLabel "Rejected" .
"""

# -----------------------------------------------------------------------------
# 2) Application Profile (SHACL)
# -----------------------------------------------------------------------------
PROFILE_SHACL_TTL = """@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix ex:  <https://example.org/edu#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:GrantApplicationShape a sh:NodeShape ;
  sh:targetClass ex:GrantApplication ;
  sh:property [
    sh:path ex:hasApplicant ;
    sh:minCount 1 ;
    sh:nodeKind sh:IRI ;
  ] ;
  sh:property [
    sh:path ex:appliesFor ;
    sh:hasValue ex:NeedBasedGrant ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:status ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:hasEvidence ;
    sh:minCount 3 ;                         # require at least 3 evidences (income, household, enrollment)
  ] .

ex:StudentShape a sh:NodeShape ;
  sh:targetClass ex:Student ;
  sh:property [
    sh:path ex:householdIncome ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:householdSize ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:enrolledECTS ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:tuitionAmount ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] .
"""

# -----------------------------------------------------------------------------
# 3) Interaction Pattern / RULES_N3 (N3, triple patterns only)
# -----------------------------------------------------------------------------
RULES_N3 = """# Interaction pattern: Need-based Student Grant
# States: Submitted -> (Eligible?) -> Approved/Rejected
# Preconditions: Application conforms to GrantApplicationShape; Student conforms to StudentShape
# Abstract messages: submitApplication, validate, decide, notify

@prefix ex:   <https://example.org/edu#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

# R1: Eligibility — ECTS ≥ 30, perCapitaIncome ≤ 18000, tuition > 0
# perCapitaIncome = householdIncome / householdSize
{
  ?app  ex:appliesFor     ex:NeedBasedGrant .
  ?app  ex:hasApplicant   ?s .
  ?s    ex:enrolledECTS   ?ects .
  ?s    ex:householdIncome ?inc .
  ?s    ex:householdSize   ?hh .
  ?s    ex:tuitionAmount   ?fee .
  "30"^^xsd:integer     math:notGreaterThan ?ects .
  (?inc ?hh)            math:quotient       ?pc .
  ?pc                   math:notGreaterThan "18000"^^xsd:integer .
  ?fee                  math:greaterThan    "0"^^xsd:integer .
}
=>
{
  ?app ex:eligible true .
} .

# R2: Approve — eligible true AND status Submitted → status Approved + payment issuance
{
  ?app ex:eligible true .
  ?app ex:status   ex:Submitted .
}
=>
{
  ?app ex:status ex:Approved .
  _:pay a ex:GrantPayment .
  ?app ex:issuedPayment _:pay .
} .

# R3: Reject — eligible false AND status Submitted → status Rejected
{
  ?app ex:eligible false .
  ?app ex:status   ex:Submitted .
}
=>
{
  ?app ex:status ex:Rejected .
} .
"""

# -----------------------------------------------------------------------------
# 4) Implementation Guide (human-readable)
# -----------------------------------------------------------------------------
IMPLEMENTATION_GUIDE_MD = """# Need-based Student Grant — Implementation Guide

This guide composes the four artefacts — **Vocabulary**, **Application Profile**, **Interaction Pattern/Rules**, and this **Implementation Guide** — into a ready-to-implement package.

## Purpose
Support equitable access to higher education by awarding grants to full-time students with modest per-capita household income.

## Artefacts overview
- `VOCAB_TTL` — domain terms (RDFS/SKOS)
- `PROFILE_SHACL_TTL` — conformance shapes for inputs
- `RULES_N3` — reusable eligibility + decision flow in N3 (triple patterns only)
- the Python in this file — a **single, self-contained script** that produces:
  **Answer**, **Reason why**, and a **Check (harness)** per P3.

## Minimal HTTP binding (suggested)
1. `POST /grant-applications` → create a GrantApplication (status=Submitted).
2. `POST /grant-applications/{id}/validate` → run SHACL (shape IDs in this guide).
3. `POST /grant-applications/{id}/decide` → execute the rules; transition to Approved/Rejected.
4. `GET  /grant-applications/{id}` → fetch current status, issued payment.

## Conformance
- Validate incoming `GrantApplication` and `Student` resources against the shapes.
- A server is conformant if, given data that meets the shapes, it transitions
  state identically to the rules in `RULES_N3`.

## Local testing
Run this file with Python. It prints the **Answer**, **Reason why**, and then runs its **Check** suite.
"""

# -----------------------------------------------------------------------------
# Namespaces and embedded sample data
# -----------------------------------------------------------------------------
EX = "https://example.org/edu#"

FACTS: List[Triple] = [
    # Student (Alice): full-time, per-capita income = 36,000 / 3 = 12,000, tuition 1,200
    (f"{EX}Alice",          f"{EX}enrolledECTS",    60),
    (f"{EX}Alice",          f"{EX}householdIncome", 36000),
    (f"{EX}Alice",          f"{EX}householdSize",   3),
    (f"{EX}Alice",          f"{EX}tuitionAmount",   1200),
    # Application
    (f"{EX}APP-GR-01",      f"{EX}hasApplicant",    f"{EX}Alice"),
    (f"{EX}APP-GR-01",      f"{EX}appliesFor",      f"{EX}NeedBasedGrant"),
    (f"{EX}APP-GR-01",      f"{EX}status",          f"{EX}Submitted"),
    # Evidence markers (to satisfy shape)
    (f"{EX}APP-GR-01",      f"{EX}hasEvidence",     "IncomeEvidence"),
    (f"{EX}APP-GR-01",      f"{EX}hasEvidence",     "EnrollmentEvidence"),
    (f"{EX}APP-GR-01",      f"{EX}hasEvidence",     "HouseholdEvidence"),
]

# -----------------------------------------------------------------------------
# Utility: print FACTS as Turtle
# -----------------------------------------------------------------------------
def to_turtle(triples, prefixes=None):
    """
    Convert a List[Triple] -> Turtle string.
    Triple = (subject:str, predicate:str, object: str|int|float|bool)
    - Subjects/predicates that look like IRIs are shortened to CURIEs using `prefixes`.
    - Predicate 'a' is printed as rdf:type shorthand.
    - Literals are auto-typed by Turtle (int/float/bool); strings are quoted+escaped.
    """
    if prefixes is None:
        prefixes = {}

    lines = []
    for pfx, ns in prefixes.items():
        lines.append(f"@prefix {pfx}: <{ns}> .")
    if prefixes:
        lines.append("")

    def is_iri(v):
        return isinstance(v, str) and (v.startswith("http://") or v.startswith("https://"))

    def qname(iri):
        for pfx, ns in prefixes.items():
            if iri.startswith(ns):
                local = iri[len(ns):]
                if local and all(c.isalnum() or c in "_-." for c in local):
                    return f"{pfx}:{local}"
        return f"<{iri}>"

    def esc(s: str) -> str:
        return s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")

    def term(v):
        if v is True:  return "true"
        if v is False: return "false"
        if isinstance(v, int):   return str(v)
        if isinstance(v, float):
            s = repr(v)
            if "." not in s and "e" not in s and "E" not in s:
                s += ".0"
            return s
        if is_iri(v):            return qname(v)
        return f"\"{esc(str(v))}\""

    # Group by subject and pretty-print
    from collections import defaultdict
    spo = defaultdict(list)
    for s, p, o in triples:
        spo[s].append((p, o))

    for s in sorted(spo.keys()):
        subj = qname(s) if is_iri(s) else f"\"{esc(str(s))}\""
        lines.append(f"{subj} ")
        po_lines = []
        for p, o in sorted(spo[s], key=lambda x: (x[0], str(x[1]))):
            pred = "a" if p == "a" else (qname(p) if is_iri(p) else f"\"{esc(str(p))}\"")
            po_lines.append(f"  {pred} {term(o)}")
        lines.append(" ;\n".join(po_lines) + " .")
        lines.append("")

    return "\n".join(lines)

# -----------------------------------------------------------------------------
# Tiny forward-chaining engine and helpers
# -----------------------------------------------------------------------------
def q(triples: List[Triple], s=None, p=None, o=None):
    """Generator for triples matching a (s, p, o) pattern; None is a wildcard."""
    for (s2, p2, o2) in triples:
        if (s is None or s == s2) and (p is None or p == p2) and (o is None or o == o2):
            yield (s2, p2, o2)

def add_unique(triples: List[Triple], triple: Triple) -> bool:
    """Append a triple if not already present. Return True if added."""
    if triple not in triples:
        triples.append(triple)
        return True
    return False

@dataclass
class RuleFire:
    rule_id: str
    bindings: Dict[str, str]

@dataclass
class ReasoningResult:
    added: int
    fires: List[RuleFire] = field(default_factory=list)

# -----------------------------------------------------------------------------
# Rules (Python mirrors of RULES_N3 for this tiny engine)
# -----------------------------------------------------------------------------
def rule_R1_eligibility(triples: List[Triple], fires: List[RuleFire]) -> int:
    """
    Eligibility if:
      - appliesFor NeedBasedGrant
      - ECTS >= 30
      - per-capita income = income / household size <= 18000
      - tuitionAmount > 0
    """
    added = 0
    apps = [s for (s,_,_) in q(triples, None, f"{EX}appliesFor", f"{EX}NeedBasedGrant")]
    for app in apps:
        students = [o for (_,_,o) in q(triples, app, f"{EX}hasApplicant", None)]
        for stu in students:
            ectsL = [o for (_,_,o) in q(triples, stu, f"{EX}enrolledECTS", None) if isinstance(o, int)]
            incL  = [o for (_,_,o) in q(triples, stu, f"{EX}householdIncome", None) if isinstance(o, int)]
            hhL   = [o for (_,_,o) in q(triples, stu, f"{EX}householdSize", None) if isinstance(o, int)]
            feeL  = [o for (_,_,o) in q(triples, stu, f"{EX}tuitionAmount", None) if isinstance(o, int)]
            if ectsL and incL and hhL and feeL:
                ects, inc, hh, fee = ectsL[0], incL[0], hhL[0], feeL[0]
                if hh <= 0:
                    # Defensive: impossible per the intent; treat as not eligible.
                    pc = float("inf")
                else:
                    # Integer quotient to mirror math:quotient in a simple way
                    pc = inc // hh
                ok = (ects >= 30) and (pc <= 18000) and (fee > 0)
                if ok:
                    if add_unique(triples, (app, f"{EX}eligible", True)):
                        fires.append(RuleFire("R1", {
                            "app": app, "student": stu,
                            "ects": str(ects), "income": str(inc),
                            "hh": str(hh), "pc": str(pc), "fee": str(fee)
                        }))
                        added += 1
                else:
                    if add_unique(triples, (app, f"{EX}eligible", False)):
                        fires.append(RuleFire("R1-not", {
                            "app": app, "student": stu,
                            "ects": str(ects), "income": str(inc),
                            "hh": str(hh), "pc": str(pc), "fee": str(fee)
                        }))
                        added += 1
    return added

payment_counter = 0
def new_payment_iri() -> str:
    """Generate a unique (mock) payment IRI."""
    global payment_counter
    payment_counter += 1
    return f"{EX}PAY-{payment_counter:03d}"

def rule_R2_approve(triples: List[Triple], fires: List[RuleFire]) -> int:
    """If eligible true and status Submitted → add status Approved and issue a payment (single-fire per application)."""
    added = 0
    for (app,_,_) in q(triples, None, f"{EX}eligible", True):
        already_approved = any(True for _ in q(triples, app, f"{EX}status", f"{EX}Approved"))
        has_payment      = any(True for _ in q(triples, app, f"{EX}issuedPayment", None))
        if (app, f"{EX}status", f"{EX}Submitted") in triples and not already_approved and not has_payment:
            if add_unique(triples, (app, f"{EX}status", f"{EX}Approved")):
                fires.append(RuleFire("R2", {"app": app}))
                added += 1
            iri = new_payment_iri()
            if add_unique(triples, (iri, "a", f"{EX}GrantPayment")):
                added += 1
            if add_unique(triples, (app, f"{EX}issuedPayment", iri)):
                added += 1
    return added

def rule_R3_reject(triples: List[Triple], fires: List[RuleFire]) -> int:
    """If eligible false and status Submitted → add status Rejected."""
    added = 0
    for (app,_,_) in q(triples, None, f"{EX}eligible", False):
        if (app, f"{EX}status", f"{EX}Submitted") in triples:
            if add_unique(triples, (app, f"{EX}status", f"{EX}Rejected")):
                fires.append(RuleFire("R3", {"app": app}))
                added += 1
    return added

RULES: List[Callable[[List[Triple], List[RuleFire]], int]] = [
    rule_R1_eligibility,
    rule_R2_approve,
    rule_R3_reject,
]

# -----------------------------------------------------------------------------
# SHACL-like checks (toy)
# -----------------------------------------------------------------------------
def validate_application_shape(triples: List[Triple], app: str) -> List[str]:
    errors = []
    if not list(q(triples, app, f"{EX}hasApplicant", None)):
        errors.append("Missing ex:hasApplicant")
    if (app, f"{EX}appliesFor", f"{EX}NeedBasedGrant") not in triples:
        errors.append("Application not for ex:NeedBasedGrant")
    if not list(q(triples, app, f"{EX}status", None)):
        errors.append("Missing ex:status")
    evidences = list(q(triples, app, f"{EX}hasEvidence", None))
    if len(evidences) < 3:
        errors.append("Need at least 3 evidences (income, household, enrollment)")
    return errors

def validate_student_shape(triples: List[Triple], stu: str) -> List[str]:
    errors = []
    if not list(q(triples, stu, f"{EX}householdIncome", None)):
        errors.append("Missing ex:householdIncome")
    if not list(q(triples, stu, f"{EX}householdSize", None)):
        errors.append("Missing ex:householdSize")
    if not list(q(triples, stu, f"{EX}enrolledECTS", None)):
        errors.append("Missing ex:enrolledECTS")
    if not list(q(triples, stu, f"{EX}tuitionAmount", None)):
        errors.append("Missing ex:tuitionAmount")
    return errors

# -----------------------------------------------------------------------------
# Reasoning pipeline
# -----------------------------------------------------------------------------
def decide(triples: List[Triple]) -> ReasoningResult:
    fires: List[RuleFire] = []
    # validation
    apps = {s for (s,p,o) in triples if p == f"{EX}appliesFor" and o == f"{EX}NeedBasedGrant"}
    for app in apps:
        errs = validate_application_shape(triples, app)
        for (_,_,stu) in q(triples, app, f"{EX}hasApplicant", None):
            errs += validate_student_shape(triples, stu)
        if errs:
            raise ValueError(f"SHACL-like validation failed for {app}: " + "; ".join(errs))
    # forward chaining to fixpoint
    added_total = 0
    while True:
        added = 0
        for rule in RULES:
            added += rule(triples, fires)
        added_total += added
        if added == 0:
            break
    return ReasoningResult(added=added_total, fires=fires)

# -----------------------------------------------------------------------------
# Rendering & helpers
# -----------------------------------------------------------------------------
def get_final_status(triples: List[Triple], app: str) -> str:
    """Return the last appended ex:status for the application, or 'UNKNOWN'."""
    statuses = [o for (s,p,o) in triples if s == app and p == f"{EX}status"]
    if not statuses:
        return "UNKNOWN"
    return statuses[-1].split("#")[-1] if isinstance(statuses[-1], str) else str(statuses[-1])

def compute_answer_reason(triples: List[Triple]):
    res = decide(triples)
    app    = f"{EX}APP-GR-01"
    status = get_final_status(triples, app)
    payment = [o for (_,_,o) in q(triples, app, f"{EX}issuedPayment", None)]
    answer = f"Application APP-GR-01 status: {status}"
    if payment:
        answer += f"; issued payment: {payment[0].split('#')[-1]}"
    # Reason — brief rule trace
    lines = []
    for f in res.fires:
        if f.rule_id == "R1":
            lines.append("R1: Eligible (ECTS≥30, perCapita≤18000, tuition>0).")
        elif f.rule_id == "R1-not":
            lines.append("R1-not: Not eligible.")
        elif f.rule_id == "R2":
            lines.append("R2: Approved + payment issued.")
        elif f.rule_id == "R3":
            lines.append("R3: Rejected.")
    reason = " ".join(lines) if lines else "No rules fired."
    return answer, reason, res

# -----------------------------------------------------------------------------
# Concise Check (harness)
# -----------------------------------------------------------------------------
def run_checks():
    def clone(trs): return [(s, p, o) for (s, p, o) in trs]

    print("Running checks...")
    # 1) Happy path -> Approved
    t1 = clone(FACTS)
    _, _, _ = compute_answer_reason(t1)
    s1 = get_final_status(t1, f"{EX}APP-GR-01")
    assert s1 == "Approved", "Expected Approved for base case"
    print("[1] Happy path → Approved ✓")

    # 2) Per-capita too high (90000/3=30000) -> Rejected
    t2 = clone(FACTS)
    t2 = [tr for tr in t2 if not (tr[0] == f"{EX}Alice" and tr[1] == f"{EX}householdIncome")]
    t2.append((f"{EX}Alice", f"{EX}householdIncome", 90000))
    _, _, _ = compute_answer_reason(t2)
    s2 = get_final_status(t2, f"{EX}APP-GR-01")
    assert s2 == "Rejected", "High per-capita income should lead to Rejected"
    print("[2] Per-capita income 30000 → Rejected ✓")

    # 3) Too few ECTS (24) -> Rejected
    t3 = clone(FACTS)
    t3 = [tr for tr in t3 if not (tr[0] == f"{EX}Alice" and tr[1] == f"{EX}enrolledECTS")]
    t3.append((f"{EX}Alice", f"{EX}enrolledECTS", 24))
    _, _, _ = compute_answer_reason(t3)
    s3 = get_final_status(t3, f"{EX}APP-GR-01")
    assert s3 == "Rejected", "Too few ECTS should lead to Rejected"
    print("[3] ECTS = 24 → Rejected ✓")

    # 4) Boundary per-capita exactly 18000 (54000/3) -> Approved
    t4 = clone(FACTS)
    t4 = [tr for tr in t4 if not (tr[0] == f"{EX}Alice" and tr[1] == f"{EX}householdIncome")]
    t4.append((f"{EX}Alice", f"{EX}householdIncome", 54000))
    _, _, _ = compute_answer_reason(t4)
    s4 = get_final_status(t4, f"{EX}APP-GR-01")
    assert s4 == "Approved", "Per-capita 18000 should be Approved (≤ 18000)"
    print("[4] Per-capita = 18000 → Approved ✓")

    # 5) Shape invalid (remove evidences) -> validation error
    t5 = clone(FACTS)
    t5 = [tr for tr in t5 if tr[1] != f"{EX}hasEvidence"]
    try:
        compute_answer_reason(t5)
        raise AssertionError("Expected validation failure for missing evidence")
    except ValueError:
        print("[5] Missing evidence → Validation error ✓")

    print("All checks passed.")

# -----------------------------------------------------------------------------
# Entrypoint
# -----------------------------------------------------------------------------
def main(argv: List[str]):
    if "--show-spec" in argv:
        print("=== 1) VOCAB_TTL ===");             print(VOCAB_TTL)
        print("=== 2) PROFILE_SHACL_TTL ===");     print(PROFILE_SHACL_TTL)
        print("=== 3) RULES_N3 ===");              print(RULES_N3)
        print("=== 4) IMPLEMENTATION_GUIDE_MD ==="); print(IMPLEMENTATION_GUIDE_MD)
        return

    if "--show-data" in argv:
        turtle = to_turtle(
            FACTS,
            prefixes={
                "ex": EX,
                "xsd": "http://www.w3.org/2001/XMLSchema#",
                "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "skos": "http://www.w3.org/2004/02/skos/core#",
            }
        )
        print("=== DATA (Turtle from FACTS) ===")
        print(turtle)
        return

    # P3 outputs
    answer, reason, _ = compute_answer_reason(FACTS.copy())
    print("Answer:")
    print("  " + answer)
    print("Reason why:")
    print("  " + reason)
    print("Check (harness):")
    run_checks()

if __name__ == "__main__":
    main(sys.argv[1:])

