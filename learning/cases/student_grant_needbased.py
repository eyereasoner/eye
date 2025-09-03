#!/usr/bin/env python3
# =============================================================================
# Student Grants — Need-Based Grant (single-file EYE-learning case with 4 specs)
# =============================================================================
# Problem (short): Decide if a submitted student grant application should be
# Approved (and an award issued) or Rejected based on these rules:
# applicant must be 18–25 (inclusive lower bound, exclusive upper bound),
# enrolled FullTime, have household income below 20,000, and GPA ≥ 2.5.
#
# This file is completely self-contained and uses no third-party packages.
#
# It demonstrates a use case that follows “four types of specification artefacts”:
#
#   1) Vocabulary (RDFS/SKOS)
#   2) Application Profile (SHACL)
#   3) Interaction Pattern / Rules (N3; triple patterns only)
#   4) Implementation Guide (human-readable composition/integration notes)
#
# and implements the EYE-learning style:
#   - Answer      : final result
#   - Reason why  : trace of rule firings
#   - Check       : concise harness verifying behavior
#
# Run:
#   python student_grant_needbased.py
#
# Optional:
#   python student_grant_needbased.py --show-spec
#       → prints the 4 artefacts embedded below
# =============================================================================

from dataclasses import dataclass, field
from typing import List, Tuple, Dict, Any, Callable
import sys

Triple = Tuple[str, str, Any]  # (subject, predicate, object) — object may be str|int|float|bool

# -----------------------------------------------------------------------------
# 1) Vocabulary (RDFS/SKOS)
# -----------------------------------------------------------------------------
VOCAB_TTL = """@prefix ex: <https://example.org/studentgrant#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Student           a rdfs:Class ; rdfs:label "Student" ; rdfs:subClassOf foaf:Person .
ex:GrantApplication  a rdfs:Class ; rdfs:label "Grant Application" .
ex:GrantAward        a rdfs:Class ; rdfs:label "Grant Award" .

ex:NeedBasedGrant a skos:Concept ; skos:prefLabel "Need-based Grant" .

ex:hasApplicant    a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:range ex:Student ; rdfs:label "has applicant" .
ex:appliesFor      a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:range skos:Concept ; rdfs:label "applies for" .
ex:status          a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:label "status" .
ex:eligible        a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:label "eligible" .
ex:issuedAward     a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:range ex:GrantAward ; rdfs:label "issued award" .

ex:hasAge           a rdf:Property ; rdfs:domain ex:Student ; rdfs:label "age" .
ex:householdIncome  a rdf:Property ; rdfs:domain ex:Student ; rdfs:label "household income" .
ex:enrollmentStatus a rdf:Property ; rdfs:domain ex:Student ; rdfs:label "enrollment status" .
ex:gpa              a rdf:Property ; rdfs:domain ex:Student ; rdfs:label "GPA" .

ex:hasEvidence a rdf:Property ; rdfs:domain ex:GrantApplication ; rdfs:label "has evidence" .
ex:AgeEvidence a rdfs:Class ; rdfs:label "Age Evidence" .
ex:IncomeEvidence a rdfs:Class ; rdfs:label "Income Evidence" .
ex:EnrollmentEvidence a rdfs:Class ; rdfs:label "Enrollment Evidence" .
ex:GpaEvidence a rdfs:Class ; rdfs:label "GPA Evidence" .

# Minimal statuses as SKOS concepts
ex:Submitted a skos:Concept ; skos:prefLabel "Submitted" .
ex:Approved  a skos:Concept ; skos:prefLabel "Approved" .
ex:Rejected  a skos:Concept ; skos:prefLabel "Rejected" .
"""

# -----------------------------------------------------------------------------
# 2) Application Profile (SHACL)
# -----------------------------------------------------------------------------
PROFILE_SHACL_TTL = """@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix ex:  <https://example.org/studentgrant#> .
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
    sh:minCount 3 ;                         # require at least 3 evidences (e.g., age, income, enrollment)
  ] .

ex:StudentShape a sh:NodeShape ;
  sh:targetClass ex:Student ;
  sh:property [
    sh:path ex:hasAge ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:householdIncome ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:enrollmentStatus ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:gpa ;
    sh:minCount 1 ;
    sh:datatype xsd:decimal ;
  ] .
"""

# -----------------------------------------------------------------------------
# 3) Interaction Pattern / RULES_N3 (N3, triple patterns only)
# -----------------------------------------------------------------------------
RULES_N3 = """# Interaction pattern: Need-based Student Grant
# States: Submitted -> (Eligible?) -> Approved/Rejected
# Preconditions: Application conforms to GrantApplicationShape; Student conforms to StudentShape
# Abstract messages: submitApplication, validate, decide, notify

@prefix ex:   <https://example.org/studentgrant#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

# R1: Eligibility — age in [18, 26), enrollment=FullTime, income < 20000, GPA >= 2.5, appliesFor NeedBasedGrant
{
  ?app  ex:appliesFor      ex:NeedBasedGrant .
  ?app  ex:hasApplicant    ?s .
  ?s    ex:hasAge          ?age .
  ?s    ex:enrollmentStatus "FullTime" .
  ?s    ex:householdIncome ?income .
  ?s    ex:gpa             ?gpa .
  "18"^^xsd:integer  math:notGreaterThan ?age .
  ?age  math:lessThan       "26"^^xsd:integer .
  ?income math:lessThan     "20000"^^xsd:integer .
  "2.5"^^xsd:decimal math:notLessThan ?gpa .
}
=>
{
  ?app ex:eligible true .
} .

# R2: Approve — eligible true AND status Submitted → status Approved + award issuance
{
  ?app ex:eligible true .
  ?app ex:status   ex:Submitted .
}
=>
{
  ?app ex:status ex:Approved .
  _:award a ex:GrantAward .
  ?app ex:issuedAward _:award .
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
Help agencies award a *Need-based Student Grant* to full-time students aged 18–25 with low household income and sufficient GPA.

## Artefacts overview
- `VOCAB_TTL` — domain terms (RDFS/SKOS)
- `PROFILE_SHACL_TTL` — conformance shapes for inputs
- `RULES_N3` — reusable eligibility + decision flow in N3 (triple patterns only)
- the Python in this file — a **single, self-contained script** that produces:
  **Answer**, **Reason why**, and a **Check (harness)** per EYE-learning.

## Minimal HTTP binding (suggested)
1. `POST /grant-applications` → create a GrantApplication (status=Submitted).
2. `POST /grant-applications/{id}/validate` → run SHACL (shape IDs in this guide).
3. `POST /grant-applications/{id}/decide` → execute the rules; transition to Approved/Rejected.
4. `GET  /grant-applications/{id}` → fetch current status, issued award.

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
EX = "https://example.org/studentgrant#"

FACTS: List[Triple] = [
    # Student
    (f"{EX}Eve",         f"{EX}hasAge",           20),
    (f"{EX}Eve",         f"{EX}enrollmentStatus", "FullTime"),
    (f"{EX}Eve",         f"{EX}householdIncome",  15000),
    (f"{EX}Eve",         f"{EX}gpa",              3.1),
    # Application
    (f"{EX}APP-1001",    f"{EX}hasApplicant",     f"{EX}Eve"),
    (f"{EX}APP-1001",    f"{EX}appliesFor",       f"{EX}NeedBasedGrant"),
    (f"{EX}APP-1001",    f"{EX}status",           f"{EX}Submitted"),
    # Evidence markers (to satisfy shape)
    (f"{EX}APP-1001",    f"{EX}hasEvidence",      "AgeEvidence"),
    (f"{EX}APP-1001",    f"{EX}hasEvidence",      "IncomeEvidence"),
    (f"{EX}APP-1001",    f"{EX}hasEvidence",      "EnrollmentEvidence"),
    (f"{EX}APP-1001",    f"{EX}hasEvidence",      "GpaEvidence"),
]

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
      - 18 <= age < 26
      - enrollment FullTime
      - income < 20000
      - gpa >= 2.5
    """
    added = 0
    apps = [s for (s,_,_) in q(triples, None, f"{EX}appliesFor", f"{EX}NeedBasedGrant")]
    for app in apps:
        persons = [o for (_,_,o) in q(triples, app, f"{EX}hasApplicant", None)]
        for stu in persons:
            ageL   = [o for (_,_,o) in q(triples, stu, f"{EX}hasAge", None) if isinstance(o, int)]
            enroll = [o for (_,_,o) in q(triples, stu, f"{EX}enrollmentStatus", None)]
            incL   = [o for (_,_,o) in q(triples, stu, f"{EX}householdIncome", None) if isinstance(o, int)]
            gpaL   = [o for (_,_,o) in q(triples, stu, f"{EX}gpa", None) if isinstance(o, (int,float))]
            if ageL and enroll and incL and gpaL:
                age, status, income, gpa = ageL[0], enroll[0], incL[0], float(gpaL[0])
                ok = (age >= 18) and (age < 26) and (status == "FullTime") and (income < 20000) and (gpa >= 2.5)
                if ok:
                    if add_unique(triples, (app, f"{EX}eligible", True)):
                        fires.append(RuleFire("R1", {
                            "app": app, "student": stu,
                            "age": str(age), "status": status,
                            "income": str(income), "gpa": f"{gpa:.2f}"
                        }))
                        added += 1
                else:
                    if add_unique(triples, (app, f"{EX}eligible", False)):
                        fires.append(RuleFire("R1-not", {
                            "app": app, "student": stu,
                            "age": str(age), "status": status,
                            "income": str(income), "gpa": f"{gpa:.2f}"
                        }))
                        added += 1
    return added

award_counter = 0
def new_award_iri() -> str:
    """Generate a unique (mock) award IRI."""
    global award_counter
    award_counter += 1
    return f"{EX}AWARD-{award_counter:03d}"

def rule_R2_approve(triples: List[Triple], fires: List[RuleFire]) -> int:
    """If eligible true and status Submitted → add status Approved and issue an award (single-fire per application)."""
    added = 0
    for (app,_,_) in q(triples, None, f"{EX}eligible", True):
        already_approved = any(True for _ in q(triples, app, f"{EX}status", f"{EX}Approved"))
        has_award        = any(True for _ in q(triples, app, f"{EX}issuedAward", None))
        if (app, f"{EX}status", f"{EX}Submitted") in triples and not already_approved and not has_award:
            if add_unique(triples, (app, f"{EX}status", f"{EX}Approved")):
                fires.append(RuleFire("R2", {"app": app}))
                added += 1
            iri = new_award_iri()
            if add_unique(triples, (iri, "a", f"{EX}GrantAward")):
                added += 1
            if add_unique(triples, (app, f"{EX}issuedAward", iri)):
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
        errors.append("Need at least 3 evidences (age, income, enrollment/GPA)")
    return errors

def validate_student_shape(triples: List[Triple], student: str) -> List[str]:
    errors = []
    if not list(q(triples, student, f"{EX}hasAge", None)):
        errors.append("Missing ex:hasAge")
    if not list(q(triples, student, f"{EX}householdIncome", None)):
        errors.append("Missing ex:householdIncome")
    if not list(q(triples, student, f"{EX}enrollmentStatus", None)):
        errors.append("Missing ex:enrollmentStatus")
    if not list(q(triples, student, f"{EX}gpa", None)):
        errors.append("Missing ex:gpa")
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
        for (_,_,student) in q(triples, app, f"{EX}hasApplicant", None):
            errs += validate_student_shape(triples, student)
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
    app    = f"{EX}APP-1001"
    status = get_final_status(triples, app)
    award  = [o for (_,_,o) in q(triples, app, f"{EX}issuedAward", None)]
    answer = f"Application APP-1001 status: {status}"
    if award:
        answer += f"; issued award: {award[0].split('#')[-1]}"
    # Reason — brief rule trace
    lines = []
    for f in res.fires:
        if f.rule_id == "R1":
            lines.append("R1: Eligible (age∈[18,26), FullTime, income<20000, GPA≥2.5).")
        elif f.rule_id == "R1-not":
            lines.append("R1-not: Not eligible.")
        elif f.rule_id == "R2":
            lines.append("R2: Approved + award issued.")
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
    ans1, _, _ = compute_answer_reason(t1)
    s1 = get_final_status(t1, f"{EX}APP-1001")
    assert s1 == "Approved", "Expected Approved for base case"
    print("[1] Happy path → Approved ✓")

    # 2) Over-age (age=27) -> Rejected
    t2 = clone(FACTS)
    t2 = [tr for tr in t2 if not (tr[0] == f"{EX}Eve" and tr[1] == f"{EX}hasAge")]
    t2.append((f"{EX}Eve", f"{EX}hasAge", 27))
    _, _, _ = compute_answer_reason(t2)
    s2 = get_final_status(t2, f"{EX}APP-1001")
    assert s2 == "Rejected", "Over-age should lead to Rejected"
    print("[2] Over-age (27) → Rejected ✓")

    # 3) Part-time enrollment -> Rejected
    t3 = clone(FACTS)
    t3 = [tr for tr in t3 if not (tr[0] == f"{EX}Eve" and tr[1] == f"{EX}enrollmentStatus")]
    t3.append((f"{EX}Eve", f"{EX}enrollmentStatus", "PartTime"))
    _, _, _ = compute_answer_reason(t3)
    s3 = get_final_status(t3, f"{EX}APP-1001")
    assert s3 == "Rejected", "Part-time should lead to Rejected"
    print("[3] Part-time → Rejected ✓")

    # 4) High income (30000) -> Rejected
    t4 = clone(FACTS)
    t4 = [tr for tr in t4 if not (tr[0] == f"{EX}Eve" and tr[1] == f"{EX}householdIncome")]
    t4.append((f"{EX}Eve", f"{EX}householdIncome", 30000))
    _, _, _ = compute_answer_reason(t4)
    s4 = get_final_status(t4, f"{EX}APP-1001")
    assert s4 == "Rejected", "High income should lead to Rejected"
    print("[4] High income (30000) → Rejected ✓")

    # 5) Low GPA (2.2) -> Rejected
    t5 = clone(FACTS)
    t5 = [tr for tr in t5 if not (tr[0] == f"{EX}Eve" and tr[1] == f"{EX}gpa")]
    t5.append((f"{EX}Eve", f"{EX}gpa", 2.2))
    _, _, _ = compute_answer_reason(t5)
    s5 = get_final_status(t5, f"{EX}APP-1001")
    assert s5 == "Rejected", "Low GPA should lead to Rejected"
    print("[5] Low GPA (2.2) → Rejected ✓")

    # 6) Shape invalid (remove evidences) -> validation error
    t6 = clone(FACTS)
    t6 = [tr for tr in t6 if tr[1] != f"{EX}hasEvidence"]
    try:
        compute_answer_reason(t6)
        raise AssertionError("Expected validation failure for missing evidence")
    except ValueError:
        print("[6] Missing evidence → Validation error ✓")

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

    # EYE-learning outputs
    answer, reason, _ = compute_answer_reason(FACTS.copy())
    print("Answer:")
    print("  " + answer)
    print("Reason why:")
    print("  " + reason)
    print("Check (harness):")
    run_checks()

if __name__ == "__main__":
    main(sys.argv[1:])

