#!/usr/bin/env python3
# =============================================================================
# BikePass Youth Discount — Single-file EYE-learning case with 4 spec artefacts
# =============================================================================
# Problem (short): Decide if a submitted BikePass Youth Discount application
# should be Approved (and a permit issued) or Rejected based on the rules:
# applicant must be under 26 and live in Ghent.
#
# This file is completely self-contained and uses no third-party packages.
#
# It demonstrates a use case that follows “four types of specification artefacts”:
#
#   1) Vocabulary (RDFS/SKOS)
#   2) Application Profile (SHACL)
#   3) Interaction Pattern / Rules (N3; triple patterns)
#   4) Implementation Guide (human-readable composition/integration notes)
#
# and implements the EYE-learning style:
#   - Answer      : final result
#   - Reason why  : trace of rule firings
#   - Check       : concise harness verifying behavior
#
# Run:
#   python bikepass_youth_discount.py
#
# Optional:
#   python bikepass_youth_discount.py --show-spec
#       → prints the 4 artefacts embedded below
#
# Notes:
# - The Python code below is a tiny forward-chaining demo engine + validator.
# - The N3 rules are provided as documentation/parallel expression of the logic
#   using only triple patterns (no FILTER). Swap this tiny engine for EYE if you
#   want formal proofs and full N3 support.
# =============================================================================

from dataclasses import dataclass, field
from typing import List, Tuple, Dict, Any, Callable
import sys

Triple = Tuple[str, str, Any]  # (subject, predicate, object) — object may be str|int|bool

# -----------------------------------------------------------------------------
# 1) Vocabulary (RDFS/SKOS)
# -----------------------------------------------------------------------------
VOCAB_TTL = """@prefix ex: <https://example.org/bikepass#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Person a rdfs:Class ;
  rdfs:label "Person" ;
  rdfs:subClassOf foaf:Person .

ex:Application a rdfs:Class ; rdfs:label "Application" .
ex:Permit      a rdfs:Class ; rdfs:label "Permit" .

ex:YouthDiscount a skos:Concept ; skos:prefLabel "Youth Discount" .

ex:hasApplicant   a rdf:Property ; rdfs:domain ex:Application ; rdfs:range ex:Person ; rdfs:label "has applicant" .
ex:appliesFor     a rdf:Property ; rdfs:domain ex:Application ; rdfs:range skos:Concept ; rdfs:label "applies for" .
ex:status         a rdf:Property ; rdfs:domain ex:Application ; rdfs:label "status" .
ex:eligible       a rdf:Property ; rdfs:domain ex:Application ; rdfs:label "eligible" .
ex:issuedPermit   a rdf:Property ; rdfs:domain ex:Application ; rdfs:range ex:Permit ; rdfs:label "issued permit" .

ex:hasAge         a rdf:Property ; rdfs:domain ex:Person ; rdfs:label "age" .
ex:residenceCity  a rdf:Property ; rdfs:domain ex:Person ; rdfs:label "residence city" .

ex:requiresEvidence a rdf:Property ; rdfs:domain skos:Concept ; rdfs:label "requires evidence type" .
ex:hasEvidence      a rdf:Property ; rdfs:domain ex:Application ; rdfs:label "has evidence" .
ex:AgeEvidence       a rdfs:Class ; rdfs:label "Age Evidence" .
ex:ResidenceEvidence a rdfs:Class ; rdfs:label "Residence Evidence" .
ex:evidenceOf a rdf:Property ; rdfs:label "evidence of" .

# Minimal status code list (could be SKOS in a real system)
ex:Submitted a skos:Concept ; skos:prefLabel "Submitted" .
ex:Approved  a skos:Concept ; skos:prefLabel "Approved" .
ex:Rejected  a skos:Concept ; skos:prefLabel "Rejected" .
"""

# -----------------------------------------------------------------------------
# 2) Application Profile (SHACL)
# -----------------------------------------------------------------------------
PROFILE_SHACL_TTL = """@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix ex:  <https://example.org/bikepass#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:ApplicationShape a sh:NodeShape ;
  sh:targetClass ex:Application ;
  sh:property [
    sh:path ex:hasApplicant ;
    sh:minCount 1 ;
    sh:nodeKind sh:IRI ;
  ] ;
  sh:property [
    sh:path ex:appliesFor ;
    sh:hasValue ex:YouthDiscount ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:status ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:hasEvidence ;
    sh:minCount 2 ;
  ] .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:hasAge ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:residenceCity ;
    sh:minCount 1 ;
  ] .
"""

# -----------------------------------------------------------------------------
# 3) Interaction Pattern / RULES_N3 (N3, triple patterns only)
# -----------------------------------------------------------------------------
RULES_N3 = """# Interaction pattern: BikePass Youth Discount application
# States: Submitted -> (Eligible?) -> Approved/Rejected
# Preconditions: Application conforms to ApplicationShape; Person conforms to PersonShape
# Abstract messages: submitApplication, validate, decide, notify

@prefix ex:   <https://example.org/bikepass#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

# R1: Eligibility — age < 26 AND city == "Ghent" AND appliesFor YouthDiscount
{
  ?app  ex:appliesFor   ex:YouthDiscount .
  ?app  ex:hasApplicant ?p .
  ?p    ex:hasAge       ?age .
  ?p    ex:residenceCity "Ghent" .
  ?age  math:lessThan   "26"^^xsd:integer .
}
=>
{
  ?app ex:eligible true .
} .

# R2: Approve — eligible true AND status Submitted → status Approved + permit issuance
{
  ?app ex:eligible true .
  ?app ex:status   ex:Submitted .
}
=>
{
  ?app ex:status ex:Approved .
  _:permit a ex:Permit .
  ?app ex:issuedPermit _:permit .
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
IMPLEMENTATION_GUIDE_MD = """# BikePass Youth Discount — Implementation Guide

This guide composes the four artefacts — **Vocabulary**, **Application Profile**, **Interaction Pattern/Rules**, and this **Implementation Guide** — into a ready-to-implement package.

## Purpose
Help cities issue a *BikePass Youth Discount* permit to residents under 26 living in Ghent.

## Artefacts overview
- `VOCAB_TTL` — domain terms (RDFS/SKOS)
- `PROFILE_SHACL_TTL` — conformance shapes for inputs
- `RULES_N3` — reusable eligibility + decision flow in N3 (triple patterns only)
- the Python in this file — a **single, self-contained script** that produces:
  **Answer**, **Reason why**, and a **Check (harness)** per EYE-learning.

## Minimal HTTP binding (suggested)
1. `POST /applications` → create an Application (status=Submitted).
2. `POST /applications/{id}/validate` → run SHACL (shape IDs in this guide).
3. `POST /applications/{id}/decide` → execute the interaction rules; transition to Approved/Rejected.
4. `GET  /applications/{id}` → fetch current status, issued permit.

## Reuse and alignment
- `ex:Person` aligns to `foaf:Person`.
- Statuses are SKOS concepts that can be extended to a full code list.

## Conformance
- Validate incoming `Application` and `Person` resources against the shapes.
- A server is conformant if, given data that meets the shapes, it transitions
  state identically to the rules in `RULES_N3`.

## Local testing
Run this file with Python. It prints the **Answer**, **Reason why**, and then runs its **Check** suite.
"""

# -----------------------------------------------------------------------------
# Namespaces and embedded sample data
# -----------------------------------------------------------------------------
EX = "https://example.org/bikepass#"

FACTS: List[Triple] = [
    (f"{EX}Alice",     f"{EX}hasAge",         22),
    (f"{EX}Alice",     f"{EX}residenceCity",  "Ghent"),
    (f"{EX}APP-001",   f"{EX}hasApplicant",   f"{EX}Alice"),
    (f"{EX}APP-001",   f"{EX}appliesFor",     f"{EX}YouthDiscount"),
    (f"{EX}APP-001",   f"{EX}status",         f"{EX}Submitted"),
    # Abstract evidence markers (to satisfy the shape)
    (f"{EX}APP-001",   f"{EX}hasEvidence",    "AgeEvidence"),
    (f"{EX}APP-001",   f"{EX}hasEvidence",    "ResidenceEvidence"),
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
    """If appliesFor YouthDiscount and applicant's age < 26 and city == Ghent → eligible true; else eligible false."""
    added = 0
    apps = [s for (s,_,_) in q(triples, None, f"{EX}appliesFor", f"{EX}YouthDiscount")]
    for app in apps:
        persons = [o for (_,_,o) in q(triples, app, f"{EX}hasApplicant", None)]
        for p in persons:
            ages   = [o for (_,_,o) in q(triples, p, f"{EX}hasAge", None) if isinstance(o, int)]
            cities = [o for (_,_,o) in q(triples, p, f"{EX}residenceCity", None)]
            if ages and cities:
                age, city = ages[0], cities[0]
                if age < 26 and city == "Ghent":
                    if add_unique(triples, (app, f"{EX}eligible", True)):
                        fires.append(RuleFire("R1", {"app": app, "person": p, "age": str(age), "city": city}))
                        added += 1
                else:
                    if add_unique(triples, (app, f"{EX}eligible", False)):
                        fires.append(RuleFire("R1-not", {"app": app, "person": p, "age": str(age), "city": city}))
                        added += 1
    return added

permit_counter = 0
def new_permit_iri() -> str:
    """Generate a unique (mock) permit IRI."""
    global permit_counter
    permit_counter += 1
    return f"{EX}PERMIT-{permit_counter:03d}"

def rule_R2_approve(triples: List[Triple], fires: List[RuleFire]) -> int:
    """If eligible true and status Submitted → add status Approved and issue a permit (single-fire per application)."""
    added = 0
    for (app,_,_) in q(triples, None, f"{EX}eligible", True):
        already_approved = any(True for _ in q(triples, app, f"{EX}status", f"{EX}Approved"))
        has_permit       = any(True for _ in q(triples, app, f"{EX}issuedPermit", None))
        if (app, f"{EX}status", f"{EX}Submitted") in triples and not already_approved and not has_permit:
            if add_unique(triples, (app, f"{EX}status", f"{EX}Approved")):
                fires.append(RuleFire("R2", {"app": app}))
                added += 1
            iri = new_permit_iri()
            if add_unique(triples, (iri, "a", f"{EX}Permit")):
                added += 1
            if add_unique(triples, (app, f"{EX}issuedPermit", iri)):
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
    if (app, f"{EX}appliesFor", f"{EX}YouthDiscount") not in triples:
        errors.append("Application not for ex:YouthDiscount")
    if not list(q(triples, app, f"{EX}status", None)):
        errors.append("Missing ex:status")
    evidences = list(q(triples, app, f"{EX}hasEvidence", None))
    if len(evidences) < 2:
        errors.append("Need at least 2 evidences (age & residence)")
    return errors

def validate_person_shape(triples: List[Triple], person: str) -> List[str]:
    errors = []
    if not list(q(triples, person, f"{EX}hasAge", None)):
        errors.append("Missing ex:hasAge")
    if not list(q(triples, person, f"{EX}residenceCity", None)):
        errors.append("Missing ex:residenceCity")
    return errors

# -----------------------------------------------------------------------------
# Reasoning pipeline
# -----------------------------------------------------------------------------
def decide(triples: List[Triple]) -> ReasoningResult:
    fires: List[RuleFire] = []
    # validation
    apps = {s for (s,p,o) in triples if p == f"{EX}appliesFor" and o == f"{EX}YouthDiscount"}
    for app in apps:
        errs = validate_application_shape(triples, app)
        for (_,_,person) in q(triples, app, f"{EX}hasApplicant", None):
            errs += validate_person_shape(triples, person)
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
    app    = f"{EX}APP-001"
    status = get_final_status(triples, app)
    permit = [o for (_,_,o) in q(triples, app, f"{EX}issuedPermit", None)]
    answer = f"Application APP-001 status: {status}"
    if permit:
        answer += f"; issued permit: {permit[0].split('#')[-1]}"
    # Reason — render fired rules briefly
    lines = []
    for f in res.fires:
        if f.rule_id == "R1":
            lines.append("R1: Eligible (age<26 & city=Ghent).")
        elif f.rule_id == "R1-not":
            lines.append("R1-not: Not eligible.")
        elif f.rule_id == "R2":
            lines.append("R2: Approved + permit issued.")
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
    s1 = get_final_status(t1, f"{EX}APP-001")
    assert s1 == "Approved", "Expected Approved for base case"
    print("[1] Happy path → Approved ✓")

    # 2) Over-age -> Rejected
    t2 = clone(FACTS)
    t2 = [tr for tr in t2 if not (tr[0] == f"{EX}Alice" and tr[1] == f"{EX}hasAge")]
    t2.append((f"{EX}Alice", f"{EX}hasAge", 27))
    _, _, _ = compute_answer_reason(t2)
    s2 = get_final_status(t2, f"{EX}APP-001")
    assert s2 == "Rejected", "Over-age should lead to Rejected"
    print("[2] Over-age (27) → Rejected ✓")

    # 3) Wrong city -> Rejected
    t3 = clone(FACTS)
    t3 = [tr for tr in t3 if not (tr[0] == f"{EX}Alice" and tr[1] == f"{EX}residenceCity")]
    t3.append((f"{EX}Alice", f"{EX}residenceCity", "Brussels"))
    _, _, _ = compute_answer_reason(t3)
    s3 = get_final_status(t3, f"{EX}APP-001")
    assert s3 == "Rejected", "Wrong city should lead to Rejected"
    print("[3] Wrong city (Brussels) → Rejected ✓")

    # 4) Shape invalid (missing evidence) -> validation error
    t4 = clone(FACTS)
    t4 = [tr for tr in t4 if tr[1] != f"{EX}hasEvidence"]
    try:
        compute_answer_reason(t4)
        raise AssertionError("Expected validation failure for missing evidence")
    except ValueError:
        print("[4] Missing evidence → Validation error ✓")

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

