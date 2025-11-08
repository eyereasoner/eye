#!/usr/bin/env python3
# =============================================================================
# Mobility — BikePass Youth Discount (single-file P3 case with 4 specs)
# =============================================================================
# Problem (short): Decide if a submitted youth BikePass discount application
# should be Approved (and a pass issued) or Rejected based on:
# age in [12,26), committed trips/week ≥ 3; if eligible, compute approvedPrice:
#   - age < 18  → 60% discount
#   - 18 ≤ age < 26 → 40% discount
#
# This file is completely self-contained and uses no third-party packages.
#
# It demonstrates a use case that follows “four types of specification artefacts” [1]:
#
#   1) Vocabulary (RDFS/SKOS)
#   2) Application Profile (SHACL)
#   3) Interaction Pattern / Rules (N3; triple patterns only, math:* built-ins)
#   4) Implementation Guide (human-readable composition/integration notes)
#
# and implements the P3 style:
#   - Answer      : final result
#   - Reason why  : brief trace of rule firings
#   - Check       : concise harness verifying behavior
#
# Run:
#   python bikepass_youth_discount.py
#
# Optional:
#   python bikepass_youth_discount.py --show-spec   # print the 4 artefacts
#   python bikepass_youth_discount.py --show-data   # print FACTS as Turtle
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
VOCAB_TTL = """@prefix ex: <https://example.org/bikepass#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Person                a rdfs:Class ; rdfs:label "Person" ; rdfs:subClassOf foaf:Person .
ex:BikePassApplication   a rdfs:Class ; rdfs:label "BikePass Discount Application" .
ex:BikePass              a rdfs:Class ; rdfs:label "BikePass" .

ex:YouthDiscount a skos:Concept ; skos:prefLabel "BikePass Youth Discount" .

ex:hasApplicant     a rdf:Property ; rdfs:domain ex:BikePassApplication ; rdfs:range ex:Person ; rdfs:label "has applicant" .
ex:appliesFor       a rdf:Property ; rdfs:domain ex:BikePassApplication ; rdfs:range skos:Concept ; rdfs:label "applies for" .
ex:status           a rdf:Property ; rdfs:domain ex:BikePassApplication ; rdfs:label "status" .
ex:eligible         a rdf:Property ; rdfs:domain ex:BikePassApplication ; rdfs:label "eligible" .
ex:issuedPass       a rdf:Property ; rdfs:domain ex:BikePassApplication ; rdfs:range ex:BikePass ; rdfs:label "issued pass" .
ex:approvedPrice    a rdf:Property ; rdfs:domain ex:BikePassApplication ; rdfs:label "approved price (monthly)" .

ex:hasAge           a rdf:Property ; rdfs:domain ex:Person ; rdfs:label "age" .
ex:tripsPerWeek     a rdf:Property ; rdfs:domain ex:Person ; rdfs:label "committed trips per week" .

ex:baseMonthlyPrice a rdf:Property ; rdfs:domain ex:BikePassApplication ; rdfs:label "base monthly price" .

ex:hasEvidence a rdf:Property ; rdfs:domain ex:BikePassApplication ; rdfs:label "has evidence" .
ex:AgeEvidence a rdfs:Class ; rdfs:label "Age Evidence" .
ex:TripsEvidence a rdfs:Class ; rdfs:label "Trips Evidence" .
ex:PriceEvidence a rdfs:Class ; rdfs:label "Price Evidence" .

# Minimal statuses as SKOS concepts
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

ex:BikePassApplicationShape a sh:NodeShape ;
  sh:targetClass ex:BikePassApplication ;
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
    sh:path ex:baseMonthlyPrice ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:status ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:hasEvidence ;
    sh:minCount 3 ;                         # require at least 3 evidences (age, trips, price)
  ] .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:hasAge ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:tripsPerWeek ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] .
"""

# -----------------------------------------------------------------------------
# 3) Interaction Pattern / RULES_N3 (N3, triple patterns only)
# -----------------------------------------------------------------------------
RULES_N3 = """# Interaction pattern: BikePass Youth Discount
# States: Submitted -> (Eligible?) -> Approved/Rejected
# Preconditions: Application conforms to BikePassApplicationShape; Person conforms to PersonShape
# Abstract messages: submitApplication, validate, decide, notify

@prefix ex:   <https://example.org/bikepass#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

# R1: Eligibility — age∈[12,26) and tripsPerWeek ≥ 3
{
  ?app  ex:appliesFor   ex:YouthDiscount .
  ?app  ex:hasApplicant ?p .
  ?p    ex:hasAge       ?age .
  ?p    ex:tripsPerWeek ?t .
  "12"^^xsd:integer  math:notGreaterThan ?age .
  ?age  math:lessThan "26"^^xsd:integer .
  "3"^^xsd:integer   math:notGreaterThan ?t .
}
=>
{
  ?app ex:eligible true .
} .

# R2a: Approve & compute price for age < 18 → 60% discount
# price = base - (base * 60 / 100)
{
  ?app  ex:eligible true .
  ?app  ex:status   ex:Submitted .
  ?app  ex:baseMonthlyPrice ?bp .
  ?app  ex:hasApplicant ?p .
  ?p    ex:hasAge ?age .
  ?age  math:lessThan "18"^^xsd:integer .
  (?bp "60"^^xsd:integer)  math:product  ?tmp .
  (?tmp "100"^^xsd:integer) math:quotient ?discAmt .
  (?bp ?discAmt)           math:difference ?price .
}
=>
{
  ?app ex:status ex:Approved .
  ?app ex:approvedPrice ?price .
  _:pass a ex:BikePass .
  ?app ex:issuedPass _:pass .
} .

# R2b: Approve & compute price for 18 ≤ age < 26 → 40% discount
# price = base - (base * 40 / 100)
{
  ?app  ex:eligible true .
  ?app  ex:status   ex:Submitted .
  ?app  ex:baseMonthlyPrice ?bp .
  ?app  ex:hasApplicant ?p .
  ?p    ex:hasAge ?age .
  "18"^^xsd:integer math:notGreaterThan ?age .
  ?age  math:lessThan "26"^^xsd:integer .
  (?bp "40"^^xsd:integer)  math:product  ?tmp2 .
  (?tmp2 "100"^^xsd:integer) math:quotient ?discAmt2 .
  (?bp ?discAmt2)           math:difference ?price2 .
}
=>
{
  ?app ex:status ex:Approved .
  ?app ex:approvedPrice ?price2 .
  _:pass a ex:BikePass .
  ?app ex:issuedPass _:pass .
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
Boost bike adoption among young residents by offering a discounted BikePass to 12–25 year olds who commit to regular trips.

## Artefacts overview
- `VOCAB_TTL` — domain terms (RDFS/SKOS)
- `PROFILE_SHACL_TTL` — conformance shapes for inputs
- `RULES_N3` — reusable eligibility + decision flow in N3 (triple patterns only)
- the Python in this file — a **single, self-contained script** that produces:
  **Answer**, **Reason why**, and a **Check (harness)** per P3.

## Minimal HTTP binding (suggested)
1. `POST /bikepass-applications` → create a BikePassApplication (status=Submitted).
2. `POST /bikepass-applications/{id}/validate` → run SHACL (shape IDs in this guide).
3. `POST /bikepass-applications/{id}/decide` → execute the rules; transition to Approved/Rejected.
4. `GET  /bikepass-applications/{id}` → fetch current status, approved price, issued pass.

## Conformance
- Validate incoming `BikePassApplication` and `Person` resources against the shapes.
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
    # Person (Zoe): age 17, commits 5 trips/week
    (f"{EX}Zoe",             f"{EX}hasAge",         17),
    (f"{EX}Zoe",             f"{EX}tripsPerWeek",   5),

    # Application with base price 30 (currency omitted; integer units)
    (f"{EX}APP-BP-01",       f"{EX}hasApplicant",   f"{EX}Zoe"),
    (f"{EX}APP-BP-01",       f"{EX}appliesFor",     f"{EX}YouthDiscount"),
    (f"{EX}APP-BP-01",       f"{EX}status",         f"{EX}Submitted"),
    (f"{EX}APP-BP-01",       f"{EX}baseMonthlyPrice", 30),

    # Evidence markers (to satisfy shape)
    (f"{EX}APP-BP-01",       f"{EX}hasEvidence",    "AgeEvidence"),
    (f"{EX}APP-BP-01",       f"{EX}hasEvidence",    "TripsEvidence"),
    (f"{EX}APP-BP-01",       f"{EX}hasEvidence",    "PriceEvidence"),
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
      - appliesFor YouthDiscount
      - 12 ≤ age < 26
      - trips/week ≥ 3
    """
    added = 0
    apps = [s for (s,_,_) in q(triples, None, f"{EX}appliesFor", f"{EX}YouthDiscount")]
    for app in apps:
        persons = [o for (_,_,o) in q(triples, app, f"{EX}hasApplicant", None)]
        for person in persons:
            ageL  = [o for (_,_,o) in q(triples, person, f"{EX}hasAge", None) if isinstance(o, int)]
            tripsL= [o for (_,_,o) in q(triples, person, f"{EX}tripsPerWeek", None) if isinstance(o, int)]
            if ageL and tripsL:
                age, trips = ageL[0], tripsL[0]
                ok = (age >= 12) and (age < 26) and (trips >= 3)
                if ok:
                    if add_unique(triples, (app, f"{EX}eligible", True)):
                        fires.append(RuleFire("R1", {
                            "app": app, "person": person,
                            "age": str(age), "trips": str(trips)
                        }))
                        added += 1
                else:
                    if add_unique(triples, (app, f"{EX}eligible", False)):
                        fires.append(RuleFire("R1-not", {
                            "app": app, "person": person,
                            "age": str(age), "trips": str(trips)
                        }))
                        added += 1
    return added

pass_counter = 0
def new_pass_iri() -> str:
    """Generate a unique (mock) BikePass IRI."""
    global pass_counter
    pass_counter += 1
    return f"{EX}PASS-{pass_counter:03d}"

def rule_R2a_approve_under18(triples: List[Triple], fires: List[RuleFire]) -> int:
    """If eligible true, status Submitted, age<18 → approve with 60% discount and issue pass."""
    added = 0
    for (app,_,_) in q(triples, None, f"{EX}eligible", True):
        if (app, f"{EX}status", f"{EX}Submitted") not in triples:
            continue
        already_approved = any(True for _ in q(triples, app, f"{EX}status", f"{EX}Approved"))
        has_pass         = any(True for _ in q(triples, app, f"{EX}issuedPass", None))
        if already_approved or has_pass:
            continue
        # Gather data
        personL = [o for (_,_,o) in q(triples, app, f"{EX}hasApplicant", None)]
        priceL  = [o for (_,_,o) in q(triples, app, f"{EX}baseMonthlyPrice", None) if isinstance(o, int)]
        if not personL or not priceL:
            continue
        person, base = personL[0], priceL[0]
        ageL = [o for (_,_,o) in q(triples, person, f"{EX}hasAge", None) if isinstance(o, int)]
        if not ageL:
            continue
        age = ageL[0]
        if age < 18:
            disc_amount = (base * 60) // 100
            price = base - disc_amount
            if add_unique(triples, (app, f"{EX}status", f"{EX}Approved")):
                fires.append(RuleFire("R2a", {"app": app, "rate": "60", "price": str(price)}))
                added += 1
            if add_unique(triples, (app, f"{EX}approvedPrice", price)):
                added += 1
            iri = new_pass_iri()
            if add_unique(triples, (iri, "a", f"{EX}BikePass")):
                added += 1
            if add_unique(triples, (app, f"{EX}issuedPass", iri)):
                added += 1
    return added

def rule_R2b_approve_youth(triples: List[Triple], fires: List[RuleFire]) -> int:
    """If eligible true, status Submitted, 18≤age<26 → approve with 40% discount and issue pass."""
    added = 0
    for (app,_,_) in q(triples, None, f"{EX}eligible", True):
        if (app, f"{EX}status", f"{EX}Submitted") not in triples:
            continue
        already_approved = any(True for _ in q(triples, app, f"{EX}status", f"{EX}Approved"))
        has_pass         = any(True for _ in q(triples, app, f"{EX}issuedPass", None))
        if already_approved or has_pass:
            continue
        personL = [o for (_,_,o) in q(triples, app, f"{EX}hasApplicant", None)]
        priceL  = [o for (_,_,o) in q(triples, app, f"{EX}baseMonthlyPrice", None) if isinstance(o, int)]
        if not personL or not priceL:
            continue
        person, base = personL[0], priceL[0]
        ageL = [o for (_,_,o) in q(triples, person, f"{EX}hasAge", None) if isinstance(o, int)]
        if not ageL:
            continue
        age = ageL[0]
        if 18 <= age < 26:
            disc_amount = (base * 40) // 100
            price = base - disc_amount
            if add_unique(triples, (app, f"{EX}status", f"{EX}Approved")):
                fires.append(RuleFire("R2b", {"app": app, "rate": "40", "price": str(price)}))
                added += 1
            if add_unique(triples, (app, f"{EX}approvedPrice", price)):
                added += 1
            iri = new_pass_iri()
            if add_unique(triples, (iri, "a", f"{EX}BikePass")):
                added += 1
            if add_unique(triples, (app, f"{EX}issuedPass", iri)):
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
    rule_R2a_approve_under18,
    rule_R2b_approve_youth,
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
    if not list(q(triples, app, f"{EX}baseMonthlyPrice", None)):
        errors.append("Missing ex:baseMonthlyPrice")
    evidences = list(q(triples, app, f"{EX}hasEvidence", None))
    if len(evidences) < 3:
        errors.append("Need at least 3 evidences (age, trips, price)")
    return errors

def validate_person_shape(triples: List[Triple], person: str) -> List[str]:
    errors = []
    if not list(q(triples, person, f"{EX}hasAge", None)):
        errors.append("Missing ex:hasAge")
    if not list(q(triples, person, f"{EX}tripsPerWeek", None)):
        errors.append("Missing ex:tripsPerWeek")
    return errors

# -----------------------------------------------------------------------------
# Reasoning pipeline
# -----------------------------------------------------------------------------
@dataclass
class RuleFire:
    rule_id: str
    bindings: Dict[str, str]

@dataclass
class ReasoningResult:
    added: int
    fires: List[RuleFire] = field(default_factory=list)

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

def get_approved_price(triples: List[Triple], app: str):
    vals = [o for (s,p,o) in triples if s == app and p == f"{EX}approvedPrice"]
    return vals[-1] if vals else None

def compute_answer_reason(triples: List[Triple]):
    res = decide(triples)
    app    = f"{EX}APP-BP-01"
    status = get_final_status(triples, app)
    price  = get_approved_price(triples, app)
    issued = [o for (_,_,o) in q(triples, app, f"{EX}issuedPass", None)]
    answer = f"Application APP-BP-01 status: {status}"
    if price is not None:
        answer += f"; approved price: {price}"
    if issued:
        answer += f"; issued pass: {issued[0].split('#')[-1]}"
    # Reason — brief rule trace
    lines = []
    for f in res.fires:
        if f.rule_id == "R1":
            lines.append("R1: Eligible (age∈[12,26), trips/week≥3).")
        elif f.rule_id == "R1-not":
            lines.append("R1-not: Not eligible.")
        elif f.rule_id == "R2a":
            lines.append("R2a: Approved with 60% discount.")
        elif f.rule_id == "R2b":
            lines.append("R2b: Approved with 40% discount.")
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
    # 1) Happy path (age 17) -> Approved, 60% discount on 30 => 12
    t1 = clone(FACTS)
    _, _, _ = compute_answer_reason(t1)
    s1 = get_final_status(t1, f"{EX}APP-BP-01")
    p1 = get_approved_price(t1, f"{EX}APP-BP-01")
    assert s1 == "Approved" and p1 == 12, "Expected Approved with price 12"
    print("[1] Age 17 → Approved, price 12 ✓")

    # 2) Age 22 → Approved with 40% discount (price 18)
    t2 = clone(FACTS)
    t2 = [tr for tr in t2 if not (tr[0] == f"{EX}Zoe" and tr[1] == f"{EX}hasAge")]
    t2.append((f"{EX}Zoe", f"{EX}hasAge", 22))
    _, _, _ = compute_answer_reason(t2)
    s2 = get_final_status(t2, f"{EX}APP-BP-01")
    p2 = get_approved_price(t2, f"{EX}APP-BP-01")
    assert s2 == "Approved" and p2 == 18, "Expected Approved with price 18"
    print("[2] Age 22 → Approved, price 18 ✓")

    # 3) Age 26 (too old) → Rejected
    t3 = clone(FACTS)
    t3 = [tr for tr in t3 if not (tr[0] == f"{EX}Zoe" and tr[1] == f"{EX}hasAge")]
    t3.append((f"{EX}Zoe", f"{EX}hasAge", 26))
    _, _, _ = compute_answer_reason(t3)
    s3 = get_final_status(t3, f"{EX}APP-BP-01")
    assert s3 == "Rejected", "Too old should lead to Rejected"
    print("[3] Age 26 → Rejected ✓")

    # 4) Too few trips (2) → Rejected
    t4 = clone(FACTS)
    t4 = [tr for tr in t4 if not (tr[0] == f"{EX}Zoe" and tr[1] == f"{EX}tripsPerWeek")]
    t4.append((f"{EX}Zoe", f"{EX}tripsPerWeek", 2))
    _, _, _ = compute_answer_reason(t4)
    s4 = get_final_status(t4, f"{EX}APP-BP-01")
    assert s4 == "Rejected", "Too few trips should lead to Rejected"
    print("[4] Trips/week = 2 → Rejected ✓")

    # 5) Boundary: age exactly 18 → Approved, 40% discount
    t5 = clone(FACTS)
    t5 = [tr for tr in t5 if not (tr[0] == f"{EX}Zoe" and tr[1] == f"{EX}hasAge")]
    t5.append((f"{EX}Zoe", f"{EX}hasAge", 18))
    _, _, _ = compute_answer_reason(t5)
    s5 = get_final_status(t5, f"{EX}APP-BP-01")
    p5 = get_approved_price(t5, f"{EX}APP-BP-01")
    assert s5 == "Approved" and p5 == 18, "Age 18 should be 40% discount"
    print("[5] Age 18 → Approved, price 18 ✓")

    # 6) Shape invalid (remove baseMonthlyPrice) → validation error
    t6 = clone(FACTS)
    t6 = [tr for tr in t6 if not (tr[0] == f"{EX}APP-BP-01" and tr[1] == f"{EX}baseMonthlyPrice")]
    try:
        compute_answer_reason(t6)
        raise AssertionError("Expected validation failure for missing baseMonthlyPrice")
    except ValueError:
        print("[6] Missing baseMonthlyPrice → Validation error ✓")

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

