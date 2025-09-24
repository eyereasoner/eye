#!/usr/bin/env python3
# =============================================================================
# Leisure — Community Sports Voucher (single-file P3 case with 4 specs)
# =============================================================================
# Problem (short): Decide if a submitted leisure voucher application should be
# Approved (and a voucher issued) or Rejected based on:
# age in [12,30), sessions/week ≥ 3, weekly activity minutes ≥ 150, income < 30000.
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
#   python leisure_sports_voucher.py
#
# Optional:
#   python leisure_sports_voucher.py --show-spec   # print the 4 artefacts
#   python leisure_sports_voucher.py --show-data   # print FACTS as Turtle
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
VOCAB_TTL = """@prefix ex: <https://example.org/leisure#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Person                 a rdfs:Class ; rdfs:label "Person" ; rdfs:subClassOf foaf:Person .
ex:LeisureApplication     a rdfs:Class ; rdfs:label "Leisure Voucher Application" .
ex:LeisureVoucher         a rdfs:Class ; rdfs:label "Leisure Voucher" .

ex:SportsVoucher a skos:Concept ; skos:prefLabel "Community Sports Voucher" .

ex:hasApplicant      a rdf:Property ; rdfs:domain ex:LeisureApplication ; rdfs:range ex:Person ; rdfs:label "has applicant" .
ex:appliesFor        a rdf:Property ; rdfs:domain ex:LeisureApplication ; rdfs:range skos:Concept ; rdfs:label "applies for" .
ex:status            a rdf:Property ; rdfs:domain ex:LeisureApplication ; rdfs:label "status" .
ex:eligible          a rdf:Property ; rdfs:domain ex:LeisureApplication ; rdfs:label "eligible" .
ex:issuedVoucher     a rdf:Property ; rdfs:domain ex:LeisureApplication ; rdfs:range ex:LeisureVoucher ; rdfs:label "issued voucher" .

ex:hasAge               a rdf:Property ; rdfs:domain ex:Person ; rdfs:label "age" .
ex:commitsSessionsPerWeek a rdf:Property ; rdfs:domain ex:Person ; rdfs:label "committed sports sessions per week" .
ex:minutesPerSession    a rdf:Property ; rdfs:domain ex:Person ; rdfs:label "minutes per session" .
ex:householdIncome      a rdf:Property ; rdfs:domain ex:Person ; rdfs:label "household income" .

ex:hasEvidence a rdf:Property ; rdfs:domain ex:LeisureApplication ; rdfs:label "has evidence" .
ex:AgeEvidence a rdfs:Class ; rdfs:label "Age Evidence" .
ex:ActivityEvidence a rdfs:Class ; rdfs:label "Activity Evidence" .
ex:IncomeEvidence a rdfs:Class ; rdfs:label "Income Evidence" .

# Minimal statuses as SKOS concepts
ex:Submitted a skos:Concept ; skos:prefLabel "Submitted" .
ex:Approved  a skos:Concept ; skos:prefLabel "Approved" .
ex:Rejected  a skos:Concept ; skos:prefLabel "Rejected" .
"""

# -----------------------------------------------------------------------------
# 2) Application Profile (SHACL)
# -----------------------------------------------------------------------------
PROFILE_SHACL_TTL = """@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix ex:  <https://example.org/leisure#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:LeisureApplicationShape a sh:NodeShape ;
  sh:targetClass ex:LeisureApplication ;
  sh:property [
    sh:path ex:hasApplicant ;
    sh:minCount 1 ;
    sh:nodeKind sh:IRI ;
  ] ;
  sh:property [
    sh:path ex:appliesFor ;
    sh:hasValue ex:SportsVoucher ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:status ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:hasEvidence ;
    sh:minCount 3 ;                         # require at least 3 evidences (age, activity, income)
  ] .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:hasAge ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:commitsSessionsPerWeek ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:minutesPerSession ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:householdIncome ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] .
"""

# -----------------------------------------------------------------------------
# 3) Interaction Pattern / RULES_N3 (N3, triple patterns only)
# -----------------------------------------------------------------------------
RULES_N3 = """# Interaction pattern: Community Sports Voucher
# States: Submitted -> (Eligible?) -> Approved/Rejected
# Preconditions: Application conforms to LeisureApplicationShape; Person conforms to PersonShape
# Abstract messages: submitApplication, validate, decide, notify

@prefix ex:   <https://example.org/leisure#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

# R1: Eligibility — age∈[12,30), sessions≥3, weeklyMinutes≥150, income<30000
# weeklyMinutes = sessions * minutesPerSession
{
  ?app  ex:appliesFor           ex:SportsVoucher .
  ?app  ex:hasApplicant         ?p .
  ?p    ex:hasAge               ?age .
  ?p    ex:commitsSessionsPerWeek ?s .
  ?p    ex:minutesPerSession    ?m .
  ?p    ex:householdIncome      ?inc .
  "12"^^xsd:integer  math:notGreaterThan ?age .
  ?age  math:lessThan           "30"^^xsd:integer .
  "3"^^xsd:integer   math:notGreaterThan ?s .
  (?s ?m)            math:product        ?weekMin .
  ?weekMin           math:notLessThan    "150"^^xsd:integer .
  ?inc               math:lessThan       "30000"^^xsd:integer .
}
=>
{
  ?app ex:eligible true .
} .

# R2: Approve — eligible true AND status Submitted → status Approved + voucher issuance
{
  ?app ex:eligible true .
  ?app ex:status   ex:Submitted .
}
=>
{
  ?app ex:status ex:Approved .
  _:v a ex:LeisureVoucher .
  ?app ex:issuedVoucher _:v .
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
IMPLEMENTATION_GUIDE_MD = """# Community Sports Voucher — Implementation Guide

This guide composes the four artefacts — **Vocabulary**, **Application Profile**, **Interaction Pattern/Rules**, and this **Implementation Guide** — into a ready-to-implement package.

## Purpose
Encourage regular physical activity by subsidizing club fees for younger residents who commit to weekly sports and meet income criteria.

## Artefacts overview
- `VOCAB_TTL` — domain terms (RDFS/SKOS)
- `PROFILE_SHACL_TTL` — conformance shapes for inputs
- `RULES_N3` — reusable eligibility + decision flow in N3 (triple patterns only)
- the Python in this file — a **single, self-contained script** that produces:
  **Answer**, **Reason why**, and a **Check (harness)** per P3.

## Minimal HTTP binding (suggested)
1. `POST /leisure-applications` → create a LeisureApplication (status=Submitted).
2. `POST /leisure-applications/{id}/validate` → run SHACL (shape IDs in this guide).
3. `POST /leisure-applications/{id}/decide` → execute the rules; transition to Approved/Rejected.
4. `GET  /leisure-applications/{id}` → fetch current status, issued voucher.

## Conformance
- Validate incoming `LeisureApplication` and `Person` resources against the shapes.
- A server is conformant if, given data that meets the shapes, it transitions
  state identically to the rules in `RULES_N3`.

## Local testing
Run this file with Python. It prints the **Answer**, **Reason why**, and then runs its **Check** suite.
"""

# -----------------------------------------------------------------------------
# Namespaces and embedded sample data
# -----------------------------------------------------------------------------
EX = "https://example.org/leisure#"

FACTS: List[Triple] = [
    # Person
    (f"{EX}Liam",           f"{EX}hasAge",                 19),
    (f"{EX}Liam",           f"{EX}commitsSessionsPerWeek", 4),
    (f"{EX}Liam",           f"{EX}minutesPerSession",      45),
    (f"{EX}Liam",           f"{EX}householdIncome",        20000),
    # Application
    (f"{EX}APP-LEIS-01",    f"{EX}hasApplicant",           f"{EX}Liam"),
    (f"{EX}APP-LEIS-01",    f"{EX}appliesFor",             f"{EX}SportsVoucher"),
    (f"{EX}APP-LEIS-01",    f"{EX}status",                 f"{EX}Submitted"),
    # Evidence markers (to satisfy shape)
    (f"{EX}APP-LEIS-01",    f"{EX}hasEvidence",            "AgeEvidence"),
    (f"{EX}APP-LEIS-01",    f"{EX}hasEvidence",            "ActivityEvidence"),
    (f"{EX}APP-LEIS-01",    f"{EX}hasEvidence",            "IncomeEvidence"),
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
      - appliesFor SportsVoucher
      - 12 <= age < 30
      - sessions/week >= 3
      - weeklyMinutes = sessions * minutesPerSession >= 150
      - household income < 30000
    """
    added = 0
    apps = [s for (s,_,_) in q(triples, None, f"{EX}appliesFor", f"{EX}SportsVoucher")]
    for app in apps:
        persons = [o for (_,_,o) in q(triples, app, f"{EX}hasApplicant", None)]
        for person in persons:
            ageL  = [o for (_,_,o) in q(triples, person, f"{EX}hasAge", None) if isinstance(o, int)]
            sL    = [o for (_,_,o) in q(triples, person, f"{EX}commitsSessionsPerWeek", None) if isinstance(o, int)]
            mL    = [o for (_,_,o) in q(triples, person, f"{EX}minutesPerSession", None) if isinstance(o, int)]
            incL  = [o for (_,_,o) in q(triples, person, f"{EX}householdIncome", None) if isinstance(o, int)]
            if ageL and sL and mL and incL:
                age, sessions, minutes, inc = ageL[0], sL[0], mL[0], incL[0]
                week_min = sessions * minutes
                ok = (age >= 12) and (age < 30) and (sessions >= 3) and (week_min >= 150) and (inc < 30000)
                if ok:
                    if add_unique(triples, (app, f"{EX}eligible", True)):
                        fires.append(RuleFire("R1", {
                            "app": app, "person": person,
                            "age": str(age), "sessions": str(sessions),
                            "minutes": str(minutes), "week_min": str(week_min),
                            "income": str(inc)
                        }))
                        added += 1
                else:
                    if add_unique(triples, (app, f"{EX}eligible", False)):
                        fires.append(RuleFire("R1-not", {
                            "app": app, "person": person,
                            "age": str(age), "sessions": str(sessions),
                            "minutes": str(minutes), "week_min": str(week_min),
                            "income": str(inc)
                        }))
                        added += 1
    return added

voucher_counter = 0
def new_voucher_iri() -> str:
    """Generate a unique (mock) voucher IRI."""
    global voucher_counter
    voucher_counter += 1
    return f"{EX}VOUCHER-{voucher_counter:03d}"

def rule_R2_approve(triples: List[Triple], fires: List[RuleFire]) -> int:
    """If eligible true and status Submitted → add status Approved and issue a voucher (single-fire per application)."""
    added = 0
    for (app,_,_) in q(triples, None, f"{EX}eligible", True):
        already_approved = any(True for _ in q(triples, app, f"{EX}status", f"{EX}Approved"))
        has_voucher      = any(True for _ in q(triples, app, f"{EX}issuedVoucher", None))
        if (app, f"{EX}status", f"{EX}Submitted") in triples and not already_approved and not has_voucher:
            if add_unique(triples, (app, f"{EX}status", f"{EX}Approved")):
                fires.append(RuleFire("R2", {"app": app}))
                added += 1
            iri = new_voucher_iri()
            if add_unique(triples, (iri, "a", f"{EX}LeisureVoucher")):
                added += 1
            if add_unique(triples, (app, f"{EX}issuedVoucher", iri)):
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
    if (app, f"{EX}appliesFor", f"{EX}SportsVoucher") not in triples:
        errors.append("Application not for ex:SportsVoucher")
    if not list(q(triples, app, f"{EX}status", None)):
        errors.append("Missing ex:status")
    evidences = list(q(triples, app, f"{EX}hasEvidence", None))
    if len(evidences) < 3:
        errors.append("Need at least 3 evidences (age, activity, income)")
    return errors

def validate_person_shape(triples: List[Triple], person: str) -> List[str]:
    errors = []
    if not list(q(triples, person, f"{EX}hasAge", None)):
        errors.append("Missing ex:hasAge")
    if not list(q(triples, person, f"{EX}commitsSessionsPerWeek", None)):
        errors.append("Missing ex:commitsSessionsPerWeek")
    if not list(q(triples, person, f"{EX}minutesPerSession", None)):
        errors.append("Missing ex:minutesPerSession")
    if not list(q(triples, person, f"{EX}householdIncome", None)):
        errors.append("Missing ex:householdIncome")
    return errors

# -----------------------------------------------------------------------------
# Reasoning pipeline
# -----------------------------------------------------------------------------
def decide(triples: List[Triple]) -> ReasoningResult:
    fires: List[RuleFire] = []
    # validation
    apps = {s for (s,p,o) in triples if p == f"{EX}appliesFor" and o == f"{EX}SportsVoucher"}
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
    app    = f"{EX}APP-LEIS-01"
    status = get_final_status(triples, app)
    voucher = [o for (_,_,o) in q(triples, app, f"{EX}issuedVoucher", None)]
    answer = f"Application APP-LEIS-01 status: {status}"
    if voucher:
        answer += f"; issued voucher: {voucher[0].split('#')[-1]}"
    # Reason — brief rule trace
    lines = []
    for f in res.fires:
        if f.rule_id == "R1":
            lines.append("R1: Eligible (age∈[12,30), sessions≥3, weeklyMinutes≥150, income<30000).")
        elif f.rule_id == "R1-not":
            lines.append("R1-not: Not eligible.")
        elif f.rule_id == "R2":
            lines.append("R2: Approved + voucher issued.")
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
    s1 = get_final_status(t1, f"{EX}APP-LEIS-01")
    assert s1 == "Approved", "Expected Approved for base case"
    print("[1] Happy path → Approved ✓")

    # 2) Too few sessions (2) -> Rejected
    t2 = clone(FACTS)
    t2 = [tr for tr in t2 if not (tr[0] == f"{EX}Liam" and tr[1] == f"{EX}commitsSessionsPerWeek")]
    t2.append((f"{EX}Liam", f"{EX}commitsSessionsPerWeek", 2))
    _, _, _ = compute_answer_reason(t2)
    s2 = get_final_status(t2, f"{EX}APP-LEIS-01")
    assert s2 == "Rejected", "Too few sessions should lead to Rejected"
    print("[2] Sessions/week = 2 → Rejected ✓")

    # 3) Weekly minutes below threshold (3×40 = 120) -> Rejected
    t3 = clone(FACTS)
    t3 = [tr for tr in t3 if not (tr[0] == f"{EX}Liam" and tr[1] == f"{EX}minutesPerSession")]
    t3.append((f"{EX}Liam", f"{EX}minutesPerSession", 40))
    t3 = [tr for tr in t3 if not (tr[0] == f"{EX}Liam" and tr[1] == f"{EX}commitsSessionsPerWeek")] + [(f"{EX}Liam", f"{EX}commitsSessionsPerWeek", 3)]
    _, _, _ = compute_answer_reason(t3)
    s3 = get_final_status(t3, f"{EX}APP-LEIS-01")
    assert s3 == "Rejected", "Weekly minutes < 150 should lead to Rejected"
    print("[3] Weekly minutes = 120 → Rejected ✓")

    # 4) Boundary weekly minutes exactly 150 (5×30) -> Approved
    t4 = clone(FACTS)
    t4 = [tr for tr in t4 if not (tr[0] == f"{EX}Liam" and tr[1] in (f"{EX}commitsSessionsPerWeek", f"{EX}minutesPerSession"))]
    t4 += [(f"{EX}Liam", f"{EX}commitsSessionsPerWeek", 5),
           (f"{EX}Liam", f"{EX}minutesPerSession", 30)]
    _, _, _ = compute_answer_reason(t4)
    s4 = get_final_status(t4, f"{EX}APP-LEIS-01")
    assert s4 == "Approved", "Boundary 150 minutes should be Approved"
    print("[4] Weekly minutes = 150 → Approved ✓")

    # 5) Income too high (40000) -> Rejected
    t5 = clone(FACTS)
    t5 = [tr for tr in t5 if not (tr[0] == f"{EX}Liam" and tr[1] == f"{EX}householdIncome")]
    t5.append((f"{EX}Liam", f"{EX}householdIncome", 40000))
    _, _, _ = compute_answer_reason(t5)
    s5 = get_final_status(t5, f"{EX}APP-LEIS-01")
    assert s5 == "Rejected", "High income should lead to Rejected"
    print("[5] Household income = 40000 → Rejected ✓")

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

