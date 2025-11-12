#!/usr/bin/env python3
# =============================================================================
# Agriculture — Drought Relief Grant (single-file P3 case with 4 specs)
# =============================================================================
# Problem (short): Decide if a submitted drought relief grant application by a
# farm should be Approved (and a relief payment issued) or Rejected based on:
# area ≥ 5 ha, rainfall last 30 days < 20 mm, and yield drop ≥ 30 (kg/ha).
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
#   - Reason why  : trace of rule firings
#   - Check       : concise harness verifying behavior
#
# Run:
#   python agri_drought_relief.py
#
# Optional:
#   python agri_drought_relief.py --show-spec   # print the 4 artefacts
#   python agri_drought_relief.py --show-data   # print FACTS as Turtle
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
VOCAB_TTL = """@prefix ex: <https://example.org/agri#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Farm              a rdfs:Class ; rdfs:label "Farm" .
ex:DroughtApplication a rdfs:Class ; rdfs:label "Drought Relief Application" .
ex:ReliefPayment     a rdfs:Class ; rdfs:label "Relief Payment" .

ex:DroughtReliefGrant a skos:Concept ; skos:prefLabel "Drought Relief Grant" .

ex:hasApplicant    a rdf:Property ; rdfs:domain ex:DroughtApplication ; rdfs:range ex:Farm ; rdfs:label "has applicant" .
ex:appliesFor      a rdf:Property ; rdfs:domain ex:DroughtApplication ; rdfs:range skos:Concept ; rdfs:label "applies for" .
ex:status          a rdf:Property ; rdfs:domain ex:DroughtApplication ; rdfs:label "status" .
ex:eligible        a rdf:Property ; rdfs:domain ex:DroughtApplication ; rdfs:label "eligible" .
ex:issuedPayment   a rdf:Property ; rdfs:domain ex:DroughtApplication ; rdfs:range ex:ReliefPayment ; rdfs:label "issued payment" .

ex:totalArea        a rdf:Property ; rdfs:domain ex:Farm ; rdfs:label "total area (ha)" .
ex:rainfallLast30d  a rdf:Property ; rdfs:domain ex:Farm ; rdfs:label "rainfall last 30 days (mm)" .
ex:baselineYield    a rdf:Property ; rdfs:domain ex:Farm ; rdfs:label "baseline yield (kg/ha)" .
ex:currentYield     a rdf:Property ; rdfs:domain ex:Farm ; rdfs:label "current yield (kg/ha)" .

ex:hasEvidence a rdf:Property ; rdfs:domain ex:DroughtApplication ; rdfs:label "has evidence" .
ex:AreaEvidence a rdfs:Class ; rdfs:label "Area Evidence" .
ex:RainfallEvidence a rdfs:Class ; rdfs:label "Rainfall Evidence" .
ex:YieldEvidence a rdfs:Class ; rdfs:label "Yield Evidence" .

# Minimal statuses as SKOS concepts
ex:Submitted a skos:Concept ; skos:prefLabel "Submitted" .
ex:Approved  a skos:Concept ; skos:prefLabel "Approved" .
ex:Rejected  a skos:Concept ; skos:prefLabel "Rejected" .
"""

# -----------------------------------------------------------------------------
# 2) Application Profile (SHACL)
# -----------------------------------------------------------------------------
PROFILE_SHACL_TTL = """@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix ex:  <https://example.org/agri#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:DroughtApplicationShape a sh:NodeShape ;
  sh:targetClass ex:DroughtApplication ;
  sh:property [
    sh:path ex:hasApplicant ;
    sh:minCount 1 ;
    sh:nodeKind sh:IRI ;
  ] ;
  sh:property [
    sh:path ex:appliesFor ;
    sh:hasValue ex:DroughtReliefGrant ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:status ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path ex:hasEvidence ;
    sh:minCount 3 ;                         # require at least 3 evidences (area, rainfall, yield)
  ] .

ex:FarmShape a sh:NodeShape ;
  sh:targetClass ex:Farm ;
  sh:property [
    sh:path ex:totalArea ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:rainfallLast30d ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:baselineYield ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] ;
  sh:property [
    sh:path ex:currentYield ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
  ] .
"""

# -----------------------------------------------------------------------------
# 3) Interaction Pattern / RULES_N3 (N3, triple patterns only)
# -----------------------------------------------------------------------------
RULES_N3 = """# Interaction pattern: Drought Relief Grant
# States: Submitted -> (Eligible?) -> Approved/Rejected
# Preconditions: Application conforms to DroughtApplicationShape; Farm conforms to FarmShape
# Abstract messages: submitApplication, validate, decide, notify

@prefix ex:   <https://example.org/agri#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

# R1: Eligibility — area ≥ 5, rainfallLast30d < 20, yield drop ≥ 30
# drop = baselineYield - currentYield
{
  ?app   ex:appliesFor       ex:DroughtReliefGrant .
  ?app   ex:hasApplicant     ?farm .
  ?farm  ex:totalArea        ?area .
  ?farm  ex:rainfallLast30d  ?rain .
  ?farm  ex:baselineYield    ?by .
  ?farm  ex:currentYield     ?cy .
  (?by ?cy) math:difference  ?drop .
  "5"^^xsd:integer    math:notGreaterThan ?area .
  ?rain               math:lessThan       "20"^^xsd:integer .
  ?drop               math:notLessThan    "30"^^xsd:integer .
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
  _:pay a ex:ReliefPayment .
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
IMPLEMENTATION_GUIDE_MD = """# Drought Relief Grant — Implementation Guide

This guide composes the four artefacts — **Vocabulary**, **Application Profile**, **Interaction Pattern/Rules**, and this **Implementation Guide** — into a ready-to-implement package.

## Purpose
Help authorities approve drought relief payments for farms meeting minimum area, severe rainfall deficit, and significant yield loss.

## Artefacts overview
- `VOCAB_TTL` — domain terms (RDFS/SKOS)
- `PROFILE_SHACL_TTL` — conformance shapes for inputs
- `RULES_N3` — reusable eligibility + decision flow in N3 (triple patterns only)
- the Python in this file — a **single, self-contained script** that produces:
  **Answer**, **Reason why**, and a **Check (harness)** per P3.

## Minimal HTTP binding (suggested)
1. `POST /drought-applications` → create a DroughtApplication (status=Submitted).
2. `POST /drought-applications/{id}/validate` → run SHACL (shape IDs in this guide).
3. `POST /drought-applications/{id}/decide` → execute the rules; transition to Approved/Rejected.
4. `GET  /drought-applications/{id}` → fetch current status, issued payment.

## Conformance
- Validate incoming `DroughtApplication` and `Farm` resources against the shapes.
- A server is conformant if, given data that meets the shapes, it transitions
  state identically to the rules in `RULES_N3`.

## Local testing
Run this file with Python. It prints the **Answer**, **Reason why**, and then runs its **Check** suite.
"""

# -----------------------------------------------------------------------------
# Namespaces and embedded sample data
# -----------------------------------------------------------------------------
EX = "https://example.org/agri#"

FACTS: List[Triple] = [
    # Farm
    (f"{EX}FarmAlpha",   f"{EX}totalArea",        8),     # ha
    (f"{EX}FarmAlpha",   f"{EX}rainfallLast30d",  12),    # mm
    (f"{EX}FarmAlpha",   f"{EX}baselineYield",    100),   # kg/ha
    (f"{EX}FarmAlpha",   f"{EX}currentYield",     60),    # kg/ha
    # Application
    (f"{EX}APP-DR-01",   f"{EX}hasApplicant",     f"{EX}FarmAlpha"),
    (f"{EX}APP-DR-01",   f"{EX}appliesFor",       f"{EX}DroughtReliefGrant"),
    (f"{EX}APP-DR-01",   f"{EX}status",           f"{EX}Submitted"),
    # Evidence markers (to satisfy shape)
    (f"{EX}APP-DR-01",   f"{EX}hasEvidence",      "AreaEvidence"),
    (f"{EX}APP-DR-01",   f"{EX}hasEvidence",      "RainfallEvidence"),
    (f"{EX}APP-DR-01",   f"{EX}hasEvidence",      "YieldEvidence"),
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
      - appliesFor DroughtReliefGrant
      - area >= 5
      - rainfallLast30d < 20
      - drop = baselineYield - currentYield >= 30
    """
    added = 0
    apps = [s for (s,_,_) in q(triples, None, f"{EX}appliesFor", f"{EX}DroughtReliefGrant")]
    for app in apps:
        farms = [o for (_,_,o) in q(triples, app, f"{EX}hasApplicant", None)]
        for farm in farms:
            areaL  = [o for (_,_,o) in q(triples, farm, f"{EX}totalArea", None) if isinstance(o, int)]
            rainL  = [o for (_,_,o) in q(triples, farm, f"{EX}rainfallLast30d", None) if isinstance(o, int)]
            byL    = [o for (_,_,o) in q(triples, farm, f"{EX}baselineYield", None) if isinstance(o, int)]
            cyL    = [o for (_,_,o) in q(triples, farm, f"{EX}currentYield", None) if isinstance(o, int)]
            if areaL and rainL and byL and cyL:
                area, rain, by, cy = areaL[0], rainL[0], byL[0], cyL[0]
                drop = by - cy
                ok = (area >= 5) and (rain < 20) and (drop >= 30)
                if ok:
                    if add_unique(triples, (app, f"{EX}eligible", True)):
                        fires.append(RuleFire("R1", {
                            "app": app, "farm": farm,
                            "area": str(area), "rain": str(rain),
                            "by": str(by), "cy": str(cy), "drop": str(drop)
                        }))
                        added += 1
                else:
                    if add_unique(triples, (app, f"{EX}eligible", False)):
                        fires.append(RuleFire("R1-not", {
                            "app": app, "farm": farm,
                            "area": str(area), "rain": str(rain),
                            "by": str(by), "cy": str(cy), "drop": str(drop)
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
            if add_unique(triples, (iri, "a", f"{EX}ReliefPayment")):
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
    if (app, f"{EX}appliesFor", f"{EX}DroughtReliefGrant") not in triples:
        errors.append("Application not for ex:DroughtReliefGrant")
    if not list(q(triples, app, f"{EX}status", None)):
        errors.append("Missing ex:status")
    evidences = list(q(triples, app, f"{EX}hasEvidence", None))
    if len(evidences) < 3:
        errors.append("Need at least 3 evidences (area, rainfall, yield)")
    return errors

def validate_farm_shape(triples: List[Triple], farm: str) -> List[str]:
    errors = []
    if not list(q(triples, farm, f"{EX}totalArea", None)):
        errors.append("Missing ex:totalArea")
    if not list(q(triples, farm, f"{EX}rainfallLast30d", None)):
        errors.append("Missing ex:rainfallLast30d")
    if not list(q(triples, farm, f"{EX}baselineYield", None)):
        errors.append("Missing ex:baselineYield")
    if not list(q(triples, farm, f"{EX}currentYield", None)):
        errors.append("Missing ex:currentYield")
    return errors

# -----------------------------------------------------------------------------
# Reasoning pipeline
# -----------------------------------------------------------------------------
def decide(triples: List[Triple]) -> ReasoningResult:
    fires: List[RuleFire] = []
    # validation
    apps = {s for (s,p,o) in triples if p == f"{EX}appliesFor" and o == f"{EX}DroughtReliefGrant"}
    for app in apps:
        errs = validate_application_shape(triples, app)
        for (_,_,farm) in q(triples, app, f"{EX}hasApplicant", None):
            errs += validate_farm_shape(triples, farm)
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
    app    = f"{EX}APP-DR-01"
    status = get_final_status(triples, app)
    payment = [o for (_,_,o) in q(triples, app, f"{EX}issuedPayment", None)]
    answer = f"Application APP-DR-01 status: {status}"
    if payment:
        answer += f"; issued payment: {payment[0].split('#')[-1]}"
    # Reason — brief rule trace
    lines = []
    for f in res.fires:
        if f.rule_id == "R1":
            lines.append("R1: Eligible (area≥5, rain<20, drop≥30).")
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
    s1 = get_final_status(t1, f"{EX}APP-DR-01")
    assert s1 == "Approved", "Expected Approved for base case"
    print("[1] Happy path → Approved ✓")

    # 2) Area too small (area=3) -> Rejected
    t2 = clone(FACTS)
    t2 = [tr for tr in t2 if not (tr[0] == f"{EX}FarmAlpha" and tr[1] == f"{EX}totalArea")]
    t2.append((f"{EX}FarmAlpha", f"{EX}totalArea", 3))
    _, _, _ = compute_answer_reason(t2)
    s2 = get_final_status(t2, f"{EX}APP-DR-01")
    assert s2 == "Rejected", "Small area should lead to Rejected"
    print("[2] Small area (3 ha) → Rejected ✓")

    # 3) Rainfall too high (rain=45) -> Rejected
    t3 = clone(FACTS)
    t3 = [tr for tr in t3 if not (tr[0] == f"{EX}FarmAlpha" and tr[1] == f"{EX}rainfallLast30d")]
    t3.append((f"{EX}FarmAlpha", f"{EX}rainfallLast30d", 45))
    _, _, _ = compute_answer_reason(t3)
    s3 = get_final_status(t3, f"{EX}APP-DR-01")
    assert s3 == "Rejected", "High rainfall should lead to Rejected"
    print("[3] High rainfall (45 mm) → Rejected ✓")

    # 4) Yield drop insufficient (currentYield=85 → drop=15) -> Rejected
    t4 = clone(FACTS)
    t4 = [tr for tr in t4 if not (tr[0] == f"{EX}FarmAlpha" and tr[1] == f"{EX}currentYield")]
    t4.append((f"{EX}FarmAlpha", f"{EX}currentYield", 85))
    _, _, _ = compute_answer_reason(t4)
    s4 = get_final_status(t4, f"{EX}APP-DR-01")
    assert s4 == "Rejected", "Small drop should lead to Rejected"
    print("[4] Small yield drop (15) → Rejected ✓")

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

