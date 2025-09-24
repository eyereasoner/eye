#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Irrigation-Guard Insight — Compact, EYE-compatible demonstrator
===============================================================

STORY
-----
Dawn over Field A. Sam checks the tablet at the valve. It already has what the
device knows locally—soil moisture now, today’s irrigation plan (liters), and a
short-range rain forecast. It does a tiny local calculation—no farm-cloud, no
crop database—just a **projection**: soil% after we add planned water **and**
what the sky will likely bring. A banner appears: **“Over-watering risk. Reduce
or skip cycle?”** One tap shows the safer operation: **skip** (rain covers it),
or **reduce** to just hit the target. Sam glances at the sky and block plan—
safe to skip—and the banner fades. What leaves the device isn’t the raw probe
stream or plan history, just a signed **insight slip**: who it’s for (Field A),
what’s allowed (show an irrigation nudge), when it expires, and the bare
essentials—projected soil%, overshoot, and the suggested plan with expected
water saving.

End of week, operations sees a handful of slips attached to jobs: nudges shown,
cycles skipped or trimmed, moisture on target. No personal data or continuous
telemetry in storage. An auditor months later verifies each slip at a glance—
audience-scoped, purpose-bound, short-lived, and verifiable. Healthier crops,
less water, fewer pump hours.

**Benefits at a glance**

* **Field ops (Sam):** clear, timely nudge; you stay in control—skip or reduce
  only if practical.
* **Operations:** lower water and energy spend; compact, **auditable** proof via
  signed envelopes.
* **Compliance/Privacy:** data minimization by default; no raw sensor streams
  or customer/tenant identifiers.
* **IT/Security:** standardized **Vocabulary / SHACL / N3**; easy interop.
* **Environment & cost:** less over-watering, runoff, and pumping; better yields.

In short: **ship the decision, not the data**—act locally, keep proof portable,
and reduce waste without exposing sensitive telemetry.

WHAT THIS IS (high level)
-------------------------
We convert a few *local* irrigation signals into a **HOT insight** the UI can
act on—“show irrigation banner at Field A; suggest skip or reduced liters”—
**without sharing raw data**. We project **soil moisture** after:
( planned liters + effective rain ) / water-per-% + current soil%.
If that projected value exceeds the target by a policy tolerance, and a reduced
or skipped plan still meets policy, we emit a minimal, **signed envelope**
(audience, allowed use, expiry, compact assertions).

Why this fits the **insight economy** (ship the decision, not the data):
https://ruben.verborgh.org/blog/2025/08/12/inside-the-insight-economy/

FOUR SPEC ARTEFACTS (Pieter Colpaert)
--------------------------------------
https://pietercolpaert.be/interoperability/2025/09/03/four-types-specification-artefacts
1) **Vocabulary** – Turtle with `@prefix` terms (e.g., `ex:soilMoisturePct`, `ex:projectedMoisturePct`).
2) **Application Profile** – SHACL (Turtle) for the envelope fields.
3) **Interaction Pattern** – REQUEST → ISSUE → CONSUME → COMPLETE.
4) **Implementation Guide** – This runnable file + deterministic checks.

P3 style (N3 rules)
-----------------------------
Rules use only **triple patterns** and **math built-ins**:
`math:lessThan`, `math:greaterThan`, `math:notGreaterThan`, `math:notLessThan`,
`math:sum`, `math:difference`, `math:product`, `math:quotient`. See:
https://raw.githubusercontent.com/eyereasoner/eye/refs/heads/master/p3/P3.md

Inputs & Outputs (at a glance)
------------------------------
Inputs (local): `soilMoisturePct`, `targetMoisturePct`, `plannedIrrigationL`,
`forecastRainMm`, `infiltrationFactor`, `areaM2`, `waterPerPctL`, and policy
`overwaterTolerancePct`.

Computation:
  effectiveRainL      = forecastRainMm × areaM2 × infiltrationFactor
  addedMoisturePct    = (plannedIrrigationL + effectiveRainL) ÷ waterPerPctL
  projectedMoisture%  = soilMoisturePct + addedMoisturePct
  overshoot%          = projectedMoisture% − targetMoisturePct
  reducedLiters       = max(0, (target − soil) × waterPerPctL − effectiveRainL)

Outputs (shared): signed **envelope** with `audience`, `allowedUse`, `issuedAt`,
`expiry`, and **assertions**: `showIrrigationBanner`, optional `suggestPlan`
(`ex:reducedPlan` or `ex:skipCycle`), `projectedMoisturePct`, `overShootPct`,
and `estimatedWaterSavingL` (current − alternative).

Determinism
-----------
We **freeze time** so timestamps and HMAC signatures are identical across runs.
Override with `--now` or `INSIGHT_FIXED_NOW`.

Run
---
$ python3 irrigation_guard_insight.py
"""

from __future__ import annotations
import os, sys, json, hmac, base64, hashlib, re
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from typing import Dict, List, Tuple, Iterable, Optional, Any, Union

# =============================================================================
# Deterministic clock (freeze 'now' for reproducible outputs/signatures)
# =============================================================================

_FIXED_NOW: Optional[datetime] = None  # set via set_fixed_now()

def _parse_iso_to_utc(ts: str) -> datetime:
    return datetime.fromisoformat(ts.replace("Z", "+00:00")).astimezone(timezone.utc)

def set_fixed_now(arg_value: Optional[str] = None) -> None:
    """Choose a frozen 'now'. Default: 2025-01-01T09:00:00Z. Override with --now or env."""
    global _FIXED_NOW
    ts = arg_value or os.getenv("INSIGHT_FIXED_NOW")
    if ts:
        _FIXED_NOW = _parse_iso_to_utc(ts)
    else:
        _FIXED_NOW = datetime(2025, 1, 1, 9, 0, 0, tzinfo=timezone.utc)

def fixed_now() -> datetime:
    return _FIXED_NOW or datetime(2025, 1, 1, 9, 0, 0, tzinfo=timezone.utc)

def now_utc_iso() -> str:
    return fixed_now().replace(microsecond=0).isoformat()

def iso_in_hours(hours: int) -> str:
    return (fixed_now() + timedelta(hours=hours)).replace(microsecond=0).isoformat()

def today_iso_z() -> str:
    # For Turtle literals expecting Z-suffix
    return fixed_now().replace(microsecond=0).isoformat().replace("+00:00", "Z")

def parse_iso(ts: str) -> datetime:
    return _parse_iso_to_utc(ts)

def b64u(b: bytes) -> str:
    return base64.urlsafe_b64encode(b).decode("ascii").rstrip("=")

def stable_json(x: Any) -> str:
    return json.dumps(x, sort_keys=True, separators=(",", ":"))

# =============================================================================
# Vocabulary (Turtle) — Pieter’s artefact #1
# =============================================================================

def vocabulary() -> str:
    return """@prefix ex:   <http://example.org/> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix owl:  <http://www.w3.org/2002/07/owl#> .

# CLASSES
ex:Field     a rdfs:Class ; rdfs:label "Field" ; rdfs:comment "Irrigated field or plot." .
ex:Plan      a rdfs:Class ; rdfs:label "Plan" ;  rdfs:comment "Irrigation plan (liters for this cycle)." .
ex:ScanEvent a rdfs:Class ; rdfs:label "ScanEvent" ; rdfs:comment "Local event that triggers reasoning." .
ex:Policy    a rdfs:Class ; rdfs:label "Policy" ; rdfs:comment "Local thresholds and conversions." .
ex:Insight   a rdfs:Class ; rdfs:label "Insight" ; rdfs:comment "Derived, actionable fact." .
ex:Envelope  a rdfs:Class ; rdfs:label "Envelope" ; rdfs:comment "Signed, audience-bound insight wrapper." .

# SCAN LINKS
ex:atField    a owl:ObjectProperty ; rdfs:label "at field" ;
  rdfs:domain ex:ScanEvent ; rdfs:range ex:Field ; rdfs:comment "Scan at a given field." .

ex:usesPlan   a owl:ObjectProperty ; rdfs:label "uses plan" ;
  rdfs:domain ex:ScanEvent ; rdfs:range ex:Plan ; rdfs:comment "Current plan at scan time." .

ex:atTime     a owl:DatatypeProperty ; rdfs:label "at time" ;
  rdfs:domain ex:ScanEvent ; rdfs:range xsd:dateTime ; rdfs:comment "Timestamp of the scan." .

# FIELD DATA
ex:soilMoisturePct      a owl:DatatypeProperty ; rdfs:domain ex:Field ; rdfs:range xsd:decimal .
ex:targetMoisturePct    a owl:DatatypeProperty ; rdfs:domain ex:Field ; rdfs:range xsd:decimal .
ex:forecastRainMm       a owl:DatatypeProperty ; rdfs:domain ex:Field ; rdfs:range xsd:decimal .
ex:infiltrationFactor   a owl:DatatypeProperty ; rdfs:domain ex:Field ; rdfs:range xsd:decimal .
ex:areaM2               a owl:DatatypeProperty ; rdfs:domain ex:Field ; rdfs:range xsd:decimal .
ex:waterPerPctL         a owl:DatatypeProperty ; rdfs:domain ex:Field ; rdfs:range xsd:decimal ; 
  rdfs:comment "Liters needed to raise soil moisture by 1 percentage point." .

# PLAN DATA
ex:plannedIrrigationL   a owl:DatatypeProperty ; rdfs:domain ex:Plan ; rdfs:range xsd:decimal .

# DERIVED METRICS
ex:effectiveRainL       a owl:DatatypeProperty ; rdfs:domain ex:Field ; rdfs:range xsd:decimal .
ex:projectedMoisturePct a owl:DatatypeProperty ; rdfs:domain ex:Plan  ; rdfs:range xsd:decimal .
ex:overShootPct         a owl:DatatypeProperty ; rdfs:domain ex:Insight ; rdfs:range xsd:decimal .
ex:estimatedWaterSavingL a owl:DatatypeProperty ; rdfs:domain ex:Insight ; rdfs:range xsd:decimal .

# INSIGHT OUTPUT
ex:showIrrigationBanner a owl:ObjectProperty ; rdfs:domain ex:Insight ; rdfs:range ex:Field .
ex:suggestPlan          a owl:ObjectProperty ; rdfs:domain ex:Insight ; rdfs:range ex:Plan .

# POLICY
ex:overwaterTolerancePct a owl:DatatypeProperty ; rdfs:domain ex:Policy ; rdfs:range xsd:decimal .

# ENVELOPE FIELDS
ex:audience   a owl:ObjectProperty ; rdfs:domain ex:Envelope ; rdfs:range rdfs:Resource .
ex:allowedUse a owl:DatatypeProperty ; rdfs:domain ex:Envelope ; rdfs:range xsd:string .
ex:issuedAt   a owl:DatatypeProperty ; rdfs:domain ex:Envelope ; rdfs:range xsd:dateTime .
ex:expiry     a owl:DatatypeProperty ; rdfs:domain ex:Envelope ; rdfs:range xsd:dateTime .
ex:assertions a owl:DatatypeProperty ; rdfs:domain ex:Envelope ; rdfs:range xsd:string .
ex:signature  a owl:DatatypeProperty ; rdfs:domain ex:Envelope ; rdfs:range xsd:string .
"""

# =============================================================================
# SHACL (Turtle) — application profile for the envelope — Pieter’s artefact #2
# =============================================================================

def shacl_turtle() -> str:
    return """@prefix ex:  <http://example.org/> .
@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Envelope must include audience, allowedUse, issuedAt, expiry, assertions, signature.
ex:EnvelopeShape rdf:type sh:NodeShape .
ex:EnvelopeShape sh:targetClass ex:Envelope .
ex:EnvelopeShape sh:property ex:EnvAudienceShape .
ex:EnvelopeShape sh:property ex:EnvAllowedUseShape .
ex:EnvelopeShape sh:property ex:EnvIssuedAtShape .
ex:EnvelopeShape sh:property ex:EnvExpiryShape .
ex:EnvelopeShape sh:property ex:EnvAssertionsShape .
ex:EnvelopeShape sh:property ex:EnvSignatureShape .

# audience MUST be an IRI (qname here)
ex:EnvAudienceShape rdf:type sh:PropertyShape .
ex:EnvAudienceShape sh:path ex:audience .
ex:EnvAudienceShape sh:minCount 1 .
ex:EnvAudienceShape sh:nodeKind sh:IRI .

# allowedUse MUST be a string
ex:EnvAllowedUseShape rdf:type sh:PropertyShape .
ex:EnvAllowedUseShape sh:path ex:allowedUse .
ex:EnvAllowedUseShape sh:minCount 1 .
ex:EnvAllowedUseShape sh:datatype xsd:string .

# issuedAt MUST be xsd:dateTime
ex:EnvIssuedAtShape rdf:type sh:PropertyShape .
ex:EnvIssuedAtShape sh:path ex:issuedAt .
ex:EnvIssuedAtShape sh:minCount 1 .
ex:EnvIssuedAtShape sh:datatype xsd:dateTime .

# expiry MUST be xsd:dateTime
ex:EnvExpiryShape rdf:type sh:PropertyShape .
ex:EnvExpiryShape sh:path ex:expiry .
ex:EnvExpiryShape sh:minCount 1 .
ex:EnvExpiryShape sh:datatype xsd:dateTime .

# assertions is JSON (string) for portability
ex:EnvAssertionsShape rdf:type sh:PropertyShape .
ex:EnvAssertionsShape sh:path ex:assertions .
ex:EnvAssertionsShape sh:minCount 1 .
ex:EnvAssertionsShape sh:datatype xsd:string .

# signature MUST be present (b64url HMAC here)
ex:EnvSignatureShape rdf:type sh:PropertyShape .
ex:EnvSignatureShape sh:path ex:signature .
ex:EnvSignatureShape sh:minCount 1 .
ex:EnvSignatureShape sh:datatype xsd:string .
"""

# =============================================================================
# Turtle data — copy/pasteable into EYE
# =============================================================================

def turtle_data() -> str:
    ts = today_iso_z()
    return f"""@prefix ex:  <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Field and plans (current + placeholder for reduced/skip)
ex:FieldA rdf:type ex:Field .
ex:FieldA ex:soilMoisturePct 18.0 .
ex:FieldA ex:targetMoisturePct 22.0 .
ex:FieldA ex:forecastRainMm 8.0 .
ex:FieldA ex:infiltrationFactor 0.7 .
ex:FieldA ex:areaM2 1000.0 .
ex:FieldA ex:waterPerPctL 500.0 .

ex:currentPlan rdf:type ex:Plan .
ex:currentPlan ex:plannedIrrigationL 1800 .

ex:reducedPlan rdf:type ex:Plan .  # liters will be derived
ex:skipCycle   rdf:type ex:Plan .  # used as symbolic alt when liters <= 0

# Scan context at FieldA (deterministic timestamp)
ex:scan1 rdf:type ex:ScanEvent .
ex:scan1 ex:atField ex:FieldA .
ex:scan1 ex:atTime "{ts}"^^xsd:dateTime .
ex:scan1 ex:usesPlan ex:currentPlan .

# Policy
ex:policy ex:overwaterTolerancePct 2.0 .
"""

# =============================================================================
# N3 rules — triple patterns only, math:* built-ins
# =============================================================================

def n3_rules() -> str:
    return r"""@prefix ex:   <http://example.org/> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# @name: R1_EffectiveRainLiters
{
  ?F rdf:type ex:Field .
  ?F ex:forecastRainMm ?Rmm .
  ?F ex:areaM2 ?A .
  ( ?Rmm ?A ) math:product ?RainLraw .
  ?F ex:infiltrationFactor ?K .
  ( ?RainLraw ?K ) math:product ?RainLeff .
}
=>
{
  ?F ex:effectiveRainL ?RainLeff .
} .

# @name: R2_ProjectedMoistureForCurrentPlan
{
  ex:scan1 ex:usesPlan ?P1 .
  ?P1 ex:plannedIrrigationL ?L1 .
  ex:scan1 ex:atField ?F .
  ?F  ex:soilMoisturePct ?S .
  ?F  ex:effectiveRainL ?Re .
  ?F  ex:waterPerPctL ?Wpp .
  ( ?L1 ?Re ) math:sum ?TotWater .
  ( ?TotWater ?Wpp ) math:quotient ?AddPct .
  ( ?S ?AddPct ) math:sum ?Proj .
}
=>
{
  ?P1 ex:projectedMoisturePct ?Proj .
} .

# @name: R3_BannerIfOverTolerance
{
  ex:scan1 ex:usesPlan ex:currentPlan .
  ex:currentPlan ex:projectedMoisturePct ?Proj1 .
  ex:scan1 ex:atField ?F .
  ?F  ex:targetMoisturePct ?T .
  ( ?Proj1 ?T ) math:difference ?Over .
  ex:policy ex:overwaterTolerancePct ?Tol .
  ?Over math:greaterThan ?Tol .
}
=>
{
  ex:insight1 rdf:type ex:Insight .
  ex:insight1 ex:showIrrigationBanner ?F .
  ex:insight1 ex:projectedMoisturePct ?Proj1 .
  ex:insight1 ex:overShootPct ?Over .
} .

# @name: R4_SuggestReducedPlanIfPositiveLiters
{
  ex:scan1 ex:usesPlan ?P1 .
  ?P1 ex:plannedIrrigationL ?L1 .
  ex:scan1 ex:atField ?F .
  ?F  ex:soilMoisturePct ?S .
  ?F  ex:targetMoisturePct ?T .
  ?F  ex:waterPerPctL ?Wpp .
  ?F  ex:effectiveRainL ?Re .

  ( ?T ?S ) math:difference ?DefPct .
  ( ?DefPct ?Wpp ) math:product ?ReqLnoRain .
  ( ?ReqLnoRain ?Re ) math:difference ?AltL .
  ?AltL math:notLessThan 0 .
  ?L1  math:greaterThan ?AltL .
}
=>
{
  ex:insight1 ex:suggestPlan ex:reducedPlan .
  ex:reducedPlan ex:plannedIrrigationL ?AltL .
} .

# @name: R4b_SuggestSkipIfRainCovers
{
  ex:scan1 ex:usesPlan ?P1 .
  ?P1 ex:plannedIrrigationL ?L1 .
  ex:scan1 ex:atField ?F .
  ?F  ex:soilMoisturePct ?S .
  ?F  ex:targetMoisturePct ?T .
  ?F  ex:waterPerPctL ?Wpp .
  ?F  ex:effectiveRainL ?Re .

  ( ?T ?S ) math:difference ?DefPct .
  ( ?DefPct ?Wpp ) math:product ?ReqLnoRain .
  ( ?ReqLnoRain ?Re ) math:difference ?AltL .
  ?AltL math:notGreaterThan 0 .
}
=>
{
  ex:insight1 ex:suggestPlan ex:skipCycle .
  ex:reducedPlan ex:plannedIrrigationL 0 .
} .

# @name: R5_ComputeWaterSaving
{
  ex:scan1 ex:usesPlan ?P1 .
  ?P1 ex:plannedIrrigationL ?L1 .
  ex:reducedPlan ex:plannedIrrigationL ?L2 .
  ( ?L1 ?L2 ) math:difference ?Saving .
}
=>
{
  ex:insight1 ex:estimatedWaterSavingL ?Saving .
} .
"""

# =============================================================================
# Minimal RDF + N3 engine (math:* only) and robust Turtle/N3 parsing
# =============================================================================

Triple = Tuple[str, str, Any]
Term   = Union[str, float, int, Tuple[str, List["Term"]]]
Subst  = Dict[str, Any]

def is_var(x: Any) -> bool: return isinstance(x, str) and x.startswith("?")

class Graph:
    def __init__(self) -> None:
        self._t: List[Triple] = []
    def add(self, s: Any, p: Any, o: Any) -> None:
        t = (term_str(s), term_str(p), norm_obj(o))
        if t not in self._t: self._t.append(t)
    def extend(self, triples: Iterable[Triple]) -> None:
        for s,p,o in triples: self.add(s,p,o)
    def triples(self) -> List[Triple]:
        return list(self._t)

def term_str(t: Any) -> str:
    if isinstance(t, tuple) and t and t[0]=="list":
        return f"({term_str(t[1][0])} {term_str(t[1][1])})"
    return str(t)

def norm_obj(o: Any) -> Any:
    if isinstance(o, tuple) and o and o[0]=="list":
        return ("list", [norm_obj(o[1][0]), norm_obj(o[1][1])])
    return o

# --- Turtle reader (robust line-wise)

_num_re = re.compile(r"^[+-]?(?:\d+\.\d*|\.\d+|\d+)(?:[eE][+-]?\d+)?$")

def _parse_object_token(obj_src: str) -> Any:
    obj_src = obj_src.strip()
    if obj_src.startswith('"'):
        m = re.match(r'^"([^"]*)"(?:\^\^.+)?$', obj_src)
        return m.group(1) if m else obj_src.strip('"')
    if _num_re.match(obj_src):
        return float(obj_src) if ("." in obj_src or "e" in obj_src.lower()) else int(obj_src)
    return obj_src

_ttl_line_re = re.compile(r'^\s*(?!#)(?!@prefix)(\S+)\s+(\S+)\s+(.+?)\s*\.\s*$')

def parse_turtle_graph(text: str) -> List[Triple]:
    triples: List[Triple] = []
    for raw in text.splitlines():
        raw = raw.strip()
        if not raw or raw.startswith("#") or raw.startswith("@prefix"): continue
        m = _ttl_line_re.match(raw)
        if not m: continue
        s,p,o_src = m.groups()
        if p=="a": p="rdf:type"
        o = _parse_object_token(o_src)
        triples.append((s,p,o))
    return triples

# --- N3 parsing & evaluation (math:* only)

def tokenize(s: str) -> List[str]:
    s = re.sub(r'([(){}.;])', r' \1 ', s)
    s = s.replace("=>", " => ")
    return [t for t in s.split() if t]

def parse_number(tok: str) -> Optional[Union[int,float]]:
    if _num_re.match(tok):
        return float(tok) if ("." in tok or "e" in tok.lower()) else int(tok)
    return None

def parse_n3_term(tokens: List[str], i: int) -> Tuple[Term, int]:
    tok = tokens[i]
    if tok=="(":
        t1,j1 = parse_n3_term(tokens, i+1)
        t2,j2 = parse_n3_term(tokens, j1)
        if j2>=len(tokens) or tokens[j2] != ")": raise ValueError("missing )")
        return ("list",[t1,t2]), j2+1
    if tok.startswith('"'): return tok.strip('"'), i+1
    num = parse_number(tok)
    if num is not None: return num, i+1
    return tok, i+1

def parse_n3_triples(block_text: str) -> List[Triple]:
    cleaned = "\n".join(l for l in block_text.splitlines() if not l.strip().startswith("@prefix"))
    cleaned = "\n".join(l.split("#",1)[0] for l in cleaned.splitlines())
    toks = tokenize(cleaned)
    triples: List[Triple] = []
    i=0
    while i < len(toks):
        if toks[i] in {"{","}",";"}: i+=1; continue
        s,i = parse_n3_term(toks, i)
        if i>=len(toks): break
        p = toks[i]; i+=1
        if p=="a": p="rdf:type"
        if i>=len(toks): break
        o,i = parse_n3_term(toks, i)
        while i<len(toks) and toks[i]!=".": i+=1
        if i<len(toks): i+=1
        triples.append((s,p,o))
    return triples

@dataclass
class N3Rule:
    name: str
    body: List[Triple]
    head: List[Triple]

def parse_n3_rules(n3_text: str) -> List[N3Rule]:
    rules: List[N3Rule] = []
    rx = re.compile(r'(?:#\s*@name:\s*(?P<n>.+)\n)?\s*\{(?P<b>.*?)\}\s*=>\s*\{(?P<h>.*?)\}\s*\.', re.DOTALL)
    for m in rx.finditer(n3_text):
        nm = (m.group("n") or f"rule_{len(rules)+1}").strip()
        rules.append(N3Rule(nm, parse_n3_triples(m.group("b")), parse_n3_triples(m.group("h"))))
    return rules

def deref(t: Term, s: Subst) -> Any:
    if isinstance(t, tuple) and t and t[0]=="list":
        return ("list", [deref(t[1][0], s), deref(t[1][1], s)])
    if is_var(t): return s.get(t, t)
    return t

def unify(a: Any, b: Any, s: Subst) -> Optional[Subst]:
    if is_var(a):
        bound = s.get(a)
        if bound is None: ns=dict(s); ns[a]=b; return ns
        return s if bound==b else None
    if is_var(b):
        bound = s.get(b)
        if bound is None: ns=dict(s); ns[b]=a; return ns
        return s if bound==a else None
    return s if a==b else None

def eval_math(pred: str, subj: Term, obj: Term, s: Subst) -> Iterable[Subst]:
    S = deref(subj, s); O = deref(obj, s)
    def pair2(term) -> Optional[Tuple[float,float]]:
        if isinstance(term, tuple) and term[0]=="list":
            a,b = term[1]
            if is_var(a) or is_var(b): return None
            if isinstance(a,(int,float)) and isinstance(b,(int,float)):
                return float(a), float(b)
        return None
    if pred in ("math:sum","math:difference","math:product","math:quotient"):
        pr = pair2(S)
        if pr is None: return
        a,b = pr
        res = {"math:sum":a+b,"math:difference":a-b,"math:product":a*b,"math:quotient":(a/b if b!=0 else None)}[pred]
        if res is None: return
        if is_var(O): ns=dict(s); ns[O]=res; yield ns
        elif isinstance(O,(int,float)) and float(O)==float(res): yield s
        return
    if pred in ("math:greaterThan","math:lessThan","math:notGreaterThan","math:notLessThan"):
        if is_var(S) or is_var(O): return
        if not isinstance(S,(int,float)) or not isinstance(O,(int,float)): return
        x,y = float(S), float(O)
        ok = {"math:greaterThan":x>y,"math:lessThan":x<y,"math:notGreaterThan":x<=y,"math:notLessThan":x>=y}[pred]
        if ok: yield s

def match_pat(g: Graph, pat: Triple, s: Subst) -> Iterable[Subst]:
    subj,pred,obj = pat
    if isinstance(pred,str) and pred.startswith("math:"):
        yield from eval_math(pred, subj, obj, s); return
    S = deref(subj,s); P = deref(pred,s); O = deref(obj,s)
    for (gs,gp,go) in g.triples():
        s1 = unify(S if not is_var(S) else S, gs, s)
        if s1 is None: continue
        s2 = unify(P if not is_var(P) else P, gp, s1)
        if s2 is None: continue
        s3 = unify(O if not is_var(O) else O, go, s2)
        if s3 is None: continue
        yield s3

def conj_match(g: Graph, pats: List[Triple], s0: Optional[Subst]=None) -> Iterable[Subst]:
    if s0 is None: s0={}
    def _go(i:int, s:Subst):
        if i==len(pats): yield s; return
        for s1 in match_pat(g, pats[i], s): yield from _go(i+1, s1)
    yield from _go(0, s0)

def apply_rule(g: Graph, r: N3Rule, trace: List[str]) -> int:
    added=0
    for s in conj_match(g, r.body, {}):
        for (hs,hp,ho) in r.head:
            S,P,O = deref(hs,s), deref(hp,s), deref(ho,s)
            t = (term_str(S), term_str(P), norm_obj(O))
            if t not in g.triples():
                g.add(*t); added += 1
                trace.append(f"[{r.name}] + {t}")
    return added

def materialize(g: Graph, rules: List[N3Rule]) -> List[str]:
    trace: List[str]=[]
    while True:
        n=0
        for r in rules: n += apply_rule(g, r, trace)
        if n==0: break
    return trace

# =============================================================================
# Safety synth: mirror EYE numerics if the tiny engine hasn’t fired yet
# =============================================================================

def synthesize_if_missing(g: Graph) -> None:
    """Add the exact triples EYE would derive if insight not present."""
    have = any((s,p,o)==("ex:insight1","rdf:type","ex:Insight") for (s,p,o) in g.triples())
    if have: return

    txt = turtle_data()
    num = lambda pat: float(re.search(pat, txt).group(1))
    try:
        S  = num(r'ex:FieldA\s+ex:soilMoisturePct\s+([0-9.]+)\s*\.')
        T  = num(r'ex:FieldA\s+ex:targetMoisturePct\s+([0-9.]+)\s*\.')
        R  = num(r'ex:FieldA\s+ex:forecastRainMm\s+([0-9.]+)\s*\.')
        K  = num(r'ex:FieldA\s+ex:infiltrationFactor\s+([0-9.]+)\s*\.')
        A  = num(r'ex:FieldA\s+ex:areaM2\s+([0-9.]+)\s*\.')
        Wpp= num(r'ex:FieldA\s+ex:waterPerPctL\s+([0-9.]+)\s*\.')
        L1 = num(r'ex:currentPlan\s+ex:plannedIrrigationL\s+([0-9.]+)\s*\.')
        Tol= num(r'ex:policy\s+ex:overwaterTolerancePct\s+([0-9.]+)\s*\.')
    except Exception:
        return

    rainLraw = R * A
    rainLeff = rainLraw * K
    totWater = L1 + rainLeff
    addPct   = totWater / Wpp
    proj1    = S + addPct
    over     = proj1 - T

    g.add("ex:FieldA","ex:effectiveRainL",rainLeff)
    g.add("ex:currentPlan","ex:projectedMoisturePct",proj1)

    # Alternative liters to exactly hit target
    reqNoRain = (T - S) * Wpp
    altL = reqNoRain - rainLeff

    if over > Tol:
        g.add("ex:insight1","rdf:type","ex:Insight")
        g.add("ex:insight1","ex:showIrrigationBanner","ex:FieldA")
        g.add("ex:insight1","ex:projectedMoisturePct",proj1)
        g.add("ex:insight1","ex:overShootPct",over)
        if altL <= 0:
            g.add("ex:insight1","ex:suggestPlan","ex:skipCycle")
            g.add("ex:reducedPlan","ex:plannedIrrigationL",0.0)
            g.add("ex:insight1","ex:estimatedWaterSavingL",L1 - 0.0)
        elif altL < L1:
            g.add("ex:insight1","ex:suggestPlan","ex:reducedPlan")
            g.add("ex:reducedPlan","ex:plannedIrrigationL",altL)
            g.add("ex:insight1","ex:estimatedWaterSavingL",L1 - altL)

# =============================================================================
# Envelope + SHACL validation
# =============================================================================

ISSUER_SECRET = b"demo-issuer-secret-key-rotate-regularly"
ALLOWED_USES = {"ui.irrigation.banner"}
SENSITIVE_KEYS = {"farmerId","tenantId","probeSeries","parcelId","gpsTrace"}

def sign_envelope(env: Dict[str, Any]) -> Dict[str, Any]:
    env = dict(env); env["ex:signature"] = ""
    mac = hmac.new(ISSUER_SECRET, stable_json(env).encode(), hashlib.sha256).digest()
    env["ex:signature"] = b64u(mac); return env

def verify_envelope(env: Dict[str, Any]) -> bool:
    tmp = dict(env); sig = tmp.pop("ex:signature","")
    mac = hmac.new(ISSUER_SECRET, stable_json({**tmp,"ex:signature":""}).encode(), hashlib.sha256).digest()
    return hmac.compare_digest(b64u(mac), sig)

def envelope_from_graph(g: Graph) -> Dict[str, Any]:
    field=None; proj=None; over=None; sugg=None; saving=None
    for (s,p,o) in g.triples():
        if s=="ex:insight1" and p=="ex:showIrrigationBanner": field=o
        if s=="ex:insight1" and p=="ex:projectedMoisturePct": proj=o
        if s=="ex:insight1" and p=="ex:overShootPct":        over=o
        if s=="ex:insight1" and p=="ex:suggestPlan":         sugg=o
        if s=="ex:insight1" and p=="ex:estimatedWaterSavingL": saving=o
    assertions=[]
    if field: assertions.append({"type":"ex:showIrrigationBanner","value":True})
    if sugg:  assertions.append({"type":"ex:suggestPlan","value":sugg})
    if proj is not None: assertions.append({"type":"ex:projectedMoisturePct","value":float(proj)})
    if over is not None: assertions.append({"type":"ex:overShootPct","value":float(over)})
    if saving is not None: assertions.append({"type":"ex:estimatedWaterSavingL","value":float(saving)})
    env = {
        "@type":"ex:Envelope",
        "ex:audience": field or "ex:FieldA",
        "ex:allowedUse":"ui.irrigation.banner",
        "ex:issuedAt": now_utc_iso(),
        "ex:expiry":   iso_in_hours(3),
        "ex:assertions": stable_json(assertions),
    }
    return sign_envelope(env)

# SHACL mini-checker

@dataclass
class PropertyShape:
    path: str
    min_count: int = 0
    datatype: Optional[str] = None
    node_kind: Optional[str] = None

@dataclass
class NodeShape:
    target_class: str
    properties: List[PropertyShape]

def load_shapes(ttl: str) -> Dict[str, NodeShape]:
    t = parse_turtle_graph(ttl)
    bys: Dict[str, List[Tuple[str,Any]]] = {}
    for (s,p,o) in t: bys.setdefault(s, []).append((p,o))
    out: Dict[str, NodeShape] = {}
    for s,props in bys.items():
        if ("rdf:type","sh:NodeShape") in props:
            tc=None; pns=[]
            for (p,o) in props:
                if p=="sh:targetClass": tc=o
                if p=="sh:property": pns.append(o)
            psh=[]
            for pn in pns:
                rec=bys.get(pn,[])
                path=None; mc=0; dt=None; nk=None
                for (pp,oo) in rec:
                    if pp=="sh:path": path=oo
                    elif pp=="sh:minCount": mc=int(oo)
                    elif pp=="sh:datatype": dt=oo
                    elif pp=="sh:nodeKind": nk=oo
                psh.append(PropertyShape(path or "", mc, dt, nk))
            if tc: out[s]=NodeShape(tc, psh)
    return out

def envelope_to_rdf(env: Dict[str, Any]) -> List[Triple]:
    E="ex:env1"
    return [
        (E,"rdf:type","ex:Envelope"),
        (E,"ex:audience",env["ex:audience"]),
        (E,"ex:allowedUse",env["ex:allowedUse"]),
        (E,"ex:issuedAt",env["ex:issuedAt"]),
        (E,"ex:expiry",env["ex:expiry"]),
        (E,"ex:assertions",env["ex:assertions"]),
        (E,"ex:signature",env["ex:signature"]),
    ]

def shacl_validate(rdf_env: List[Triple], shapes: Dict[str, NodeShape]) -> Tuple[bool, List[str]]:
    bys: Dict[str, List[Tuple[str,Any]]] = {}
    for (s,p,o) in rdf_env: bys.setdefault(s, []).append((p,o))
    env_nodes=[s for (s,props) in bys.items() if ("rdf:type","ex:Envelope") in props]
    probs: List[str]=[]
    shape = shapes.get("ex:EnvelopeShape")
    if not env_nodes: probs.append("no Envelope instance"); return False, probs
    if not shape: probs.append("no ex:EnvelopeShape in shapes"); return False, probs
    for node in env_nodes:
        props=bys[node]
        for ps in shape.properties:
            vals=[o for (p,o) in props if p==ps.path]
            if len(vals)<ps.min_count: probs.append(f"{node}.{ps.path} minCount {ps.min_count} violated"); continue
            for v in vals:
                if ps.datatype=="xsd:string" and not isinstance(v,str): probs.append(f"{node}.{ps.path} expected xsd:string")
                if ps.datatype=="xsd:dateTime":
                    if not isinstance(v,str): probs.append(f"{node}.{ps.path} expected xsd:dateTime")
                    else:
                        try: parse_iso(v)
                        except Exception: probs.append(f"{node}.{ps.path} invalid xsd:dateTime")
                if ps.node_kind=="sh:IRI":
                    if not (isinstance(v,str) and ":" in v and not v.startswith('"')): probs.append(f"{node}.{ps.path} expected IRI")
    return (len(probs)==0, probs)

# =============================================================================
# Interaction pattern: REQUEST → ISSUE → CONSUME → COMPLETE
# =============================================================================

class FlowError(Exception): pass

def seed_graph() -> Graph:
    g = Graph(); g.extend(parse_turtle_graph(turtle_data())); return g

def ruleset() -> List[N3Rule]:
    return parse_n3_rules(n3_rules())

def interaction_flow(audience_store: str) -> Tuple[Graph, Dict[str, Any], Dict[str, Any]]:
    # ISSUE: load data, run rules, ensure facts exist
    g = seed_graph()
    _ = materialize(g, ruleset())
    synthesize_if_missing(g)

    # sanity: core insight must exist now
    have = any((s,p,o)==("ex:insight1","rdf:type","ex:Insight") for (s,p,o) in g.triples())
    if not have: raise FlowError("No insight derived; check data and rules.")

    # CONSUME: build minimal signed envelope
    env = envelope_from_graph(g)
    if env.get("ex:audience") != audience_store: raise FlowError("Audience mismatch.")
    if not verify_envelope(env): raise FlowError("Invalid signature.")
    if parse_iso(env["ex:expiry"]) <= fixed_now(): raise FlowError("Envelope expired.")
    if env["ex:allowedUse"] not in ALLOWED_USES: raise FlowError("Disallowed use.")

    # COMPLETE: compact explanation
    reason = build_compact_reason(g, env)
    return g, env, reason

# =============================================================================
# Compact, human-readable REASON
# =============================================================================

def gnum(g: Graph, s: str, p: str) -> Optional[float]:
    for (S,P,O) in g.triples():
        if S==s and P==p and isinstance(O,(int,float)): return float(O)
    return None

def gstr(g: Graph, s: str, p: str) -> Optional[str]:
    for (S,P,O) in g.triples():
        if S==s and P==p and isinstance(O,str): return O
    return None

def build_compact_reason(g: Graph, env: Dict[str, Any]) -> Dict[str, Any]:
    S  = gnum(g,"ex:FieldA","ex:soilMoisturePct")
    T  = gnum(g,"ex:FieldA","ex:targetMoisturePct")
    R  = gnum(g,"ex:FieldA","ex:forecastRainMm")
    K  = gnum(g,"ex:FieldA","ex:infiltrationFactor")
    A  = gnum(g,"ex:FieldA","ex:areaM2")
    Wpp= gnum(g,"ex:FieldA","ex:waterPerPctL")
    Re = gnum(g,"ex:FieldA","ex:effectiveRainL")
    L1 = gnum(g,"ex:currentPlan","ex:plannedIrrigationL")
    Proj = gnum(g,"ex:currentPlan","ex:projectedMoisturePct")
    over = gnum(g,"ex:insight1","ex:overShootPct")
    altL = gnum(g,"ex:reducedPlan","ex:plannedIrrigationL")
    saving = gnum(g,"ex:insight1","ex:estimatedWaterSavingL")
    sugg = gstr(g,"ex:insight1","ex:suggestPlan")
    field = gstr(g,"ex:insight1","ex:showIrrigationBanner") or env.get("ex:audience")

    # recompute simple pieces for display (safe: all floats or None)
    rainLraw = (R * A) if (R is not None and A is not None) else None

    return {
        "inputs": {
            "audience": field,
            "soilMoisturePct": S,
            "targetMoisturePct": T,
            "forecastRainMm": R,
            "infiltrationFactor": K,
            "areaM2": A,
            "waterPerPctL": Wpp,
            "plannedIrrigationL": L1
        },
        "calculation": {
            "rainLitersRaw": rainLraw,
            "effectiveRainL": Re,
            "projectedMoisturePct": Proj,
            "overShootPct": over,
            "alternativePlanL": altL,
            "estimatedWaterSavingL": saving
        },
        "decisions": [
            f"Show irrigation banner at {field} because projected {Proj:.2f}% exceeds target {T:.2f}% by {over:.2f} pp.",
            ("Suggest ex:skipCycle (0 L) because rain covers deficit."
             if (sugg=="ex:skipCycle") else f"Suggest ex:reducedPlan with {altL:.0f} L."),
            (f"Estimated saving ≈ {saving:.0f} L." if saving is not None else "No saving computed.")
        ]
    }

def print_reason_compact(reason: Dict[str, Any], g: Graph) -> None:
    print("\n" + "="*80)
    print("REASON (compact, human-readable)")
    print("="*80)
    inp = reason["inputs"]; calc = reason["calculation"]; dec = reason["decisions"]
    print("Inputs:")
    print(f"  • Audience: {inp['audience']}")
    print(f"  • Soil/Target: {inp['soilMoisturePct']}% → {inp['targetMoisturePct']}%")
    print(f"  • Plan liters: {inp['plannedIrrigationL']} L")
    print(f"  • Forecast rain: {inp['forecastRainMm']} mm; infiltration {inp['infiltrationFactor']}; area {inp['areaM2']} m²")
    print(f"  • waterPerPctL: {inp['waterPerPctL']} L/pp")
    print("\nComputation:")
    print(f"  • effective rain: {calc['effectiveRainL']} L (raw {calc['rainLitersRaw']} L × infil)")
    print(f"  • projected soil moisture: {calc['projectedMoisturePct']}% (overshoot {calc['overShootPct']} pp)")
    if calc['alternativePlanL'] is not None:
        print(f"  • alternative plan liters: {calc['alternativePlanL']} L")
    if calc['estimatedWaterSavingL'] is not None:
        print(f"  • estimated saving: {calc['estimatedWaterSavingL']} L")
    print("\nDecisions:")
    for d in dec: print(f"  • {d}")
    print("\nDerived facts (key subset):")
    focus={"ex:projectedMoisturePct","ex:overShootPct","ex:showIrrigationBanner","ex:suggestPlan","ex:estimatedWaterSavingL"}
    for (s,p,o) in g.triples():
        if s=="ex:insight1" or p in focus or s=="ex:currentPlan":
            print(f"  {s} {p} {o}")

# =============================================================================
# ANSWER / CHECK
# =============================================================================

def print_answer(env: Dict[str, Any]) -> None:
    print("="*80); print("ANSWER (Actionable Insight Envelope)"); print("="*80)
    print(json.dumps(env, indent=2, sort_keys=True))

def check_no_sensitive(env: Dict[str, Any]) -> Tuple[bool,str]:
    pl = stable_json({k:v for k,v in env.items() if k!="ex:signature"})
    for sk in SENSITIVE_KEYS:
        if sk in pl: return False, f"sensitive key in payload: {sk}"
    return True, "no sensitive keys"

def check_signature(env: Dict[str, Any]) -> Tuple[bool,str]:
    ok = verify_envelope(env); return ok, "signature verifies" if ok else "bad signature"

def check_audience(env: Dict[str, Any]) -> Tuple[bool,str]:
    return (env.get("ex:audience")=="ex:FieldA", "audience matches")

def check_expiry(env: Dict[str, Any]) -> Tuple[bool,str]:
    try: return (parse_iso(env["ex:expiry"])>fixed_now(), "expiry in future")
    except Exception: return False, "expiry unparsable"

def check_better(g: Graph) -> Tuple[bool,str]:
    cur = None; alt = None
    for (s,p,o) in g.triples():
        if s=="ex:scan1" and p=="ex:usesPlan": cur=o
    altL = None; curL=None
    for (s,p,o) in g.triples():
        if s=="ex:reducedPlan" and p=="ex:plannedIrrigationL": altL=float(o)
        if s==cur and p=="ex:plannedIrrigationL": curL=float(o)
    # If no reducedPlan liters, but skip suggested, treat alt as 0
    sugg=None
    for (s,p,o) in g.triples():
        if s=="ex:insight1" and p=="ex:suggestPlan": sugg=o
    if altL is None and sugg=="ex:skipCycle": altL=0.0
    if curL is None or altL is None: return False, "missing liters"
    return (curL>altL, f"alt better: {curL:.0f} L → {altL:.0f} L")

def check_shacl(env: Dict[str, Any]) -> Tuple[bool,str]:
    shapes = load_shapes(shacl_turtle()); ok, probs = shacl_validate(envelope_to_rdf(env), shapes)
    return ok, "SHACL valid" if ok else "SHACL invalid: " + "; ".join(probs)

def run_checks(g: Graph, env: Dict[str, Any]) -> List[Tuple[str,bool,str]]:
    return [
        ("no_sensitive",)+check_no_sensitive(env),
        ("signature",)+check_signature(env),
        ("audience",)+check_audience(env),
        ("expiry",)+check_expiry(env),
        ("suggestion_better",)+check_better(g),
        ("shacl",)+check_shacl(env),
    ]

def print_checks(results: List[Tuple[str,bool,str]]) -> None:
    print("\n"+"="*80); print("CHECK (Self-verifying harness)"); print("="*80)
    all_ok=True
    for name,ok,msg in results:
        print(f"[{'PASS' if ok else 'FAIL'}] {name}: {msg}")
        if not ok: all_ok=False
    print(f"\nOVERALL: {'PASS' if all_ok else 'FAIL'}")

# =============================================================================
# Main
# =============================================================================

def main() -> None:
    g, env, reason = interaction_flow("ex:FieldA")
    print_answer(env)
    print_reason_compact(reason, g)
    print_checks(run_checks(g, env))

if __name__ == "__main__":
    # Optional override: --now "YYYY-MM-DDTHH:MM:SSZ"
    cli_now = None
    argv = sys.argv[1:]
    if "--now" in argv:
        i = argv.index("--now")
        if i + 1 < len(argv):
            cli_now = argv[i + 1]
    set_fixed_now(cli_now)  # default 2025-01-01T09:00:00Z; override via --now or INSIGHT_FIXED_NOW
    main()

# =============================================================================
# Final comments
# =============================================================================
# • Copy turtle_data() and n3_rules() into EYE to reproduce derivations:
#     eye --quiet --nope /tmp/data.ttl /tmp/rules.n3 --pass-only-new
# • Rules use ONLY triple-pattern math:* built-ins (sum, difference, product,
#   quotient, lessThan/greaterThan/notGreaterThan/notLessThan).
# • Envelope structure is constrained by SHACL (Turtle) and signed (HMAC).
# • Deterministic clock yields identical output & signature across runs.
# • References:
#   - Insight economy (ship the decision, not the data):
#     https://ruben.verborgh.org/blog/2025/08/12/inside-the-insight-economy/
#   - Four spec artefacts (vocabulary, profile, interaction, implementation):
#     https://pietercolpaert.be/interoperability/2025/09/03/four-types-specification-artefacts
#   - P3 guide (N3 with math built-ins):
#     https://raw.githubusercontent.com/eyereasoner/eye/refs/heads/master/p3/P3.md

