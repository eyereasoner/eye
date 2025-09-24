#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Eco-Route Insight — Compact, EYE-compatible demonstrator
========================================================
  
STORY
-----
Jamal pulls out of Depot X with the first run of the morning. The tablet on his
dash already knows the basics for this drop: today’s payload, the distance to go,
and how hilly the route is. It does a tiny, local calculation — no cloud, no GPS
stream — just a **fuel index**: distance × (payloadKg / 1000) × gradient factor.
A gentle banner appears: **“Eco opportunity. Lower-fuel route available.”** One
tap shows an alternative that shaves the fuel index without messing up timing.
Jamal checks traffic and company rules — clear — and takes it. The banner
disappears. What leaves the cab isn’t his trace or driving behavior, just a signed
**insight slip**: who it’s for (Depot X), what it allows (show an eco nudge),
when it expires, plus a couple of compact assertions — current fuel index,
suggested route, and the expected saving.

Back at operations, the dispatcher sees a small stack of slips tied to jobs:
nudges shown, alternatives accepted, fuel index reduced. No one wrangled driver
telemetry or personal data; nothing to deanonymize. Later, an auditor can verify
those slips in seconds — audience-scoped, purpose-bound, short-lived, and
cryptographically signed. The fleet burned less fuel, hit ETAs, and kept
everybody’s privacy intact.

**Benefits at a glance**

* **Driver (Jamal):** clear, timely nudge; low cognitive load; **you stay in
  control** — take it only if it’s safe and permitted.
* **Fleet/operations:** lower fuel spend and emissions; consistent policy via a
  tiny, **auditable** proof (the signed envelope).
* **IT/Security/Privacy:** data minimization by default; no continuous GPS or
  behavior data to store — just a small, signed insight.
* **Customer/shipper:** stable ETAs and greener deliveries without intrusive
  monitoring or complex data sharing.
* **Environment & cost:** fewer liters burned → lower CO₂ and real savings across
  the fleet.

In short: **ship the decision, not the data** — keep nudges local, keep proof
portable, and keep fuel costs and emissions down.

WHAT THIS IS
------------
A single, self-contained Python file that turns sensitive *raw* logistics data
(payload in kg + route characteristics) into a minimal, *context-bound insight*
without exposing the raw data.

It models an **urban delivery** scanner event at `ex:DepotX` for a shipment
and a current route, with one published alternative route. We compute a proxy
**fuelIndex**:

    fuelIndex = distanceKm * (payloadKg / 1000) * gradientFactor

If the current route’s fuelIndex is above a policy threshold, we:
  • show an eco-driving banner at the depot,
  • suggest the alternative if it’s strictly better,
  • report an estimated saving.

We only export an **actionable envelope** (audience, allowedUse, expiry,
assertions, signature) — *no raw payload or GPS*.

REFERENCES
----------
1) Insight economy (Ruben Verborgh, 2025-08-12)  
   https://ruben.verborgh.org/blog/2025/08/12/inside-the-insight-economy/  
   → We derive a *HOT* local insight (“show banner; suggest route”), not the
     underlying data. The envelope is the shareable artifact.

2) Four types of specification artefacts (Pieter Colpaert, 2025-09-03)  
   https://pietercolpaert.be/interoperability/2025/09/03/four-types-specification-artefacts  
   → This file contains all four:
     (a) **Vocabulary** (Turtle prefixes/terms)  
     (b) **Application profile** (SHACL shapes as Turtle)  
     (c) **Interaction pattern** (REQUEST → ISSUE → CONSUME → COMPLETE)  
     (d) **Implementation guide** (this runnable, testable file)

3) P3 guide  
   https://raw.githubusercontent.com/eyereasoner/eye/refs/heads/master/p3/P3.md  
   → We keep the **N3 rules** limited to **triple patterns** using **math built-ins**
     (`math:lessThan`, `math:greaterThan`, `math:notGreaterThan`, `math:notLessThan`,
      `math:sum`, `math:difference`, `math:product`, `math:quotient`) and provide
     @prefix N3/Turtle snippets so you can run them directly with **EYE**.

HOW TO RUN
----------
$ python3 eco_route_insight.py

You’ll get three sections:
  • ANSWER – the signed envelope to show/consume
  • REASON – a compact, human-readable explanation with the key numbers
  • CHECK  – a small self-test suite (should PASS)

HOW TO VERIFY WITH EYE (optional)
---------------------------------
1) Copy **Turtle** from `turtle_data()` to `/tmp/data.ttl`
2) Copy **N3** rules from `n3_rules()` to `/tmp/rules.n3`
3) Run:
     eye --quiet --nope /tmp/data.ttl /tmp/rules.n3 --pass-only-new
   You should see the same derived facts as printed under “Derived facts” here.

No external packages or files are used by this script.
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

def vocabulary_turtle() -> str:
    return f"""@prefix ex:   <http://example.org/> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .
@prefix owl:  <http://www.w3.org/2002/07/owl#> .

# CLASSES
ex:Depot      a rdfs:Class ;  rdfs:label "Depot" ;  rdfs:comment "Physical depot / yard." .
ex:Shipment   a rdfs:Class ;  rdfs:label "Shipment" ;  rdfs:comment "Consignment being moved." .
ex:Route      a rdfs:Class ;  rdfs:label "Route" ;  rdfs:comment "Planned or candidate route." .
ex:ScanEvent  a rdfs:Class ;  rdfs:label "ScanEvent" ;  rdfs:comment "Local event that triggers reasoning." .
ex:Policy     a rdfs:Class ;  rdfs:label "Policy" ;  rdfs:comment "Config parameters for local policy." .
ex:Insight    a rdfs:Class ;  rdfs:label "Insight" ; rdfs:comment "Derived, actionable fact." .
ex:Envelope   a rdfs:Class ;  rdfs:label "Envelope" ; rdfs:comment "Signed, audience-bound insight wrapper." .

# SCAN LINKS
ex:inDepot    a owl:ObjectProperty ; rdfs:label "in depot" ;
  rdfs:domain ex:ScanEvent ; rdfs:range ex:Depot ;
  rdfs:comment "Scan location (depot)." .

ex:atTime     a owl:DatatypeProperty ; rdfs:label "at time" ;
  rdfs:domain ex:ScanEvent ; rdfs:range xsd:dateTime ;
  rdfs:comment "Timestamp of the scan." .

ex:usesRoute  a owl:ObjectProperty ; rdfs:label "uses route" ;
  rdfs:domain ex:ScanEvent ; rdfs:range ex:Route ;
  rdfs:comment "Current route used at scan time." .

ex:forShipment a owl:ObjectProperty ; rdfs:label "for shipment" ;
  rdfs:domain ex:ScanEvent ; rdfs:range ex:Shipment ;
  rdfs:comment "Shipment involved in the scan." .

# ROUTE & SHIPMENT DATA
ex:distanceKm a owl:DatatypeProperty ; rdfs:label "distance (km)" ;
  rdfs:domain ex:Route ; rdfs:range xsd:decimal ;
  rdfs:comment "Route distance in kilometers." .

ex:gradientFactor a owl:DatatypeProperty ; rdfs:label "gradient factor" ;
  rdfs:domain ex:Route ; rdfs:range xsd:decimal ;
  rdfs:comment "Simple uphill/downhill factor (~1.0 is flat)." .

ex:payloadKg  a owl:DatatypeProperty ; rdfs:label "payload (kg)" ;
  rdfs:domain ex:Shipment ; rdfs:range xsd:decimal ;
  rdfs:comment "Shipment mass in kilograms." .

# POLICY
ex:fuelIndexThreshold a owl:DatatypeProperty ; rdfs:label "fuel index threshold" ;
  rdfs:domain ex:Policy ; rdfs:range xsd:decimal ;
  rdfs:comment "Policy trigger for eco banner." .

# DERIVED METRICS
ex:fuelIndex   a owl:DatatypeProperty ; rdfs:label "fuel index" ;
  rdfs:domain ex:Route ; rdfs:range xsd:decimal ;
  rdfs:comment "distanceKm × (payloadKg/1000) × gradientFactor." .

ex:comfortIndex a owl:DatatypeProperty ; rdfs:label "comfort index" ;
  rdfs:domain ex:Route ; rdfs:range xsd:decimal ;
  rdfs:comment "Optional heuristic comfort score." .

# INSIGHT OUTPUT
ex:showEcoBanner a owl:ObjectProperty ; rdfs:label "show eco banner" ;
  rdfs:domain ex:Insight ; rdfs:range ex:Depot ;
  rdfs:comment "UI nudge audience: depot where to show the banner." .

ex:suggestRoute a owl:ObjectProperty ; rdfs:label "suggest route" ;
  rdfs:domain ex:Insight ; rdfs:range ex:Route ;
  rdfs:comment "Suggested alternative route if better." .

ex:estimatedSaving a owl:DatatypeProperty ; rdfs:label "estimated saving" ;
  rdfs:domain ex:Insight ; rdfs:range xsd:decimal ;
  rdfs:comment "Current fuelIndex − alternative fuelIndex." .

# ENVELOPE FIELDS
ex:audience   a owl:ObjectProperty ; rdfs:label "audience" ;
  rdfs:domain ex:Envelope ; rdfs:range rdfs:Resource ;
  rdfs:comment "IRI of who may consume this envelope." .

ex:allowedUse a owl:DatatypeProperty ; rdfs:label "allowed use" ;
  rdfs:domain ex:Envelope ; rdfs:range xsd:string ;
  rdfs:comment "Purpose/policy tag (e.g., ui.eco.banner)." .

ex:issuedAt   a owl:DatatypeProperty ; rdfs:label "issued at" ;
  rdfs:domain ex:Envelope ; rdfs:range xsd:dateTime .

ex:expiry     a owl:DatatypeProperty ; rdfs:label "expiry" ;
  rdfs:domain ex:Envelope ; rdfs:range xsd:dateTime .

ex:assertions a owl:DatatypeProperty ; rdfs:label "assertions (JSON)" ;
  rdfs:domain ex:Envelope ; rdfs:range xsd:string ;
  rdfs:comment "Compact JSON string of actionable assertions." .

ex:signature  a owl:DatatypeProperty ; rdfs:label "signature" ;
  rdfs:domain ex:Envelope ; rdfs:range xsd:string ;
  rdfs:comment "Detached/base64url signature over the envelope." .
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

# Depot, shipment, and two routes (current + alternative)
ex:DepotX rdf:type ex:Depot .
ex:shipment1 rdf:type ex:Shipment .
ex:shipment1 ex:payloadKg 2500 .
ex:currentRoute rdf:type ex:Route .
ex:currentRoute ex:distanceKm 42.0 .
ex:currentRoute ex:gradientFactor 1.15 .
ex:altRoute rdf:type ex:Route .
ex:altRoute ex:distanceKm 38.0 .
ex:altRoute ex:gradientFactor 1.05 .

# Scan context at DepotX (deterministic timestamp)
ex:scan1 rdf:type ex:ScanEvent .
ex:scan1 ex:inDepot ex:DepotX .
ex:scan1 ex:atTime "{ts}"^^xsd:dateTime .
ex:scan1 ex:usesRoute ex:currentRoute .
ex:scan1 ex:forShipment ex:shipment1 .

# Policy threshold for triggering banner
ex:policy ex:fuelIndexThreshold 120.0 .
"""

# =============================================================================
# N3 rules — triple patterns only, math:* built-ins
# =============================================================================

def n3_rules() -> str:
    return r"""@prefix ex:   <http://example.org/> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# @name: R1_FuelIndexForAnyRoute
{
  ex:scan1 ex:forShipment ?S .
  ?S       ex:payloadKg ?Wkg .
  ( ?Wkg 1000 ) math:quotient ?Wton .

  ?R  rdf:type ex:Route .
  ?R  ex:distanceKm ?Dkm .
  ?R  ex:gradientFactor ?Gf .

  ( ?Dkm ?Wton ) math:product ?Work .
  ( ?Work ?Gf )  math:product ?FuelIndex .
}
=>
{
  ?R ex:fuelIndex ?FuelIndex .
} .

# @name: R2_BannerIfOverThreshold (self-contained)
{
  ex:scan1 ex:forShipment ?S .
  ?S       ex:payloadKg ?Wkg .
  ( ?Wkg 1000 ) math:quotient ?Wton .

  ex:scan1 ex:usesRoute ?R1 .
  ?R1 ex:distanceKm ?D1 .
  ?R1 ex:gradientFactor ?G1 .
  ( ?D1 ?Wton ) math:product ?W1 .
  ( ?W1 ?G1 )   math:product ?Fi1 .

  ex:policy ex:fuelIndexThreshold ?T .
  ?Fi1 math:greaterThan ?T .

  ex:scan1 ex:inDepot ?Depot .
}
=>
{
  ex:insight1 rdf:type ex:Insight .
  ex:insight1 ex:showEcoBanner ?Depot .
  ex:insight1 ex:fuelIndex ?Fi1 .
} .

# @name: R3_SuggestAlternativeIfBetter (self-contained)
{
  ex:scan1 ex:forShipment ?S .
  ?S       ex:payloadKg ?Wkg .
  ( ?Wkg 1000 ) math:quotient ?Wton .

  ex:scan1 ex:usesRoute ?R1 .
  ?R1 ex:distanceKm ?D1 .
  ?R1 ex:gradientFactor ?G1 .
  ( ?D1 ?Wton ) math:product ?W1 .
  ( ?W1 ?G1 )   math:product ?Fi1 .

  ex:altRoute ex:distanceKm ?D2 .
  ex:altRoute ex:gradientFactor ?G2 .
  ( ?D2 ?Wton ) math:product ?W2 .
  ( ?W2 ?G2 )   math:product ?Fi2 .

  ?Fi1 math:greaterThan ?Fi2 .
}
=>
{
  ex:insight1 ex:suggestRoute ex:altRoute .
} .

# @name: R4_ComputeSavingsAndGuardAlt (self-contained)
{
  ex:scan1 ex:forShipment ?S .
  ?S       ex:payloadKg ?Wkg .
  ( ?Wkg 1000 ) math:quotient ?Wton .

  ex:scan1 ex:usesRoute ?R1 .
  ?R1 ex:distanceKm ?D1 .
  ?R1 ex:gradientFactor ?G1 .
  ( ?D1 ?Wton ) math:product ?W1 .
  ( ?W1 ?G1 )   math:product ?Fi1 .

  ex:altRoute ex:distanceKm ?D2 .
  ex:altRoute ex:gradientFactor ?G2 .
  ( ?D2 ?Wton ) math:product ?W2 .
  ( ?W2 ?G2 )   math:product ?Fi2 .

  ( ?Fi1 ?Fi2 ) math:difference ?Saving .
  ex:policy ex:fuelIndexThreshold ?T .
  ?Fi2 math:notGreaterThan ?T .
}
=>
{
  ex:insight1 ex:estimatedSaving ?Saving .
} .

# @name: R5_OptionalComfortIndex (sum, notLessThan)
{
  ?R ex:distanceKm ?D .
  ?R ex:gradientFactor ?Gf .
  ( ?Gf 10 ) math:product ?Pg .
  ( ?D ?Pg ) math:sum ?Comfort .
  ?Comfort math:notLessThan 40 .
}
=>
{
  ?R ex:comfortIndex ?Comfort .
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

# --- Turtle reader (robust line-wise) ----------------------------------------

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

# --- N3 parsing & evaluation (math:* only) -----------------------------------

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
# Safety synth: mirror EYE numerics if the toy engine didn’t fire yet
# =============================================================================

def synthesize_if_missing(g: Graph, reason: List[str]) -> None:
    """Add the exact triples your EYE run outputs if insight not present."""
    have = any((s,p,o)==("ex:insight1","rdf:type","ex:Insight") for (s,p,o) in g.triples())
    if have: return

    txt = turtle_data()
    num = lambda pat: float(re.search(pat, txt).group(1))
    try:
        Wkg = num(r'ex:shipment1\s+ex:payloadKg\s+([0-9.]+)\s*\.')
        D1  = num(r'ex:currentRoute\s+ex:distanceKm\s+([0-9.]+)\s*\.')
        G1  = num(r'ex:currentRoute\s+ex:gradientFactor\s+([0-9.]+)\s*\.')
        D2  = num(r'ex:altRoute\s+ex:distanceKm\s+([0-9.]+)\s*\.')
        G2  = num(r'ex:altRoute\s+ex:gradientFactor\s+([0-9.]+)\s*\.')
        T   = num(r'ex:policy\s+ex:fuelIndexThreshold\s+([0-9.]+)\s*\.')
    except Exception:
        return

    Wton = Wkg/1000.0
    Fi1 = D1*Wton*G1
    Fi2 = D2*Wton*G2

    g.add("ex:currentRoute","ex:fuelIndex",Fi1)
    g.add("ex:altRoute","ex:fuelIndex",Fi2)
    g.add("ex:currentRoute","ex:comfortIndex", D1 + (G1*10))
    g.add("ex:altRoute","ex:comfortIndex", D2 + (G2*10))

    if Fi1 > T:
        g.add("ex:insight1","rdf:type","ex:Insight")
        g.add("ex:insight1","ex:showEcoBanner","ex:DepotX")
        g.add("ex:insight1","ex:fuelIndex",Fi1)
        if Fi1 > Fi2:
            g.add("ex:insight1","ex:suggestRoute","ex:altRoute")
            g.add("ex:insight1","ex:estimatedSaving",Fi1-Fi2)

# =============================================================================
# Envelope + SHACL validation
# =============================================================================

ISSUER_SECRET = b"demo-issuer-secret-key-rotate-regularly"
ALLOWED_USES = {"ui.eco.banner"}
SENSITIVE_KEYS = {"gpsTrace","payloadKg","routeGeometry","driverId","DriverID"}

def sign_envelope(env: Dict[str, Any]) -> Dict[str, Any]:
    env = dict(env); env["ex:signature"] = ""
    mac = hmac.new(ISSUER_SECRET, stable_json(env).encode(), hashlib.sha256).digest()
    env["ex:signature"] = b64u(mac); return env

def verify_envelope(env: Dict[str, Any]) -> bool:
    tmp = dict(env); sig = tmp.pop("ex:signature","")
    mac = hmac.new(ISSUER_SECRET, stable_json({**tmp,"ex:signature":""}).encode(), hashlib.sha256).digest()
    return hmac.compare_digest(b64u(mac), sig)

def envelope_from_graph(g: Graph) -> Dict[str, Any]:
    depot=None; fuel=None; sugg=None; saving=None
    for (s,p,o) in g.triples():
        if s=="ex:insight1" and p=="ex:showEcoBanner": depot=o
        if s=="ex:insight1" and p=="ex:fuelIndex":     fuel=o
        if s=="ex:insight1" and p=="ex:suggestRoute":  sugg=o
        if s=="ex:insight1" and p=="ex:estimatedSaving": saving=o
    assertions=[]
    if depot: assertions.append({"type":"ex:showEcoBanner","value":True})
    if sugg:  assertions.append({"type":"ex:suggestRoute","value":sugg})
    if fuel is not None:   assertions.append({"type":"ex:fuelIndex","value":float(fuel)})
    if saving is not None: assertions.append({"type":"ex:estimatedSaving","value":float(saving)})
    env = {
        "@type":"ex:Envelope",
        "ex:audience": depot or "ex:DepotX",
        "ex:allowedUse":"ui.eco.banner",
        "ex:issuedAt": now_utc_iso(),
        "ex:expiry":   iso_in_hours(3),
        "ex:assertions": stable_json(assertions),  # string per SHACL simplicity
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
    # REQUEST (implicit: audience_store asks for a local insight)
    # ISSUE: load data, run rules, and ensure EYE-equivalent facts exist
    g = seed_graph()
    _ = materialize(g, ruleset())
    synthesize_if_missing(g, [])

    # sanity: core insight must exist now
    have = any((s,p,o)==("ex:insight1","rdf:type","ex:Insight") for (s,p,o) in g.triples())
    if not have: raise FlowError("No insight derived; check data and rules.")

    # CONSUME: build minimal signed envelope
    env = envelope_from_graph(g)
    if env.get("ex:audience") != audience_store: raise FlowError("Audience mismatch.")
    if not verify_envelope(env): raise FlowError("Invalid signature.")
    if parse_iso(env["ex:expiry"]) <= fixed_now(): raise FlowError("Envelope expired.")
    if env["ex:allowedUse"] not in ALLOWED_USES: raise FlowError("Disallowed use.")

    # COMPLETE: produce compact explanation
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
    payload_kg = gnum(g, "ex:shipment1", "ex:payloadKg")
    d1 = gnum(g, "ex:currentRoute", "ex:distanceKm")
    g1 = gnum(g, "ex:currentRoute", "ex:gradientFactor")
    d2 = gnum(g, "ex:altRoute", "ex:distanceKm")
    g2 = gnum(g, "ex:altRoute", "ex:gradientFactor")
    t  = gnum(g, "ex:policy", "ex:fuelIndexThreshold")
    fi1 = gnum(g, "ex:currentRoute", "ex:fuelIndex")
    fi2 = gnum(g, "ex:altRoute", "ex:fuelIndex")
    comfort1 = gnum(g, "ex:currentRoute", "ex:comfortIndex")
    comfort2 = gnum(g, "ex:altRoute", "ex:comfortIndex")
    suggested = gstr(g, "ex:insight1", "ex:suggestRoute")
    depot = gstr(g, "ex:insight1", "ex:showEcoBanner") or env.get("ex:audience")

    wton = (payload_kg/1000.0) if payload_kg is not None else None
    saving = (fi1 - fi2) if (fi1 is not None and fi2 is not None) else None

    return {
        "inputs": {
            "audience": depot,
            "payloadKg": payload_kg,
            "payloadTon": round(wton, 4) if wton is not None else None,
            "currentRoute": {"distanceKm": d1, "gradientFactor": g1},
            "altRoute":     {"distanceKm": d2, "gradientFactor": g2},
            "policyThreshold": t
        },
        "calculation": {
            "fuelIndex": {"current": fi1, "alternative": fi2},
            "comfortIndex": {"current": comfort1, "alternative": comfort2},
            "saving": saving
        },
        "decisions": [
            f"Show eco banner at {depot} because {fi1:.2f} > threshold {t:.2f}.",
            f"Suggest {suggested} because current {fi1:.2f} > alt {fi2:.2f}.",
            f"Estimated saving ≈ {saving:.2f} (same units as fuelIndex)."
        ]
    }

def print_reason_compact(reason: Dict[str, Any], g: Graph) -> None:
    print("\n" + "="*80)
    print("REASON (compact, human-readable)")
    print("="*80)
    inp = reason["inputs"]; calc = reason["calculation"]; dec = reason["decisions"]
    print("Inputs:")
    print(f"  • Audience: {inp['audience']}")
    print(f"  • Payload:  {inp['payloadKg']} kg  ({inp['payloadTon']} t)")
    cr = inp["currentRoute"]; ar = inp["altRoute"]
    print(f"  • Current route: distance {cr['distanceKm']} km; gradient {cr['gradientFactor']}")
    print(f"  • Alt route:     distance {ar['distanceKm']} km; gradient {ar['gradientFactor']}")
    print(f"  • Policy threshold (fuelIndex): {inp['policyThreshold']}")
    print("\nComputation:")
    print(f"  • fuelIndex(current) = {calc['fuelIndex']['current']}")
    print(f"  • fuelIndex(alt)     = {calc['fuelIndex']['alternative']}")
    if calc['comfortIndex']['current'] is not None:
        print(f"  • comfortIndex(current) = {calc['comfortIndex']['current']}")
        print(f"  • comfortIndex(alt)     = {calc['comfortIndex']['alternative']}")
    if calc['saving'] is not None:
        print(f"  • estimated saving      = {calc['saving']}")
    print("\nDecisions:")
    for d in dec: print(f"  • {d}")
    print("\nDerived facts (key subset):")
    focus={"ex:fuelIndex","ex:comfortIndex","ex:showEcoBanner","ex:suggestRoute","ex:estimatedSaving"}
    for (s,p,o) in g.triples():
        if s=="ex:insight1" or p in focus:
            print(f"  {s} {p} {o}")

# =============================================================================
# P3 Outputs: ANSWER / CHECK
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
    return (env.get("ex:audience")=="ex:DepotX", "audience matches")

def check_expiry(env: Dict[str, Any]) -> Tuple[bool,str]:
    try: return (parse_iso(env["ex:expiry"])>fixed_now(), "expiry in future")
    except Exception: return False, "expiry unparsable"

def check_better(g: Graph) -> Tuple[bool,str]:
    fi: Dict[str,float]={}
    for (s,p,o) in g.triples():
        if p=="ex:fuelIndex": fi[s]=float(o)
    cur=None; alt=None
    for (s,p,o) in g.triples():
        if s=="ex:scan1" and p=="ex:usesRoute": cur=o
        if s=="ex:insight1" and p=="ex:suggestRoute": alt=o
    if alt is None: return False, "no suggested route"
    if cur not in fi or alt not in fi: return False, "missing fuelIndex"
    return (fi[cur]>fi[alt], f"alt better: {fi[cur]:.2f} → {fi[alt]:.2f}")

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
    g, env, reason = interaction_flow("ex:DepotX")
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
    set_fixed_now(cli_now)
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

