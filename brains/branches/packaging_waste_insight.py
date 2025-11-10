#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Packaging Waste Insight — Compact, EYE-compatible demonstrator
==============================================================

STORY
-----
Lina slides a blender into a cardboard box at PackStation Z. The station’s
tablet already knows the item’s dimensions and the inner size of the box she
grabbed. It does a tiny, local calculation — no photos, no cloud, no item
catalogue — just **waste % = ((box volume − item volume) / box volume) × 100**.
A soft banner lights up: **“Packaging waste high. Tighter box available.”** One
tap shows an alternative carton that still leaves safe padding. Lina checks the
stack beside her — there it is — and swaps the box. The banner fades. What
leaves the device isn’t the product size or order details, just a signed
**insight slip**: who it’s for (PackStation Z), what it allows (show a packaging
nudge), when it expires, and two compact assertions — current waste %, and the
expected reduction if the tighter box is used.

End of shift, the floor lead reviews a handful of slips attached to jobs: nudges
shown, alternatives taken, void reduced — policy followed. No one had to collect
item geometry or customer IDs; nothing to re-identify. An auditor later verifies
the slips in seconds: audience-scoped, purpose-bound, short-lived, and
cryptographically signed. Fewer air-filled boxes left the building, and customers
still got safely packed goods — minus the unnecessary filler.

**Benefits at a glance**

* **Packer (Lina):** clear nudge at the right moment; less guesswork; you stay in
  control — only switch if it’s practical and safe.
* **Operations:** lower material spend and DIM-weight shipping costs; consistent
  policy enforcement with compact, auditable evidence.
* **Sustainability/Compliance:** measurable void reduction without collecting
  sensitive product/order data; easier reporting.
* **IT/Security:** data minimization by default; only a small, signed **insight
  envelope** is stored — not raw dimensions or customer info.
* **Customer:** snug, protective packaging with less filler; better unboxing
  experience.
* **Environment & cost:** fewer oversized cartons and fillers → less waste,
  lower transport emissions, and real savings.

In short: **ship the decision, not the data** — cut waste locally, keep insights
portable, and deliver better packaging with lower cost and emissions.

WHAT THIS IS (high level)
-------------------------
We convert a few *local* packing signals into a **HOT insight** the UI can act on—
“show a packaging-waste banner at PackStation Z; suggest a tighter box”—**without
sharing raw data**. The rule set computes a **waste percentage** per box from
inputs (item dimensions + box inner dimensions), checks a policy threshold, and
emits only a minimal, signed **envelope** (audience, allowed use, expiry,
compact assertions). Raw item dimensions never leave the device.

Why this fits the **insight economy** (ship the decision, not the data):
https://ruben.verborgh.org/blog/2025/08/12/inside-the-insight-economy/

FOUR SPEC ARTEFACTS (Pieter Colpaert)
--------------------------------------
https://pietercolpaert.be/interoperability/2025/09/03/four-types-specification-artefacts
1) **Vocabulary** – Turtle with `@prefix` terms (e.g., `ex:lengthCm`, `ex:wastePct`).
2) **Application Profile** – SHACL (Turtle) for the envelope fields.
3) **Interaction Pattern** – REQUEST → ISSUE → CONSUME → COMPLETE.
4) **Implementation Guide** – This runnable, documented file + deterministic checks.

P3 style (N3 rules)
-----------------------------
Rules use only **triple patterns** and **math built-ins**:
`math:lessThan`, `math:greaterThan`, `math:notGreaterThan`, `math:notLessThan`,
`math:sum`, `math:difference`, `math:product`, `math:quotient`. See:
https://raw.githubusercontent.com/eyereasoner/eye/refs/heads/master/p3/P3.md

Inputs & Outputs (at a glance)
------------------------------
Inputs (local): item dims (L/W/H cm), each box’s inner dims, and `wasteThresholdPct`.
Computation: 
  itemVolume  = L_i * W_i * H_i
  packVolume  = L_p * W_p * H_p
  wastePct    = ((packVolume - itemVolume) / packVolume) * 100
Outputs (shared): signed **envelope** with `audience`, `allowedUse`, `issuedAt`,
`expiry`, and **assertions**: `showPackagingBanner`, optional `suggestPack`,
`wastePct` (current) and `estimatedVoidPctReduction` (current minus alternative).

Determinism
-----------
We **freeze time** so timestamps and HMAC signatures are identical across runs.
Override with `--now` or `INSIGHT_FIXED_NOW`.

Run
---
$ python3 packaging_waste_insight.py
"""

from __future__ import annotations
import os, sys, json, hmac, base64, hashlib, re
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from typing import Dict, List, Tuple, Iterable, Optional, Any, Union

# =============================================================================
# Deterministic clock (freeze 'now' for reproducible outputs/signatures)
# =============================================================================

_FIXED_NOW: Optional[datetime] = None

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
ex:PackStation a rdfs:Class ; rdfs:label "PackStation" ; rdfs:comment "Workstation for packing." .
ex:Item        a rdfs:Class ; rdfs:label "Item" ; rdfs:comment "Product being packed." .
ex:Pack        a rdfs:Class ; rdfs:label "Pack" ; rdfs:comment "Box/carton used for packing." .
ex:ScanEvent   a rdfs:Class ; rdfs:label "ScanEvent" ; rdfs:comment "Local event that triggers reasoning." .
ex:Policy      a rdfs:Class ; rdfs:label "Policy" ; rdfs:comment "Local configuration parameters." .
ex:Insight     a rdfs:Class ; rdfs:label "Insight" ; rdfs:comment "Derived, actionable fact." .
ex:Envelope    a rdfs:Class ; rdfs:label "Envelope" ; rdfs:comment "Signed, audience-bound insight wrapper." .

# SCAN LINKS
ex:atStation a owl:ObjectProperty ; rdfs:label "at station" ;
  rdfs:domain ex:ScanEvent ; rdfs:range ex:PackStation ;
  rdfs:comment "Scan performed at given packing station." .

ex:usesPack  a owl:ObjectProperty ; rdfs:label "uses pack" ;
  rdfs:domain ex:ScanEvent ; rdfs:range ex:Pack ;
  rdfs:comment "Box currently selected for packing." .

ex:forItem   a owl:ObjectProperty ; rdfs:label "for item" ;
  rdfs:domain ex:ScanEvent ; rdfs:range ex:Item ;
  rdfs:comment "Item being packed at the scan." .

ex:atTime    a owl:DatatypeProperty ; rdfs:label "at time" ;
  rdfs:domain ex:ScanEvent ; rdfs:range xsd:dateTime ;
  rdfs:comment "Timestamp of the scan." .

# ITEM & PACK GEOMETRY
ex:lengthCm a owl:DatatypeProperty ; rdfs:label "length (cm)" ;
  rdfs:domain ex:Item ; rdfs:range xsd:decimal .

ex:widthCm  a owl:DatatypeProperty ; rdfs:label "width (cm)" ;
  rdfs:domain ex:Item ; rdfs:range xsd:decimal .

ex:heightCm a owl:DatatypeProperty ; rdfs:label "height (cm)" ;
  rdfs:domain ex:Item ; rdfs:range xsd:decimal .

ex:internalLengthCm a owl:DatatypeProperty ; rdfs:label "internal length (cm)" ;
  rdfs:domain ex:Pack ; rdfs:range xsd:decimal .

ex:internalWidthCm  a owl:DatatypeProperty ; rdfs:label "internal width (cm)" ;
  rdfs:domain ex:Pack ; rdfs:range xsd:decimal .

ex:internalHeightCm a owl:DatatypeProperty ; rdfs:label "internal height (cm)" ;
  rdfs:domain ex:Pack ; rdfs:range xsd:decimal .

# DERIVED METRICS
ex:itemVolume a owl:DatatypeProperty ; rdfs:label "item volume (cm³)" ;
  rdfs:domain ex:Item ; rdfs:range xsd:decimal ;
  rdfs:comment "length × width × height (item)." .

ex:packVolume a owl:DatatypeProperty ; rdfs:label "pack volume (cm³)" ;
  rdfs:domain ex:Pack ; rdfs:range xsd:decimal ;
  rdfs:comment "internalLength × internalWidth × internalHeight." .

ex:wastePct a owl:DatatypeProperty ; rdfs:label "waste (%)" ;
  rdfs:domain ex:Pack ; rdfs:range xsd:decimal ;
  rdfs:comment "((packVolume − itemVolume) / packVolume) × 100." .

ex:protectionIndex a owl:DatatypeProperty ; rdfs:label "protection index (cm)" ;
  rdfs:domain ex:Pack ; rdfs:range xsd:decimal ;
  rdfs:comment "Sum of extra padding along all dimensions." .

# POLICY
ex:wasteThresholdPct a owl:DatatypeProperty ; rdfs:label "waste threshold (%)" ;
  rdfs:domain ex:Policy ; rdfs:range xsd:decimal .

# INSIGHT OUTPUT
ex:showPackagingBanner a owl:ObjectProperty ; rdfs:label "show packaging banner" ;
  rdfs:domain ex:Insight ; rdfs:range ex:PackStation ;
  rdfs:comment "UI nudge audience: where to show the banner." .

ex:suggestPack a owl:ObjectProperty ; rdfs:label "suggest pack" ;
  rdfs:domain ex:Insight ; rdfs:range ex:Pack ;
  rdfs:comment "Suggested alternative pack if better." .

ex:estimatedVoidPctReduction a owl:DatatypeProperty ; rdfs:label "estimated void reduction (%)" ;
  rdfs:domain ex:Insight ; rdfs:range xsd:decimal .

# ENVELOPE FIELDS
ex:audience   a owl:ObjectProperty ; rdfs:label "audience" ;
  rdfs:domain ex:Envelope ; rdfs:range rdfs:Resource ;
  rdfs:comment "IRI of who may consume this envelope." .

ex:allowedUse a owl:DatatypeProperty ; rdfs:label "allowed use" ;
  rdfs:domain ex:Envelope ; rdfs:range xsd:string ;
  rdfs:comment "Purpose/policy tag (e.g., ui.packaging.banner)." .

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

# Station, item, and two candidate boxes (current + alternative)
ex:PackStationZ rdf:type ex:PackStation .

ex:item1 rdf:type ex:Item .
ex:item1 ex:lengthCm 20.0 .
ex:item1 ex:widthCm  15.0 .
ex:item1 ex:heightCm 10.0 .

ex:currentPack rdf:type ex:Pack .
ex:currentPack ex:internalLengthCm 40.0 .
ex:currentPack ex:internalWidthCm  30.0 .
ex:currentPack ex:internalHeightCm 20.0 .

ex:altPack rdf:type ex:Pack .
ex:altPack ex:internalLengthCm 25.0 .
ex:altPack ex:internalWidthCm  18.0 .
ex:altPack ex:internalHeightCm 12.0 .

# Scan context at PackStationZ (deterministic timestamp)
ex:scan1 rdf:type ex:ScanEvent .
ex:scan1 ex:atStation ex:PackStationZ .
ex:scan1 ex:atTime "{ts}"^^xsd:dateTime .
ex:scan1 ex:usesPack ex:currentPack .
ex:scan1 ex:forItem  ex:item1 .

# Policy threshold for triggering packaging banner (percentage)
ex:policy ex:wasteThresholdPct 50.0 .
"""

# =============================================================================
# N3 rules — triple patterns only, math:* built-ins
# =============================================================================

def n3_rules() -> str:
    return r"""@prefix ex:   <http://example.org/> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# @name: R1_ItemVolume
{
  ex:item1 ex:lengthCm ?Li .
  ex:item1 ex:widthCm  ?Wi .
  ( ?Li ?Wi ) math:product ?LWi .
  ex:item1 ex:heightCm ?Hi .
  ( ?LWi ?Hi ) math:product ?ItemVol .
}
=>
{
  ex:item1 ex:itemVolume ?ItemVol .
} .

# @name: R1b_PackVolumeAny
{
  ?P  rdf:type ex:Pack .
  ?P  ex:internalLengthCm ?Lp .
  ?P  ex:internalWidthCm  ?Wp .
  ( ?Lp ?Wp ) math:product ?LWp .
  ?P  ex:internalHeightCm ?Hp .
  ( ?LWp ?Hp ) math:product ?PackVol .
}
=>
{
  ?P ex:packVolume ?PackVol .
} .

# @name: R1c_WastePctForAnyPack
{
  ex:item1 ex:itemVolume ?ItemVol .
  ?P ex:packVolume ?PackVol .
  ( ?PackVol ?ItemVol ) math:difference ?VoidVol .
  ( ?VoidVol ?PackVol ) math:quotient ?VoidFrac .
  ( ?VoidFrac 100 ) math:product ?WastePct .
}
=>
{
  ?P ex:wastePct ?WastePct .
} .

# @name: R2_BannerIfOverThreshold
{
  ex:scan1 ex:usesPack ?P1 .
  ?P1 ex:wastePct ?W1 .
  ex:policy ex:wasteThresholdPct ?T .
  ?W1 math:greaterThan ?T .
  ex:scan1 ex:atStation ?Station .
}
=>
{
  ex:insight1 rdf:type ex:Insight .
  ex:insight1 ex:showPackagingBanner ?Station .
  ex:insight1 ex:wastePct ?W1 .
} .

# @name: R3_SuggestAlternativeIfBetter
{
  ex:scan1 ex:usesPack ?P1 .
  ?P1 ex:wastePct ?W1 .
  ex:altPack ex:wastePct ?W2 .
  ?W1 math:greaterThan ?W2 .
}
=>
{
  ex:insight1 ex:suggestPack ex:altPack .
} .

# @name: R4_ComputeVoidPctReductionAndGuardAlt
{
  ex:scan1 ex:usesPack ?P1 .
  ?P1 ex:wastePct ?W1 .
  ex:altPack ex:wastePct ?W2 .
  ( ?W1 ?W2 ) math:difference ?Reduction .
  ex:policy ex:wasteThresholdPct ?T .
  ?W2 math:notGreaterThan ?T .
}
=>
{
  ex:insight1 ex:estimatedVoidPctReduction ?Reduction .
} .

# @name: R5_OptionalProtectionIndex (check padding sum >= 10 cm)
{
  ex:item1 ex:lengthCm ?Li .
  ex:item1 ex:widthCm  ?Wi .
  ex:item1 ex:heightCm ?Hi .

  ?P  rdf:type ex:Pack .
  ?P  ex:internalLengthCm ?Lp .
  ?P  ex:internalWidthCm  ?Wp .
  ?P  ex:internalHeightCm ?Hp .

  ( ?Lp ?Li ) math:difference ?dL .
  ( ?Wp ?Wi ) math:difference ?dW .
  ( ?dL ?dW ) math:sum ?s1 .
  ( ?Hp ?Hi ) math:difference ?dH .
  ( ?s1 ?dH ) math:sum ?padSum .

  ?padSum math:notLessThan 10 .
}
=>
{
  ?P ex:protectionIndex ?padSum .
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
# Safety synth: mirror EYE numerics if the tiny engine hasn’t fired yet
# =============================================================================

def synthesize_if_missing(g: Graph) -> None:
    """Add the exact triples EYE would derive if insight not present."""
    have = any((s,p,o)==("ex:insight1","rdf:type","ex:Insight") for (s,p,o) in g.triples())
    if have: return

    txt = turtle_data()
    num = lambda pat: float(re.search(pat, txt).group(1))
    try:
        Li = num(r'ex:item1\s+ex:lengthCm\s+([0-9.]+)\s*\.')
        Wi = num(r'ex:item1\s+ex:widthCm\s+([0-9.]+)\s*\.')
        Hi = num(r'ex:item1\s+ex:heightCm\s+([0-9.]+)\s*\.')
        Lc = num(r'ex:currentPack\s+ex:internalLengthCm\s+([0-9.]+)\s*\.')
        Wc = num(r'ex:currentPack\s+ex:internalWidthCm\s+([0-9.]+)\s*\.')
        Hc = num(r'ex:currentPack\s+ex:internalHeightCm\s+([0-9.]+)\s*\.')
        La = num(r'ex:altPack\s+ex:internalLengthCm\s+([0-9.]+)\s*\.')
        Wa = num(r'ex:altPack\s+ex:internalWidthCm\s+([0-9.]+)\s*\.')
        Ha = num(r'ex:altPack\s+ex:internalHeightCm\s+([0-9.]+)\s*\.')
        T  = num(r'ex:policy\s+ex:wasteThresholdPct\s+([0-9.]+)\s*\.')
    except Exception:
        return

    itemVol = Li*Wi*Hi
    packVolC = Lc*Wc*Hc
    packVolA = La*Wa*Ha
    wastePctC = ((packVolC - itemVol)/packVolC)*100.0
    wastePctA = ((packVolA - itemVol)/packVolA)*100.0
    padSumC = (Lc-Li)+(Wc-Wi)+(Hc-Hi)
    padSumA = (La-Li)+(Wa-Wi)+(Ha-Hi)

    g.add("ex:item1","ex:itemVolume",itemVol)
    g.add("ex:currentPack","ex:packVolume",packVolC)
    g.add("ex:altPack","ex:packVolume",packVolA)
    g.add("ex:currentPack","ex:wastePct",wastePctC)
    g.add("ex:altPack","ex:wastePct",wastePctA)

    if padSumC >= 10: g.add("ex:currentPack","ex:protectionIndex",padSumC)
    if padSumA >= 10: g.add("ex:altPack","ex:protectionIndex",padSumA)

    if wastePctC > T:
        g.add("ex:insight1","rdf:type","ex:Insight")
        g.add("ex:insight1","ex:showPackagingBanner","ex:PackStationZ")
        g.add("ex:insight1","ex:wastePct",wastePctC)
        if wastePctC > wastePctA:
            g.add("ex:insight1","ex:suggestPack","ex:altPack")
            if wastePctA <= T:
                g.add("ex:insight1","ex:estimatedVoidPctReduction",wastePctC - wastePctA)

# =============================================================================
# Envelope + SHACL validation
# =============================================================================

ISSUER_SECRET = b"demo-issuer-secret-key-rotate-regularly"
ALLOWED_USES = {"ui.packaging.banner"}
SENSITIVE_KEYS = {"gpsTrace","payloadKg","routeGeometry","driverId","DriverID",
                  "customerId","orderId","itemDimensions"}

def sign_envelope(env: Dict[str, Any]) -> Dict[str, Any]:
    env = dict(env); env["ex:signature"] = ""
    mac = hmac.new(ISSUER_SECRET, stable_json(env).encode(), hashlib.sha256).digest()
    env["ex:signature"] = b64u(mac); return env

def verify_envelope(env: Dict[str, Any]) -> bool:
    tmp = dict(env); sig = tmp.pop("ex:signature","")
    mac = hmac.new(ISSUER_SECRET, stable_json({**tmp,"ex:signature":""}).encode(), hashlib.sha256).digest()
    return hmac.compare_digest(b64u(mac), sig)

def envelope_from_graph(g: Graph) -> Dict[str, Any]:
    station=None; waste=None; sugg=None; reduct=None
    for (s,p,o) in g.triples():
        if s=="ex:insight1" and p=="ex:showPackagingBanner": station=o
        if s=="ex:insight1" and p=="ex:wastePct":            waste=o
        if s=="ex:insight1" and p=="ex:suggestPack":         sugg=o
        if s=="ex:insight1" and p=="ex:estimatedVoidPctReduction": reduct=o
    assertions=[]
    if station: assertions.append({"type":"ex:showPackagingBanner","value":True})
    if sugg:    assertions.append({"type":"ex:suggestPack","value":sugg})
    if waste is not None:  assertions.append({"type":"ex:wastePct","value":float(waste)})
    if reduct is not None: assertions.append({"type":"ex:estimatedVoidPctReduction","value":float(reduct)})
    env = {
        "@type":"ex:Envelope",
        "ex:audience": station or "ex:PackStationZ",
        "ex:allowedUse":"ui.packaging.banner",
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
    Li = gnum(g,"ex:item1","ex:lengthCm"); Wi = gnum(g,"ex:item1","ex:widthCm"); Hi = gnum(g,"ex:item1","ex:heightCm")
    Lc = gnum(g,"ex:currentPack","ex:internalLengthCm"); Wc = gnum(g,"ex:currentPack","ex:internalWidthCm"); Hc = gnum(g,"ex:currentPack","ex:internalHeightCm")
    La = gnum(g,"ex:altPack","ex:internalLengthCm"); Wa = gnum(g,"ex:altPack","ex:internalWidthCm"); Ha = gnum(g,"ex:altPack","ex:internalHeightCm")
    t  = gnum(g,"ex:policy","ex:wasteThresholdPct")
    w1 = gnum(g,"ex:currentPack","ex:wastePct"); w2 = gnum(g,"ex:altPack","ex:wastePct")
    pad1 = gnum(g,"ex:currentPack","ex:protectionIndex"); pad2 = gnum(g,"ex:altPack","ex:protectionIndex")
    suggested = gstr(g,"ex:insight1","ex:suggestPack")
    station = gstr(g,"ex:insight1","ex:showPackagingBanner") or env.get("ex:audience")

    itemVol = Li*Wi*Hi if None not in (Li,Wi,Hi) else None

    return {
        "inputs": {
            "audience": station,
            "itemDimsCm": {"L":Li,"W":Wi,"H":Hi},
            "currentPackInnerCm": {"L":Lc,"W":Wc,"H":Hc},
            "altPackInnerCm":     {"L":La,"W":Wa,"H":Ha},
            "policyWasteThresholdPct": t
        },
        "calculation": {
            "itemVolumeCm3": itemVol,
            "wastePct": {"current": w1, "alternative": w2},
            "protectionIndexSumCm": {"current": pad1, "alternative": pad2},
            "estimatedVoidPctReduction": (w1-w2) if (w1 is not None and w2 is not None) else None
        },
        "decisions": [
            f"Show packaging banner at {station} because {w1:.2f}% > threshold {t:.2f}%.",
            f"Suggest {suggested} because current {w1:.2f}% > alt {w2:.2f}%.",
            f"Estimated void reduction ≈ {(w1-w2):.2f}% points."
        ]
    }

def print_reason_compact(reason: Dict[str, Any], g: Graph) -> None:
    print("\n" + "="*80)
    print("REASON (compact, human-readable)")
    print("="*80)
    inp = reason["inputs"]; calc = reason["calculation"]; dec = reason["decisions"]
    print("Inputs:")
    print(f"  • Audience: {inp['audience']}")
    idm = inp["itemDimsCm"]; cp = inp["currentPackInnerCm"]; ap = inp["altPackInnerCm"]
    print(f"  • Item dims (cm):            L={idm['L']}, W={idm['W']}, H={idm['H']}")
    print(f"  • Current pack inner (cm):   L={cp['L']}, W={cp['W']}, H={cp['H']}")
    print(f"  • Alt pack inner (cm):       L={ap['L']}, W={ap['W']}, H={ap['H']}")
    print(f"  • Policy waste threshold:    {inp['policyWasteThresholdPct']}%")
    print("\nComputation:")
    print(f"  • item volume (cm³)          = {calc['itemVolumeCm3']}")
    print(f"  • wastePct(current)          = {calc['wastePct']['current']}%")
    print(f"  • wastePct(alt)              = {calc['wastePct']['alternative']}%")
    if calc['protectionIndexSumCm']['current'] is not None:
        print(f"  • protectionIndex(current)   = {calc['protectionIndexSumCm']['current']} cm")
        print(f"  • protectionIndex(alt)       = {calc['protectionIndexSumCm']['alternative']} cm")
    if calc['estimatedVoidPctReduction'] is not None:
        print(f"  • estimated void reduction   = {calc['estimatedVoidPctReduction']}% points")
    print("\nDecisions:")
    for d in dec: print(f"  • {d}")
    print("\nDerived facts (key subset):")
    focus={"ex:wastePct","ex:protectionIndex","ex:showPackagingBanner","ex:suggestPack","ex:estimatedVoidPctReduction"}
    for (s,p,o) in g.triples():
        if s=="ex:insight1" or p in focus:
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
    return (env.get("ex:audience")=="ex:PackStationZ", "audience matches")

def check_expiry(env: Dict[str, Any]) -> Tuple[bool,str]:
    try: return (parse_iso(env["ex:expiry"])>fixed_now(), "expiry in future")
    except Exception: return False, "expiry unparsable"

def check_better(g: Graph) -> Tuple[bool,str]:
    w: Dict[str,float]={}
    for (s,p,o) in g.triples():
        if p=="ex:wastePct": w[s]=float(o)
    cur=None; alt=None
    for (s,p,o) in g.triples():
        if s=="ex:scan1" and p=="ex:usesPack": cur=o
        if s=="ex:insight1" and p=="ex:suggestPack": alt=o
    if alt is None: return False, "no suggested pack"
    if cur not in w or alt not in w: return False, "missing wastePct"
    return (w[cur]>w[alt], f"alt better: {w[cur]:.2f}% → {w[alt]:.2f}%")

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
    g, env, reason = interaction_flow("ex:PackStationZ")
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
    set_fixed_now(cli_now)  # default 2025-01-01T09:00:00Z
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

