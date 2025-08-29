
from __future__ import annotations
"""
Clinical Support (Educational Demo) — EYE-style mixed computation
=================================================================

  ME (user) ---------------------+
                                |        +-------------------+
                                |        |   Behaviors       |  (learned policy templates /
                                |        |   (rules/N3)      |   rulebooks)
                                |        +-------------------+
                                |                 ^    ^
                                |                 |    |
                                v                 |    |  context (dynamic)
+-------------+     AC      +----------+          |    |
| data        | <---------> | context  |----------+    |
| (RDF)       |   (access)  +----------+               |
+-------------+                                        v
                          Agent (partial evaluator / codegen)
                                   |
                                   v
                           Driver (specialized scoring fn)
                                   |
                                   v
              +----------------------------------------------+
              | Targets / Capabilities / Context (candidates)| --> ranked options
              +----------------------------------------------+
                                   |
                                   v
                         actionable insight + feedback

Legend:
- ME:            the clinician/user persona (here implicit; preferences could be added)
- data:          RDF (Turtle) for ontology fragments, default policy weights, thresholds
- context:       RDF (Turtle) for patient vitals/labs/comorbidities + local settings
- behaviors:     N3 rule templates that describe how scoring/logic works at a high level
- agent:         partial evaluator that closes over static constants (compile-time)
- driver:        specialized scorer that consumes dynamic facts per patient (run-time)
- targets/caps:  actions/interventions available in this ED encounter
- actionable insight: ranked interventions + justification trace (for auditability)

Example scenario
----------------
Toy early sepsis risk stratification and intervention prioritization:
- Compute a qSOFA-like score (RR, SBP, mental status) + lactate contribution.
- Adjust priorities based on context (CHF fluid caution, allergies constrain antibiotic
  class).
- Output a ranked list of **generic** interventions (not drug names), e.g.:
  - Trigger Sepsis Protocol
  - Order Lactate
  - Start Isotonic Fluids (caution w/ CHF)
  - Begin Empiric Antibiotics (avoid beta-lactams if allergy; general advice only)

Outputs
-------
- A folder output/clinicsupport_artifacts with RDF/N3 and CSV:
    static.ttl, dynamic.ttl, rules-static.n3, rules-dynamic.n3, ranked_actions.csv
- Console prints 'ALL TESTS PASSED' if self-checks succeed.

SAFETY NOTICE
-------------
This program is an **illustrative demo** only, not medical advice, not a medical
device, and not intended for clinical use. It simplifies complex topics and is
unsafe for real patient care. Use it only to understand the data/reasoning
patterns (RDF + N3 + partial evaluation) discussed in the EYE learning guide.

"""
import os, math, csv
from typing import Any, Dict, List, Tuple

BASE = os.path.join(os.path.dirname(__file__), "output/clinicsupport_artifacts")
os.makedirs(BASE, exist_ok=True)

EX = "http://example.org/clinic#"

# ---------------------------------------------------------------------------
# STATIC (compile-time): ontology fragments + default policies/thresholds
# ---------------------------------------------------------------------------
STATIC_TTL = """@prefix ex: <http://example.org/clinic#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Ontology-ish stubs
ex:Patient a ex:Class .
ex:Action  a ex:Class .

# Default policy weights (compile-time constants)
# These are learned or standardized defaults that the Agent can bake in.
ex:defaultPolicy ex:qsofaWeight 1.0 .
ex:defaultPolicy ex:lactateWeight 0.5 .
ex:defaultPolicy ex:interventionThreshold 1.0 .
ex:defaultPolicy ex:fluidBasePriority 0.6 .
ex:defaultPolicy ex:lactateOrderPriority 0.9 .
ex:defaultPolicy ex:antibioticBasePriority 0.8 .
ex:defaultPolicy ex:contraFluidCHF 0.4 .
ex:defaultPolicy ex:contraBetaLactamAllergy 1.0 .

# qSOFA cutoffs (compile-time constants)
ex:qsofaRRcutoff ex:value 22 .      # respirations per min
ex:qsofaSBPcutoff ex:value 100 .    # systolic BP mmHg
ex:qsofaMentationFlag ex:value "altered" .

# Lactate scaling parameters (toy)
ex:lactateUpper ex:value 4.0 .
ex:lactateLower ex:value 2.0 .
"""

# ---------------------------------------------------------------------------
# DYNAMIC (run-time): patient context + available actions (targets/caps)
# ---------------------------------------------------------------------------
DYNAMIC_TTL = """@prefix ex: <http://example.org/clinic#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Patient context (dynamic)
ex:pt123 a ex:Patient .
ex:pt123 ex:RR 24 .
ex:pt123 ex:SBP 92 .
ex:pt123 ex:MentalStatus "altered" .
ex:pt123 ex:Lactate 4.2 .
ex:pt123 ex:TempC 38.6 .
ex:pt123 ex:HR 118 .
ex:pt123 ex:Comorbidity "CHF" .
ex:pt123 ex:Allergy "beta_lactam" .

# Available actions (targets/capabilities), kept deliberately generic
ex:act_protocol a ex:Action .
ex:act_protocol ex:label "Trigger Sepsis Protocol" .

ex:act_lactate a ex:Action .
ex:act_lactate ex:label "Order Lactate + Broad Labs" .

ex:act_fluids a ex:Action .
ex:act_fluids ex:label "Start Isotonic Fluids (titrate/caution)" .

ex:act_antibiotics a ex:Action .
ex:act_antibiotics ex:label "Begin Empiric Antibiotics (class selection per allergy)" .

# Contextual constraints in this encounter
ex:pt123 ex:contraindication ex:betaLactamIfAllergy .  # avoid beta-lactams
ex:pt123 ex:fluidCaution ex:CHF .                      # fluids with CHF -> caution
"""

# ---------------------------------------------------------------------------
# Behaviors: documented as N3 rule templates (mirrors what the code enforces)
# ---------------------------------------------------------------------------
STATIC_N3 = """@prefix math: <http://www.w3.org/2000/10/swap/math#>.
@prefix ex: <http://example.org/clinic#> .

# qSOFA points
{ ?p ex:RR ?rr . ?rr math:greaterThan 22 } => { ?p ex:qsofaPoint 1 } .
{ ?p ex:SBP ?sbp . ?sbp math:lessThan 100 } => { ?p ex:qsofaPoint 1 } .
{ ?p ex:MentalStatus "altered" } => { ?p ex:qsofaPoint 1 } .

# Lactate contribution (piecewise)
{ ?p ex:Lactate ?l . ?l math:notLessThan 4.0 } => { ?p ex:lactateBucket "high" } .
{ ?p ex:Lactate ?l . ?l math:notLessThan 2.0 . ?l math:lessThan 4.0 } => { ?p ex:lactateBucket "mod" } .

# Risk -> interventions
{ ?p ex:risk ?r . ?r math:greaterThan 1.0 } => { ?p ex:shouldTrigger ex:act_protocol } .
{ ?p ex:risk ?r } => { ?p ex:shouldOrder ex:act_lactate } .

# Contraindications (documentation mirror)
{ ?p ex:Allergy "beta_lactam" } => { ex:act_antibiotics ex:penalized true } .
{ ?p ex:Comorbidity "CHF" }    => { ex:act_fluids ex:penalized true } .
"""

DYNAMIC_N3 = """@prefix ex: <http://example.org/clinic#> .
# Here we could capture dynamic preference weights, staffing load, etc.
# (kept empty in this toy example; policy overrides would slot here).
"""

def _write(name: str, content: str):
    path = os.path.join(BASE, name)
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)
    return path

_write("static.ttl", STATIC_TTL)
_write("dynamic.ttl", DYNAMIC_TTL)
_write("rules-static.n3", STATIC_N3)
_write("rules-dynamic.n3", DYNAMIC_N3)

# ---------------------------------------------------------------------------
# Tiny Turtle reader
# ---------------------------------------------------------------------------
def parse_ttl(ttl_text: str):
    prefixes, triples = {}, []
    for raw in ttl_text.splitlines():
        line = raw.strip()
        if not line or line.startswith("#"): 
            continue
        if line.startswith("@prefix"):
            parts = line.split()
            prefixes[parts[1].rstrip(":")] = parts[2].strip("<>")
            continue
        if line.endswith("."): 
            line = line[:-1].strip()
        toks, buf, q = [], "", False
        for ch in line:
            if ch == '"':
                q = not q
                buf += ch
                continue
            if ch == ' ' and not q:
                if buf:
                    toks.append(buf)
                    buf = ''
            else:
                buf += ch
        if buf: 
            toks.append(buf)
        if len(toks) < 3: 
            continue
        s, p, o = toks[0], toks[1], ' '.join(toks[2:])
        def expand(t):
            if t.startswith('<') and t.endswith('>'):
                return t[1:-1]
            if ':' in t and not t.startswith('"'):
                pref, local = t.split(':', 1)
                return prefixes.get(pref, pref + ':') + local
            return t
        s_e, p_e = expand(s), expand(p)
        if o.startswith('"') and o.endswith('"'):
            obj = o.strip('"')
        elif o in ('true','false'):
            obj = (o == 'true')
        else:
            try:
                obj = float(o) if '.' in o else int(o)
            except Exception:
                obj = expand(o)
        triples.append((s_e, p_e, obj))
    return prefixes, triples

def triples_index(triples):
    idx = {}
    for s,p,o in triples:
        idx.setdefault(s, {}).setdefault(p, []).append(o)
    return idx

def get1(idx, s, p, default=None):
    vals = idx.get(s, {}).get(p, [])
    return vals[0] if vals else default

# Load graphs
with open(os.path.join(BASE, "static.ttl"), encoding="utf-8") as f:
    _, S_triples = parse_ttl(f.read())
with open(os.path.join(BASE, "dynamic.ttl"), encoding="utf-8") as f:
    _, D_triples = parse_ttl(f.read())

S = triples_index(S_triples)
D = triples_index(D_triples)

# ---------------------------------------------------------------------------
# AGENT: capture constants and build a specialized DRIVER via closures
# ---------------------------------------------------------------------------
def policy_get(pred, default):
    return float(get1(S, EX+"defaultPolicy", EX+pred, default))

POLICY = {
    "qsofaWeight": policy_get("qsofaWeight", 1.0),
    "lactateWeight": policy_get("lactateWeight", 0.5),
    "interventionThreshold": policy_get("interventionThreshold", 1.0),
    "fluidBasePriority": policy_get("fluidBasePriority", 0.6),
    "lactateOrderPriority": policy_get("lactateOrderPriority", 0.9),
    "antibioticBasePriority": policy_get("antibioticBasePriority", 0.8),
    "contraFluidCHF": policy_get("contraFluidCHF", 0.4),
    "contraBetaLactamAllergy": policy_get("contraBetaLactamAllergy", 1.0),
    "qsofaRRcutoff": 22.0,
    "qsofaSBPcutoff": 100.0,
    "qsofaMentationFlag": "altered",
    "lactateUpper": 4.0,
    "lactateLower": 2.0,
}

def make_sepsis_driver(policy):
    q_w = policy["qsofaWeight"]; l_w = policy["lactateWeight"]
    thr = policy["interventionThreshold"]
    lact_lo, lact_hi = policy["lactateLower"], policy["lactateUpper"]
    rr_cut, sbp_cut, ment_flag = policy["qsofaRRcutoff"], policy["qsofaSBPcutoff"], policy["qsofaMentationFlag"]
    p_fluid_base = policy["fluidBasePriority"]
    p_lact_order = policy["lactateOrderPriority"]
    p_abx_base = policy["antibioticBasePriority"]
    pen_fluid_chf = policy["contraFluidCHF"]
    pen_beta_allergy = policy["contraBetaLactamAllergy"]

    def compute_qsofa(vitals: Dict[str, float | str]) -> Tuple[float, List[str]]:
        pts = 0; why = []
        if vitals.get("RR", 0) >= rr_cut: pts += 1; why.append("RR≥22")
        if vitals.get("SBP", 999) <= sbp_cut: pts += 1; why.append("SBP≤100")
        if vitals.get("MentalStatus","normal") == ment_flag: pts += 1; why.append("Altered mentation")
        return q_w * pts, why

    def lactate_score(lactate: float) -> Tuple[float, str]:
        if lactate >= lact_hi: return l_w * 1.0, "Lactate high (≥4)"
        if lactate >= lact_lo: return l_w * 0.5, "Lactate moderate (2–4)"
        return 0.0, "Lactate low"

    def propose_actions(patient: str, facts: Dict[str, Any]) -> List[Tuple[str, float, List[str]]]:
        vitals = {k: facts.get(k) for k in ["RR","SBP","MentalStatus","Lactate","TempC","HR"]}
        q, qwhy = compute_qsofa(vitals)
        l, lwhy = lactate_score(facts.get("Lactate", 0.0))
        risk = q + l
        out: List[Tuple[str, float, List[str]]] = []

        # Protocol trigger
        score_protocol = risk - thr
        out.append(("Trigger Sepsis Protocol", score_protocol, [f"risk={risk:.2f} - thr={thr:.2f}", *(qwhy+[lwhy])]))

        # Order lactate (and broad labs)
        out.append(("Order Lactate + Broad Labs", p_lact_order + risk*0.1, ["baseline="+str(p_lact_order), f"risk bump={risk*0.1:.2f}"]))

        # Start fluids (CHF caution)
        fluids_score = p_fluid_base
        trace = [f"baseline={p_fluid_base}"]
        if "CHF" in facts.get("Comorbidity",""):
            fluids_score -= pen_fluid_chf
            trace.append(f"-CHF penalty={pen_fluid_chf}")
        out.append(("Start Isotonic Fluids (titrate/caution)", fluids_score, trace))

        # Begin empiric antibiotics (avoid beta-lactams if allergy)
        abx_score = p_abx_base + risk*0.1
        abx_trace = [f"baseline={p_abx_base}", f"risk bump={risk*0.1:.2f}"]
        if "beta_lactam" in facts.get("Allergy",""):
            abx_score -= pen_beta_allergy
            abx_trace.append(f"-allergy penalty={pen_beta_allergy}")
            abx_trace.append("Note: select NON-beta-lactam class")
        out.append(("Begin Empiric Antibiotics (class per allergy)", abx_score, abx_trace))

        return out

    return propose_actions

driver = make_sepsis_driver(POLICY)

# Materialize patient facts
pt = EX + "pt123"
facts = {
    "RR": float(get1(D, pt, EX+"RR", 0.0)),
    "SBP": float(get1(D, pt, EX+"SBP", 0.0)),
    "MentalStatus": get1(D, pt, EX+"MentalStatus", "normal"),
    "Lactate": float(get1(D, pt, EX+"Lactate", 0.0)),
    "TempC": float(get1(D, pt, EX+"TempC", 0.0)),
    "HR": float(get1(D, pt, EX+"HR", 0.0)),
    "Comorbidity": get1(D, pt, EX+"Comorbidity", ""),
    "Allergy": get1(D, pt, EX+"Allergy", ""),
}

actions = driver(pt, facts)
actions_sorted = sorted(actions, key=lambda x: x[1], reverse=True)

# Self-checks
protocol = [a for a in actions_sorted if a[0].startswith("Trigger Sepsis")][0]
assert protocol[1] > 0, "Expected protocol to be favored (risk above threshold)"
abx = [a for a in actions_sorted if a[0].startswith("Begin Empiric Antibiotics")][0]
assert any("allergy penalty" in s for s in abx[2]), "Antibiotic allergy penalty missing"
assert any("NON-beta-lactam" in s for s in abx[2]), "Allergy note missing"
fluids = [a for a in actions_sorted if a[0].startswith("Start Isotonic Fluids")][0]
assert any("CHF" in s for s in fluids[2]), "CHF caution penalty missing for fluids"
lact = [a for a in actions_sorted if a[0].startswith("Order Lactate")][0]
assert lact is not None, "Lactate order action missing"

# Emit CSV
csv_path = os.path.join(BASE, "ranked_actions.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["action","score","trace"])
    for name, score, trace in actions_sorted:
        w.writerow([name, f"{score:.3f}", " | ".join(trace)])

print("ALL TESTS PASSED")
print("Top actions (toy, educational):")
for name, score, trace in actions_sorted[:3]:
    print(" -", name, "=>", round(score,3))

