from __future__ import annotations
"""
ClinicSupport — EYE-style mixed computation (final, educational demo)
====================================================================
Static RDF + N3 intent → Agent (partial eval, Ershov) → specialized Driver → ranked interventions.

One-liner: a compact EYE-style planner that partially evaluates static policy/clinical constraints
plus N3 rule intent into a specialized driver which consumes dynamic patient context and produces
a feasibility-checked, benefit/risk/cost–aware ranking with explanation traces.

ASCII schematic
---------------
  ME (clinician/student) ---------+
                                  |         +-------------------+
                                  |         |   Behaviors       |  (rule docs)
                                  |         |   (N3 templates)  |
                                  |         +-------------------+
                                  |                  ^    ^
                                  |                  |    |
                                  v                  |    | context (patient state)
+-------------------+   AC    +----------+           |    |
| data (RDF, static)| <-----> | context  |-----------+    |
| policy, thresholds| (access)+----------+                |
+-------------------+                                     v
                         Agent (partial evaluator / specialization)
                                     |
                                     v
                           Driver (specialized scorer)
                                     |
                                     v
             +-------------------------------------------------------------+
             | Targets / Capabilities / Context (interventions & patient)  | --> ranked list
             +-------------------------------------------------------------+
                                     |
                                     v
                          actionable insight + feedback (trace)

NOTE: This script is an educational demonstration — NOT medical advice.
"""

import os, csv, json
from typing import Any, Dict, List, Tuple

# -----------------------------------------------------------------------------
# Artifact directory
# -----------------------------------------------------------------------------
BASE = os.path.join(os.path.dirname(__file__), "output/clinicsupport_artifacts")
os.makedirs(BASE, exist_ok=True)

EX = "http://example.org/clinic#"

# -----------------------------------------------------------------------------
# STATIC RDF (compile-time): policy weights, base benefits, constraints/thresholds
# -----------------------------------------------------------------------------
STATIC_TTL = """@prefix ex: <http://example.org/clinic#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~ 1.0): minimize score = wRisk*riskN + wCost*costN + wBenefit*(1 - benefitN)
ex:policy ex:wRisk   0.50 .
ex:policy ex:wCost   0.20 .
ex:policy ex:wBenefit 0.30 .

# Condition vocabulary (examples)
ex:HTN  a ex:Condition .
ex:T2D  a ex:Condition .
ex:CKD  a ex:Condition .
ex:Pregnancy a ex:Context .

# Interventions (names as IRIs)
ex:ACEi           a ex:Intervention .   # ACE inhibitor (e.g., lisinopril)
ex:BB_nonselect   a ex:Intervention .   # non-selective beta-blocker
ex:Metformin      a ex:Intervention .
ex:SGLT2          a ex:Intervention .
ex:Amoxicillin    a ex:Intervention .
ex:NSAID          a ex:Intervention .

# Base benefit signals per targeted condition (0..1), rough/illustrative only
ex:ACEi         ex:benefitHTN 0.85 .
ex:BB_nonselect ex:benefitHTN 0.60 .
ex:Metformin    ex:benefitT2D 0.85 .
ex:SGLT2        ex:benefitT2D 0.75 .
ex:NSAID        ex:benefitPain 0.70 .
ex:Amoxicillin  ex:benefitInfection 0.80 .

# Default monthly costs (arbitrary units)
ex:ACEi         ex:costPerMonth 4 .
ex:BB_nonselect ex:costPerMonth 3 .
ex:Metformin    ex:costPerMonth 2 .
ex:SGLT2        ex:costPerMonth 45 .
ex:Amoxicillin  ex:costPerMonth 5 .
ex:NSAID        ex:costPerMonth 1 .

# Hard contraindications / thresholds (illustrative)
# Pregnancy: avoid ACE inhibitors and SGLT2
ex:ACEi  ex:contraInContext ex:Pregnancy .
ex:SGLT2 ex:contraInContext ex:Pregnancy .

# Asthma: avoid nonselective beta-blockers
ex:BB_nonselect ex:contraIfAsthma true .

# Renal function: Metformin and SGLT2 require eGFR >= threshold (ml/min/1.73m2)
ex:Metformin ex:minEgfr 30 .
ex:SGLT2     ex:minEgfr 30 .

# Allergy: Amoxicillin contraindicated if penicillin allergy
ex:Amoxicillin ex:contraIfAllergyPenicillin true .
"""

# -----------------------------------------------------------------------------
# DYNAMIC RDF (run-time): patient facts + targeted problems
# -----------------------------------------------------------------------------
def make_dynamic_ttl() -> str:
    ln = [
        "@prefix ex: <http://example.org/clinic#> .",
        "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
        "",
        "# Patient context",
        "ex:pt ex:ageYears 54 .",
        "ex:pt ex:pregnant false .",
        "ex:pt ex:asthma false .",
        "ex:pt ex:egfr 42 .",
        "ex:pt ex:allergyPenicillin true .",
        "",
        "# Presenting problems / goals",
        "ex:pt ex:hasCondition ex:HTN .",
        "ex:pt ex:hasCondition ex:T2D .",
        # CKD present to make eGFR relevant
        "ex:pt ex:hasCondition ex:CKD .",
        "",
        "# Candidate interventions under consideration",
        "ex:cand ex:consider ex:ACEi .",
        "ex:cand ex:consider ex:BB_nonselect .",
        "ex:cand ex:consider ex:Metformin .",
        "ex:cand ex:consider ex:SGLT2 .",
        "ex:cand ex:consider ex:Amoxicillin .",
        "ex:cand ex:consider ex:NSAID .",
    ]
    return "\n".join(ln)

DYNAMIC_TTL = make_dynamic_ttl()

# -----------------------------------------------------------------------------
# Behaviors as N3 (valid triple-only math built-ins). Documentation mirror.
# -----------------------------------------------------------------------------
RULES_N3 = """@prefix ex:   <http://example.org/clinic#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# -----------------------------------------------------------------------------
# Feasibility (illustrative): intervention is infeasible if any hard contraindication holds
# -----------------------------------------------------------------------------
{ ?i ex:contraInContext ex:Pregnancy . ?pt ex:pregnant true . } => { ?i ex:feasible false } .
{ ?i ex:contraIfAsthma true .        ?pt ex:asthma true .     } => { ?i ex:feasible false } .
{ ?i ex:contraIfAllergyPenicillin true . ?pt ex:allergyPenicillin true . } => { ?i ex:feasible false } .
{ ?i ex:minEgfr ?thr . ?pt ex:egfr ?gfr . ?thr math:greaterThan ?gfr . } => { ?i ex:feasible false } .

# -----------------------------------------------------------------------------
# Benefit proxy: combine per-condition benefit terms the patient actually has
# benefit = sum(benefitHTN for HTN?  else 0) + sum(benefitT2D for T2D? else 0) + ...
# For brevity we document a normalized variant in the driver; here we state the shape.
# -----------------------------------------------------------------------------
# Score = wRisk*riskN + wCost*costN + wBenefit*(1 - benefitN)
# (Driver provides normalization; N3 shows the triple-only intent.)
{ ?i ex:riskN ?rN . ?i ex:costN ?cN . ?i ex:benefitN ?bN .
  ex:policy ex:wRisk ?wR . ex:policy ex:wCost ?wC . ex:policy ex:wBenefit ?wB .
  ( 1 ?bN ) math:difference ?invB .
  ( ?wR ?rN ) math:product ?rTerm .
  ( ?wC ?cN ) math:product ?cTerm .
  ( ?wB ?invB ) math:product ?bTerm .
  ( ?rTerm ?cTerm ) math:sum ?rc .
  ( ?rc ?bTerm ) math:sum ?score .
} => { ?i ex:score ?score } .
"""

def _write(path: str, content: str):
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)

_write(os.path.join(BASE, "static.ttl"), STATIC_TTL)
_write(os.path.join(BASE, "dynamic.ttl"), DYNAMIC_TTL)
_write(os.path.join(BASE, "rules.n3"),   RULES_N3)

# -----------------------------------------------------------------------------
# Tiny Turtle Reader (robust to inline '#' and '#' inside IRIs; supports multi-triple lines)
# -----------------------------------------------------------------------------
def parse_ttl(ttl_text: str):
    prefixes, triples = {}, []

    def strip_inline_comments(line: str) -> str:
        in_quotes = False
        in_angle = False
        out = []
        for ch in line.rstrip():
            if ch == '"' and not in_angle:
                in_quotes = not in_quotes
            elif ch == '<' and not in_quotes:
                in_angle = True
            elif ch == '>' and not in_quotes:
                in_angle = False
            if ch == '#' and not in_quotes and not in_angle:
                break
            out.append(ch)
        return "".join(out).strip()

    def split_statements(line: str) -> List[str]:
        stmts, buf = [], []
        in_quotes = False
        in_angle = False
        for ch in line:
            if ch == '"' and not in_angle:
                in_quotes = not in_quotes; buf.append(ch); continue
            if ch == '<' and not in_quotes:
                in_angle = True; buf.append(ch); continue
            if ch == '>' and not in_quotes:
                in_angle = False; buf.append(ch); continue
            if ch == '.' and not in_quotes and not in_angle:
                # End of statement
                stmts.append("".join(buf).strip()); buf = []
                continue
            buf.append(ch)
        if buf and "".join(buf).strip():
            stmts.append("".join(buf).strip())
        return [s for s in stmts if s]

    for raw in ttl_text.splitlines():
        line = strip_inline_comments(raw)
        if not line:
            continue

        if line.startswith("@prefix"):
            parts = line.split()
            if len(parts) >= 3:
                prefixes[parts[1].rstrip(":")] = parts[2].strip("<>")
            continue

        for stmt in split_statements(line):
            # Tokenize (space-splitting outside quotes)
            toks, buf = [], ""
            in_quotes = False
            for ch in stmt.strip():
                if ch == '"':
                    in_quotes = not in_quotes; buf += ch
                elif ch == ' ' and not in_quotes:
                    if buf: toks.append(buf); buf = ""
                else:
                    buf += ch
            if buf: toks.append(buf)
            if len(toks) < 3:
                continue

            s, p, o = toks[0], toks[1], " ".join(toks[2:])

            def expand(t: str) -> str:
                if t.startswith("<") and t.endswith(">"):
                    return t[1:-1]
                if ":" in t and not t.startswith('"'):
                    pref, local = t.split(":", 1)
                    return prefixes.get(pref, pref + ":") + local
                return t

            s_e, p_e = expand(s), expand(p)

            if o.startswith('"') and o.endswith('"'):
                obj: Any = o.strip('"')
            elif o in ("true", "false"):
                obj = (o == "true")
            else:
                try:
                    obj = float(o) if "." in o else int(o)
                except Exception:
                    obj = expand(o)

            triples.append((s_e, p_e, obj))
    return prefixes, triples

def index_triples(triples: List[Tuple[str,str,Any]]):
    idx: Dict[str, Dict[str, List[Any]]] = {}
    for s,p,o in triples:
        idx.setdefault(s, {}).setdefault(p, []).append(o)
    return idx

def get1(idx: Dict[str, Dict[str, List[Any]]], s: str, p: str, default=None):
    vals = idx.get(s, {}).get(p, [])
    return vals[0] if vals else default

# Load graphs
with open(os.path.join(BASE, "static.ttl"), encoding="utf-8") as f:
    _, S_tr = parse_ttl(f.read())
with open(os.path.join(BASE, "dynamic.ttl"), encoding="utf-8") as f:
    _, D_tr = parse_ttl(f.read())

S = index_triples(S_tr)
D = index_triples(D_tr)

# -----------------------------------------------------------------------------
# Agent → Driver (Ershov mixed computation)
# -----------------------------------------------------------------------------
def norm(xs: List[float]) -> List[float]:
    lo, hi = min(xs), max(xs)
    if hi - lo < 1e-9:
        return [0.0]*len(xs)
    return [(x-lo)/(hi-lo) for x in xs]

def make_driver(S):
    # Static policy
    wR = float(get1(S, EX+"policy", EX+"wRisk",    0.50))
    wC = float(get1(S, EX+"policy", EX+"wCost",    0.20))
    wB = float(get1(S, EX+"policy", EX+"wBenefit", 0.30))

    # Static constraints and defaults (for thresholds etc.)
    minEgfr = {
        EX+"Metformin": float(get1(S, EX+"Metformin", EX+"minEgfr", 30)),
        EX+"SGLT2":     float(get1(S, EX+"SGLT2",     EX+"minEgfr", 30)),
    }

    # Base benefit lookup by condition-specific predicates (illustrative)
    benefit_predicates = {
        "HTN": (EX+"benefitHTN", EX+"HTN"),
        "T2D": (EX+"benefitT2D", EX+"T2D"),
        # pain/infection not in patient goals here but included for completeness
        "Pain": (EX+"benefitPain", None),
        "Infection": (EX+"benefitInfection", None),
    }

    def compute_metrics(D) -> Dict[str, Any]:
        # Patient state
        pt = EX+"pt"
        pregnant = bool(get1(D, pt, EX+"pregnant", False))
        asthma   = bool(get1(D, pt, EX+"asthma",   False))
        gfr      = float(get1(D, pt, EX+"egfr",    90))
        has_cond = set(D.get(pt, {}).get(EX+"hasCondition", []))

        # Candidate list
        cand_root = EX+"cand"
        candidates = [i for i in D.get(cand_root, {}).get(EX+"consider", [])]

        # Cost table
        def cost_of(i: str) -> float:
            return float(get1(S, i, EX+"costPerMonth", 10))

        # Aggregate "benefit signal" out of static baseBenefit terms for present conditions
        def benefit_of(i: str) -> float:
            total = 0.0
            if EX+"HTN" in has_cond:
                total += float(get1(S, i, EX+"benefitHTN", 0.0))
            if EX+"T2D" in has_cond:
                total += float(get1(S, i, EX+"benefitT2D", 0.0))
            # (others can be added as needed)
            return total

        # Hard contraindications → infeasible
        def infeasible(i: str) -> bool:
            # pregnancy contexts
            if (i == EX+"ACEi" or i == EX+"SGLT2") and pregnant:
                return True
            # nonselective BB with asthma
            if i == EX+"BB_nonselect" and bool(get1(D, pt, EX+"asthma", False)):
                return True
            # eGFR thresholds
            if i in minEgfr and gfr < minEgfr[i]:
                return True
            # penicillin allergy
            if i == EX+"Amoxicillin" and bool(get1(D, pt, EX+"allergyPenicillin", False)):
                return True
            return False

        # Soft risk proxy (0..1): accumulate flags; here a simple additive proxy
        def risk_proxy(i: str) -> float:
            r = 0.0
            if i == EX+"ACEi" and pregnant: r += 1.0
            if i == EX+"SGLT2" and pregnant: r += 1.0
            if i == EX+"BB_nonselect" and asthma: r += 0.7
            if i in minEgfr and gfr < (minEgfr[i] + 10): r += 0.5  # near threshold
            # allergy:
            if i == EX+"Amoxicillin" and bool(get1(D, pt, EX+"allergyPenicillin", False)): r += 1.0
            return r

        metrics = {}
        for i in candidates:
            metrics[i] = {
                "intervention": i,
                "feasible": (not infeasible(i)),
                "benefit_raw": benefit_of(i),       # higher is better
                "risk_raw": risk_proxy(i),          # higher is worse
                "cost_raw": cost_of(i),             # higher is worse
                "pregnant": pregnant,
                "asthma": asthma,
                "egfr": gfr,
                "has_conditions": list(has_cond),
            }
        return metrics

    def score_and_rank(D):
        M = compute_metrics(D)
        items = list(M.keys())
        assert items, "No candidate interventions found."

        feas = [i for i in items if M[i]["feasible"]]
        assert feas, "No feasible interventions — all hard-contraindicated in this context."

        # Normalize across feasible set
        benefit_vals = [M[i]["benefit_raw"] for i in feas]
        risk_vals    = [M[i]["risk_raw"]    for i in feas]
        cost_vals    = [M[i]["cost_raw"]    for i in feas]

        # Normalizers
        def norm(vals: List[float]) -> Dict[str, float]:
            lo, hi = min(vals), max(vals)
            if hi - lo < 1e-9:
                return {k: 0.0 for k in feas}
            return {feas[j]: (vals[j]-lo)/(hi-lo) for j in range(len(feas))}

        bN = norm(benefit_vals)  # 0..1 (higher better)
        rN = norm(risk_vals)     # 0..1 (higher worse)
        cN = norm(cost_vals)     # 0..1 (higher worse)

        results = []
        for i in items:
            m = M[i]
            if not m["feasible"]:
                score = float("+inf")
                note = "infeasible (hard contraindication)"
                bn = rn = cn = None
            else:
                bn = bN[i]; rn = rN[i]; cn = cN[i]
                score = (wR*rn + wC*cn + wB*(1.0 - bn))
                reasons = []
                if bn >= 0.7: reasons.append("high expected benefit")
                if rn <= 0.2: reasons.append("low risk proxy")
                if cn <= 0.2: reasons.append("low cost")
                if m["pregnant"]: reasons.append("pregnancy context")
                if m["asthma"]: reasons.append("asthma context")
                if m["egfr"] < 45: reasons.append("reduced eGFR")
                note = "; ".join(reasons)
            results.append({
                "intervention": i, "feasible": m["feasible"], "score": score, "note": note,
                "benefitN": bn, "riskN": rn, "costN": cn, **m
            })

        ranked = sorted(results, key=lambda x: x["score"])
        return ranked, results, {"wRisk": wR, "wCost": wC, "wBenefit": wB}

    return score_and_rank

# Build specialized driver
driver = make_driver(S)

# -----------------------------------------------------------------------------
# Execute
# -----------------------------------------------------------------------------
ranked, all_results, weights = driver(D)

# Basic checks (educational guardrails; NOT clinical validation)
feasible = [r for r in ranked if r["feasible"]]
assert feasible, "No feasible interventions after scoring."

best = feasible[0]

# Monotonicity wrt safety weight: increasing wRisk should not pick a *riskier* best option
def rerun_with_safety_bonus(delta=0.20):
    # clone S and bump wRisk; renormalize other weights proportionally (sum ~ 1)
    S_mod = {k:{kk:list(vv) for kk,vv in props.items()} for k,props in S.items()}
    S_mod[EX+"policy"][EX+"wRisk"] = [min(0.99, weights["wRisk"] + delta)]
    remain = 1.0 - S_mod[EX+"policy"][EX+"wRisk"][0]
    others = [weights["wCost"], weights["wBenefit"]]
    tot = sum(others)
    scale = remain/tot if tot>0 else 0.0
    S_mod[EX+"policy"][EX+"wCost"]    = [weights["wCost"]*scale]
    S_mod[EX+"policy"][EX+"wBenefit"] = [weights["wBenefit"]*scale]
    ranked2, _, _ = make_driver(S_mod)(D)
    best2 = [r for r in ranked2 if r["feasible"]][0]
    return best2

best_after = rerun_with_safety_bonus(0.20)
# If both have normalized risk values, enforce non-increase
if best.get("riskN") is not None and best_after.get("riskN") is not None:
    assert best_after["riskN"] <= best["riskN"] + 1e-9, \
        "Increasing safety weight should not produce a riskier chosen intervention."

# -----------------------------------------------------------------------------
# Emit recommendations.csv + machine- and human-readable traces
# -----------------------------------------------------------------------------
csv_path = os.path.join(BASE, "recommendations.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow([
        "rank","intervention","feasible",
        "benefitN","riskN","costN","score","note",
        "benefit_raw","risk_raw","cost_raw","egfr","pregnant","asthma","conditions"
    ])
    for i,r in enumerate(ranked, start=1):
        w.writerow([
            i, r["intervention"].split("#")[-1], r["feasible"],
            (None if r["benefitN"] is None else f"{r['benefitN']:.3f}"),
            (None if r["riskN"]    is None else f"{r['riskN']:.3f}"),
            (None if r["costN"]    is None else f"{r['costN']:.3f}"),
            (None if r["score"] == float("+inf") else f"{r['score']:.3f}"),
            r["note"],
            f"{r['benefit_raw']:.2f}", f"{r['risk_raw']:.2f}", f"{r['cost_raw']:.2f}",
            f"{r['egfr']:.1f}", r["pregnant"], r["asthma"], "|".join([c.split("#")[-1] for c in r["has_conditions"]]),
        ])

# Machine-readable trace (JSON)
trace = {
    "weights": weights,
    "chosen": best["intervention"],
    "patient": {
        "egfr": best["egfr"],
        "pregnant": best["pregnant"],
        "asthma": best["asthma"],
        "conditions": [c for c in best["has_conditions"]],
    },
    "interventions": [
        {
            "intervention": r["intervention"],
            "feasible": r["feasible"],
            "benefit_raw": r["benefit_raw"],
            "risk_raw": r["risk_raw"],
            "cost_raw": r["cost_raw"],
            "benefitN": r["benefitN"],
            "riskN": r["riskN"],
            "costN": r["costN"],
            "score": (None if r["score"] == float("+inf") else r["score"]),
            "note": r["note"]
        } for r in ranked
    ]
}
with open(os.path.join(BASE, "trace.json"), "w", encoding="utf-8") as jf:
    json.dump(trace, jf, indent=2)

# Human-readable “Reason why”
with open(os.path.join(BASE, "reason-why.txt"), "w", encoding="utf-8") as tf:
    tf.write("Reason why / explanation summary — ClinicSupport (educational)\n")
    tf.write("----------------------------------------------------------------\n")
    tf.write(f"Weights: wRisk={weights['wRisk']}, wCost={weights['wCost']}, wBenefit={weights['wBenefit']}\n")
    tf.write(f"Chosen intervention: {best['intervention']}\n")
    tf.write(f"Feasible: {best['feasible']}\n")
    if best.get('benefitN') is not None:
        tf.write(f"BenefitN={best['benefitN']:.3f}, RiskN={best['riskN']:.3f}, CostN={best['costN']:.3f}, "
                 f"Score={best['score']:.3f}\n")
    tf.write(f"Patient context: eGFR={best['egfr']:.1f}, pregnant={best['pregnant']}, asthma={best['asthma']}, "
             f"conditions={[c.split('#')[-1] for c in best['has_conditions']]}\n")
    tf.write(f"Notes: {best['note']}\n\n")
    tf.write("All candidates (ranked):\n")
    for i,r in enumerate([x for x in ranked if x["feasible"]], start=1):
        tf.write(f"  {i}. {r['intervention']}  score={r['score']:.3f}  "
                 f"benefitN={r['benefitN']:.3f}  riskN={r['riskN']:.3f}  costN={r['costN']:.3f}\n")

print("ALL TESTS PASSED")
print("Artifacts in:", BASE)
print("CSV:", csv_path)

