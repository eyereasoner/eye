#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (policy, base benefits, thresholds) + RULES_N3 (math:* triple
# patterns only) are partially evaluated into a compact Driver. At run time,
# the Driver ingests dynamic RDF (patient context and candidate interventions),
# computes normalized benefit/risk/cost features, mirrors the N3 scoring
# (weighted sum with math:* semantics), enforces feasibility from hard
# contraindications, and prints:
#   1) Answer (ranked feasible interventions),
#   2) Reason why (trace lines that mirror math:* steps),
#   3) Check (harness that revalidates feasibility, score math, and ordering).
#
# Contract with P3:
# - All arithmetic/relations in RULES_N3 use math:* built-ins only.
# - Everything is inline (no external file writes).
# - One file produces Answer • Reason why • Check.
from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Tuple

# ──────────────────────────────────────────────────────────────────────────────
# Static + Dynamic RDF (inline)
# ──────────────────────────────────────────────────────────────────────────────
STATIC_TTL = r"""
@prefix ex:  <http://example.org/clinic#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~ 1.0): minimize score = wRisk*riskN + wCost*costN + wBenefit*(1 - benefitN)
ex:policy ex:wRisk    0.50 .
ex:policy ex:wCost    0.20 .
ex:policy ex:wBenefit 0.30 .

# Condition & context vocabulary (examples)
ex:HTN a ex:Condition .
ex:T2D a ex:Condition .
ex:CKD a ex:Condition .
ex:Pregnancy a ex:Context .

# Interventions
ex:ACEi a ex:Intervention .      # ACE inhibitor (e.g., lisinopril)
ex:BB_nonselect a ex:Intervention .   # non-selective beta-blocker
ex:Metformin a ex:Intervention .
ex:SGLT2 a ex:Intervention .
ex:Amoxicillin a ex:Intervention .
ex:NSAID a ex:Intervention .

# Base benefit proxies per condition (0..1) — illustrative
ex:ACEi ex:benefitHTN 0.85 .
ex:BB_nonselect ex:benefitHTN 0.60 .
ex:Metformin ex:benefitT2D 0.85 .
ex:SGLT2 ex:benefitT2D 0.75 .
ex:NSAID ex:benefitPain 0.70 .
ex:Amoxicillin ex:benefitInfection 0.80 .

# Monthly cost (arbitrary units)
ex:ACEi ex:costPerMonth 4 .
ex:BB_nonselect ex:costPerMonth 3 .
ex:Metformin ex:costPerMonth 2 .
ex:SGLT2 ex:costPerMonth 45 .
ex:Amoxicillin ex:costPerMonth 5 .
ex:NSAID ex:costPerMonth 1 .

# Hard contraindications / thresholds (illustrative)
# Pregnancy: avoid ACE inhibitors and SGLT2
ex:ACEi ex:contraInContext ex:Pregnancy .
ex:SGLT2 ex:contraInContext ex:Pregnancy .

# Asthma: avoid nonselective beta-blockers
ex:BB_nonselect ex:contraIfAsthma true .

# Renal function thresholds (ml/min/1.73m2)
ex:Metformin ex:minEgfr 30 .
ex:SGLT2 ex:minEgfr 30 .

# Allergy: Amoxicillin contraindicated if penicillin allergy
ex:Amoxicillin ex:contraIfAllergyPenicillin true .
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/clinic#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Patient context
ex:pt ex:ageYears 54 .
ex:pt ex:pregnant false .
ex:pt ex:asthma false .
ex:pt ex:egfr 42 .
ex:pt ex:allergyPenicillin true .

# Presenting problems / goals
ex:pt ex:hasCondition ex:HTN .
ex:pt ex:hasCondition ex:T2D .
ex:pt ex:hasCondition ex:CKD .

# Candidate interventions under consideration
ex:cand ex:consider ex:ACEi .
ex:cand ex:consider ex:BB_nonselect .
ex:cand ex:consider ex:Metformin .
ex:cand ex:consider ex:SGLT2 .
ex:cand ex:consider ex:Amoxicillin .
ex:cand ex:consider ex:NSAID .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — only math:* built-ins for arithmetic/relations
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/clinic#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Feasibility (hard contraindications)
{ ?i ex:contraInContext ex:Pregnancy . ?pt ex:pregnant true . } => { ?i ex:feasible false } .
{ ?i ex:contraIfAsthma true . ?pt ex:asthma true . }           => { ?i ex:feasible false } .
{ ?i ex:contraIfAllergyPenicillin true . ?pt ex:allergyPenicillin true . } => { ?i ex:feasible false } .
{ ?i ex:minEgfr ?thr . ?pt ex:egfr ?gfr . ?thr math:greaterThan ?gfr . }    => { ?i ex:feasible false } .

# Score = wRisk*riskN + wCost*costN + wBenefit*(1 - benefitN)
{
  ?i ex:riskN ?rN .
  ?i ex:costN ?cN .
  ?i ex:benefitN ?bN .
  ex:policy ex:wRisk ?wR .
  ex:policy ex:wCost ?wC .
  ex:policy ex:wBenefit ?wB .
  ( 1 ?bN ) math:difference ?invB .
  ( ?wR ?rN ) math:product ?rTerm .
  ( ?wC ?cN ) math:product ?cTerm .
  ( ?wB ?invB ) math:product ?bTerm .
  ( ?rTerm ?cTerm ) math:sum ?rc .
  ( ?rc ?bTerm ) math:sum ?score .
}
=> { ?i ex:score ?score } .
"""

# ──────────────────────────────────────────────────────────────────────────────
# Tiny Turtle reader (inline-comment–safe; collects repeated predicates)
# ──────────────────────────────────────────────────────────────────────────────
def parse_turtle_simple(ttl: str) -> Dict[str, Dict[str, Any]]:
    out: Dict[str, Dict[str, Any]] = {}

    def strip_inline_comment(line: str) -> str:
        in_str = False
        esc = False
        buf = []
        for ch in line:
            if ch == '"' and not esc:
                in_str = not in_str
            if ch == '#' and not in_str:
                break
            buf.append(ch)
            if esc:
                esc = False
            elif ch == '\\':
                esc = True
        return ''.join(buf)

    def add(s: str, p: str, o: Any):
        slot = out.setdefault(s, {})
        if p not in slot:
            slot[p] = o
        else:
            slot[p] = slot[p] + [o] if isinstance(slot[p], list) else [slot[p], o]

    for raw in ttl.splitlines():
        if raw.lstrip().startswith('@prefix'):
            continue
        line = strip_inline_comment(raw).strip()
        if not line or line.startswith('#'):
            continue
        for stmt in [s.strip() for s in line.split(' . ') if s.strip()]:
            if stmt.endswith('.'):
                stmt = stmt[:-1].strip()
            if not stmt:
                continue
            parts = stmt.split(None, 2)
            if len(parts) < 3:
                continue
            s, p, o = parts[0], parts[1], parts[2].strip()
            if o.startswith('"'):
                add(s, p, o[1:o.find('"', 1)])
            else:
                tok = o.split()[0]
                if tok in ('true', 'false'):
                    add(s, p, tok == 'true')
                else:
                    try:
                        add(s, p, float(tok) if '.' in tok else int(tok))
                    except Exception:
                        add(s, p, tok)
    return out

def listify(v) -> List[Any]:
    if v is None: return []
    return v if isinstance(v, list) else [v]

# ──────────────────────────────────────────────────────────────────────────────
# Domain and driver
# ──────────────────────────────────────────────────────────────────────────────
@dataclass
class Weights:
    wRisk: float
    wCost: float
    wBenefit: float

@dataclass
class IntervEval:
    iri: str
    name: str
    feasible: bool
    cost: float
    benefit_raw: float
    risk_raw: float
    # normalized features
    riskN: float
    costN: float
    benefitN: float
    score: float
    trace: List[str]

def clamp(x, lo, hi): return lo if x < lo else hi if x > hi else x

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    W = Weights(
        wRisk=float(S["ex:policy"]["ex:wRisk"]),
        wCost=float(S["ex:policy"]["ex:wCost"]),
        wBenefit=float(S["ex:policy"]["ex:wBenefit"]),
    )

    # Lookup helpers on static
    def s_get(i: str, p: str, default=0.0):
        return float(S.get(i, {}).get(p, default)) if isinstance(S.get(i, {}).get(p, default), (int, float)) else default

    # Patient and conditions are dynamic; feasibility uses both
    def driver(D: Dict[str, Dict[str, Any]]):
        pt = D.get("ex:pt", {})
        pregnant = bool(pt.get("ex:pregnant", False))
        asthma = bool(pt.get("ex:asthma", False))
        egfr = float(pt.get("ex:egfr", 0.0))
        allergyPen = bool(pt.get("ex:allergyPenicillin", False))
        conds = set(listify(pt.get("ex:hasCondition")))

        cands = [str(x) for x in listify(D["ex:cand"]["ex:consider"])]
        rows = []
        for iri in cands:
            # feasibility flags
            infeasible = False
            if pregnant and ("ex:contraInContext" in S.get(iri, {})):
                ctx = listify(S[iri]["ex:contraInContext"])
                if "ex:Pregnancy" in ctx:
                    infeasible = True
            if asthma and bool(S.get(iri, {}).get("ex:contraIfAsthma", False)):
                infeasible = True
            thr = S.get(iri, {}).get("ex:minEgfr", None)
            if thr is not None and isinstance(thr, (int, float)) and egfr < float(thr):
                infeasible = True
            if allergyPen and bool(S.get(iri, {}).get("ex:contraIfAllergyPenicillin", False)):
                infeasible = True

            # cost
            cost = s_get(iri, "ex:costPerMonth", 0.0)

            # benefit proxy: sum per-condition benefits present (cap at 1.0)
            benefit = 0.0
            if "ex:HTN" in conds:
                benefit += s_get(iri, "ex:benefitHTN", 0.0)
            if "ex:T2D" in conds:
                benefit += s_get(iri, "ex:benefitT2D", 0.0)
            if "ex:Pain" in conds:
                benefit += s_get(iri, "ex:benefitPain", 0.0)
            if "ex:Infection" in conds:
                benefit += s_get(iri, "ex:benefitInfection", 0.0)
            benefit = clamp(benefit, 0.0, 1.0)

            # risk proxy (illustrative): 0 for feasible; 1 for any hard flag (those are infeasible already)
            risk = 0.0 if not infeasible else 1.0

            rows.append({
                "iri": iri,
                "name": iri.split("#")[-1],
                "feasible": not infeasible,
                "cost": cost,
                "benefit_raw": benefit,
                "risk_raw": risk,
            })

        # normalization (min-max, smaller better; risk already 0/1 so it may be all zeros)
        def minmax(vals):
            lo, hi = min(vals), max(vals)
            if hi - lo < 1e-12:
                return [0.0]*len(vals), (lo, hi)
            return [(v - lo)/(hi - lo) for v in vals], (lo, hi)

        costN, _ = minmax([r["cost"] for r in rows])
        # For benefit, we prefer higher benefit → lower normalized. We can normalize benefit and keep as is,
        # scoring uses (1 - benefitN).
        benefitN, _ = minmax([r["benefit_raw"] for r in rows])
        riskN, _ = minmax([r["risk_raw"] for r in rows])

        evals: List[IntervEval] = []
        for i, r in enumerate(rows):
            invB = 1.0 - benefitN[i]
            rTerm = W.wRisk * riskN[i]
            cTerm = W.wCost * costN[i]
            bTerm = W.wBenefit * invB
            rc = rTerm + cTerm
            score = rc + bTerm

            trace = [
                f"Feasibility flags → feasible={str(r['feasible']).lower()}",
                f"N3 score: (1 {benefitN[i]:.3f}) math:difference invB={invB:.3f}; "
                f"({W.wRisk:.2f} {riskN[i]:.3f}) math:product {rTerm:.3f}; "
                f"({W.wCost:.2f} {costN[i]:.3f}) math:product {cTerm:.3f}; "
                f"({W.wBenefit:.2f} {invB:.3f}) math:product {bTerm:.3f}; "
                f"({rTerm:.3f} {cTerm:.3f}) math:sum {rc:.3f}; "
                f"({rc:.3f} {bTerm:.3f}) math:sum {score:.3f}"
            ]

            evals.append(IntervEval(
                iri=r["iri"], name=r["name"], feasible=r["feasible"],
                cost=r["cost"], benefit_raw=r["benefit_raw"], risk_raw=r["risk_raw"],
                riskN=riskN[i], costN=costN[i], benefitN=benefitN[i],
                score=score, trace=trace
            ))

        # sort: feasible first, then ascending score, tie-break by cost then name
        evals.sort(key=lambda z: (not z.feasible, z.score, z.cost, z.name))
        return evals, W

    return driver

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────
def main():
    S = parse_turtle_simple(STATIC_TTL)
    D = parse_turtle_simple(DYNAMIC_TTL)

    driver = specialize_driver(S)
    evals, W = driver(D)

    feas = [e for e in evals if e.feasible]

    # ── ANSWER ──
    print("Answer:")
    if not feas:
        print("- No feasible interventions for this patient context.")
    else:
        for rank, e in enumerate(feas, start=1):
            print(f"- #{rank} {e.name} • cost {e.cost:.0f}/mo • benefit {e.benefit_raw:.2f} • score {e.score:.3f}")

    # ── REASON WHY ──
    print("\nReason why:")
    print(f"- Weights: wRisk={W.wRisk:.2f}, wCost={W.wCost:.2f}, wBenefit={W.wBenefit:.2f}")
    for e in evals:
        print(f"- {e.name}:")
        for ln in e.trace:
            print(f"  • {ln}")

    # ── CHECK (harness) ──
    print("\nCheck (harness):")
    errors: List[str] = []

    # C1: Recompute score exactly from stored normalized fields + weights
    c1_max_d = 0.0
    for e in evals:
        invB = 1.0 - e.benefitN
        rTerm = W.wRisk * e.riskN
        cTerm = W.wCost * e.costN
        bTerm = W.wBenefit * invB
        rc = rTerm + cTerm
        recomputed = rc + bTerm
        d = abs(recomputed - e.score)
        c1_max_d = max(c1_max_d, d)
        if d > 1e-12:
            errors.append(f"(C1) Score mismatch for {e.name}: {e.score:.6f} vs {recomputed:.6f}")

    # C2: Feasibility logic re-check
    pt = D.get("ex:pt", {})
    pregnant = bool(pt.get("ex:pregnant", False))
    asthma = bool(pt.get("ex:asthma", False))
    egfr = float(pt.get("ex:egfr", 0.0))
    allergyPen = bool(pt.get("ex:allergyPenicillin", False))

    c2_mismatches = 0
    for e in evals:
        sinfo = S.get(e.iri, {})
        infeasible = False
        if pregnant and ("ex:contraInContext" in sinfo):
            if "ex:Pregnancy" in listify(sinfo["ex:contraInContext"]):
                infeasible = True
        if asthma and bool(sinfo.get("ex:contraIfAsthma", False)):
            infeasible = True
        thr = sinfo.get("ex:minEgfr", None)
        if isinstance(thr, (int, float)) and egfr < float(thr):
            infeasible = True
        if allergyPen and bool(sinfo.get("ex:contraIfAllergyPenicillin", False)):
            infeasible = True
        if (not infeasible) != e.feasible:
            c2_mismatches += 1
            errors.append(f"(C2) Feasibility mismatch for {e.name}")

    # C3: Sorting order check: feasible first, then score, cost, name
    order_ok = [x.iri for x in sorted(evals, key=lambda z: (not z.feasible, z.score, z.cost, z.name))] \
               == [x.iri for x in evals]
    if not order_ok:
        errors.append("(C3) Sorting order mismatch")

    # C4: Cost-weight monotonicity: increasing wCost shouldn't pick a more expensive winner
    def winner_with_wcost(delta: float):
        wC = W.wCost + delta
        remain = max(0.0, 1.0 - wC)
        other_sum = W.wRisk + W.wBenefit
        scale = (remain / other_sum) if other_sum > 1e-12 else 0.0
        wR = W.wRisk * scale
        wB = W.wBenefit * scale

        def new_score(e: IntervEval):
            return (W.wRisk*scale)*e.riskN + (wC)*e.costN + (wB)*(1.0 - e.benefitN)

        reranked = sorted(evals, key=lambda z: (not z.feasible, new_score(z), z.cost, z.name))
        feas2 = [x for x in reranked if x.feasible]
        return feas2[0] if feas2 else None

    top = feas[0] if feas else None
    top2 = winner_with_wcost(0.20)
    if top and top2 and (top2.cost > top.cost + 1e-9):
        errors.append(f"(C4) Increasing wCost produced a more expensive winner ({top.cost:.0f} → {top2.cost:.0f})")

    # Outcome
    if errors:
        print("❌ FAIL")
        for er in errors:
            print(" -", er)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Score re-check OK: max Δ={c1_max_d:.3e}")
        print(f"  • [C2] Feasibility OK: mismatches={c2_mismatches}")
        print(f"  • [C3] Order OK: {'stable' if order_ok else '—'}")
        if top and top2:
            print(f"  • [C4] Cost-weight monotonicity OK: top cost {top.cost:.0f} → {top2.cost:.0f} (expected Δ≤0)")
        else:
            print("  • [C4] Cost-weight monotonicity OK: not applicable")
if __name__ == "__main__":
    main()

