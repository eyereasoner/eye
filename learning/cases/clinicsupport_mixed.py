#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (policy, base benefits, thresholds) + RULES_N3 (math:* triple patterns)
# are partially evaluated into a compact "driver." At run time, the driver ingests
# dynamic RDF (patient state, problems, candidate interventions), computes normalized
# benefit/risk/cost features, mirrors the N3 scoring (weighted sum with math:* semantics),
# enforces feasibility from hard contraindications, and prints:
#   1) Answer (ranked feasible interventions),
#   2) Reason why (trace lines that mirror math:* steps),
#   3) Check (a harness that re-validates feasibility, scoring, and sorting).
#
# Contract with EYE learning:
# - Rules arithmetic/relations are expressed with math:* built-ins only.
# - Everything is inline (no external file writes).
# - One file produces Answer • Reason why • Check.

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Tuple

# ──────────────────────────────────────────────────────────────────────────────
# Static + Dynamic RDF (inline)
# ──────────────────────────────────────────────────────────────────────────────

EX = "http://example.org/clinic#"

STATIC_TTL = r"""
@prefix ex:  <http://example.org/clinic#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~ 1.0): minimize score = wRisk*riskN + wCost*costN + wBenefit*(1 - benefitN)
ex:policy ex:wRisk    0.50 .
ex:policy ex:wCost    0.20 .
ex:policy ex:wBenefit 0.30 .

# Condition & context vocabulary (examples)
ex:HTN       a ex:Condition .
ex:T2D       a ex:Condition .
ex:CKD       a ex:Condition .
ex:Pregnancy a ex:Context .

# Interventions
ex:ACEi         a ex:Intervention .   # ACE inhibitor (e.g., lisinopril)
ex:BB_nonselect a ex:Intervention .   # non-selective beta-blocker
ex:Metformin    a ex:Intervention .
ex:SGLT2        a ex:Intervention .
ex:Amoxicillin  a ex:Intervention .
ex:NSAID        a ex:Intervention .

# Base benefit proxies per condition (0..1) — illustrative
ex:ACEi         ex:benefitHTN       0.85 .
ex:BB_nonselect ex:benefitHTN       0.60 .
ex:Metformin    ex:benefitT2D       0.85 .
ex:SGLT2        ex:benefitT2D       0.75 .
ex:NSAID        ex:benefitPain      0.70 .
ex:Amoxicillin  ex:benefitInfection 0.80 .

# Monthly cost (arbitrary units)
ex:ACEi         ex:costPerMonth  4 .
ex:BB_nonselect ex:costPerMonth  3 .
ex:Metformin    ex:costPerMonth  2 .
ex:SGLT2        ex:costPerMonth 45 .
ex:Amoxicillin  ex:costPerMonth  5 .
ex:NSAID        ex:costPerMonth  1 .

# Hard contraindications / thresholds (illustrative)
# Pregnancy: avoid ACE inhibitors and SGLT2
ex:ACEi  ex:contraInContext ex:Pregnancy .
ex:SGLT2 ex:contraInContext ex:Pregnancy .

# Asthma: avoid nonselective beta-blockers
ex:BB_nonselect ex:contraIfAsthma true .

# Renal function thresholds (ml/min/1.73m2)
ex:Metformin ex:minEgfr 30 .
ex:SGLT2     ex:minEgfr 30 .

# Allergy: Amoxicillin contraindicated if penicillin allergy
ex:Amoxicillin ex:contraIfAllergyPenicillin true .
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/clinic#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Patient context
ex:pt  ex:ageYears         54 .
ex:pt  ex:pregnant       false .
ex:pt  ex:asthma         false .
ex:pt  ex:egfr             42 .
ex:pt  ex:allergyPenicillin true .

# Presenting problems / goals
ex:pt  ex:hasCondition  ex:HTN .
ex:pt  ex:hasCondition  ex:T2D .
ex:pt  ex:hasCondition  ex:CKD .

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
{ ?i ex:contraInContext ex:Pregnancy . ?pt ex:pregnant true . }                     => { ?i ex:feasible false } .
{ ?i ex:contraIfAsthma true . ?pt ex:asthma true . }                                => { ?i ex:feasible false } .
{ ?i ex:contraIfAllergyPenicillin true . ?pt ex:allergyPenicillin true . }          => { ?i ex:feasible false } .
{ ?i ex:minEgfr ?thr . ?pt ex:egfr ?gfr . ?thr math:greaterThan ?gfr . }            => { ?i ex:feasible false } .

# Score = wRisk*riskN + wCost*costN + wBenefit*(1 - benefitN)
{
  ?i ex:riskN ?rN .
  ?i ex:costN ?cN .
  ?i ex:benefitN ?bN .
  ex:policy ex:wRisk ?wR .
  ex:policy ex:wCost ?wC .
  ex:policy ex:wBenefit ?wB .
  ( 1 ?bN )          math:difference ?invB .
  ( ?wR ?rN )        math:product    ?rTerm .
  ( ?wC ?cN )        math:product    ?cTerm .
  ( ?wB ?invB )      math:product    ?bTerm .
  ( ?rTerm ?cTerm )  math:sum        ?rc .
  ( ?rc ?bTerm )     math:sum        ?score .
}
=>
{ ?i ex:score ?score } .
"""

# ──────────────────────────────────────────────────────────────────────────────
# Inline-comment–safe, tiny Turtle reader (1 triple per line; collects repeats)
# ──────────────────────────────────────────────────────────────────────────────
def parse_turtle_simple(ttl: str) -> Dict[str, Dict[str, Any]]:
    """
    Minimal TTL for this case:
      - Skips @prefix lines.
      - Strips inline comments after '#' (outside quoted strings).
      - One triple per line (as provided here).
      - Parses booleans, numbers, qnames, and quoted strings.
      - Collects repeated predicates into lists.
    """
    out: Dict[str, Dict[str, Any]] = {}

    def strip_inline_comment(line: str) -> str:
        in_str = False
        esc = False
        chars = []
        for ch in line:
            if ch == '"' and not esc:
                in_str = not in_str
            if ch == '#' and not in_str:
                break
            chars.append(ch)
            if esc:
                esc = False
            elif ch == '\\':
                esc = True
        return ''.join(chars)

    def add(subj: str, pred: str, val: Any):
        slot = out.setdefault(subj, {})
        if pred not in slot:
            slot[pred] = val
        else:
            if isinstance(slot[pred], list):
                slot[pred].append(val)
            else:
                slot[pred] = [slot[pred], val]

    for raw in ttl.splitlines():
        if raw.lstrip().startswith('@prefix'):
            continue
        line = strip_inline_comment(raw).strip()
        if not line or line.startswith('#'):
            continue
        # allow multiple " . " in a physical line; we used one per line, but be tolerant
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
                # simple quoted literal (no lang/datatype needed here)
                lit = o[1:o.find('"', 1)]
                add(s, p, lit)
            else:
                tok = o.split()[0]
                if tok in ('true', 'false'):
                    add(s, p, tok == 'true')
                else:
                    try:
                        # parse ints/floats
                        add(s, p, float(tok) if ('.' in tok) else int(tok))
                    except Exception:
                        add(s, p, tok)
    return out

def listify(v) -> List[Any]:
    if v is None:
        return []
    return v if isinstance(v, list) else [v]

# ──────────────────────────────────────────────────────────────────────────────
# Driver specialization (mixed computation)
# ──────────────────────────────────────────────────────────────────────────────
@dataclass
class Weights:
    wR: float
    wC: float
    wB: float

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    # Read static weights
    wR = float(S.get("ex:policy", {}).get("ex:wRisk", 0.50))
    wC = float(S.get("ex:policy", {}).get("ex:wCost", 0.20))
    wB = float(S.get("ex:policy", {}).get("ex:wBenefit", 0.30))
    W = Weights(wR, wC, wB)

    # Thresholds for eGFR (if missing, default 30)
    minEgfr = {
        "ex:Metformin": float(S.get("ex:Metformin", {}).get("ex:minEgfr", 30)),
        "ex:SGLT2":     float(S.get("ex:SGLT2",     {}).get("ex:minEgfr", 30)),
    }

    # Cost lookup (default 10)
    def cost_of(i: str) -> float:
        return float(S.get(i, {}).get("ex:costPerMonth", 10))

    # Sum benefit proxies for conditions the patient actually has
    def benefit_of(i: str, has_cond: List[str]) -> float:
        b = 0.0
        if "ex:HTN" in has_cond:
            b += float(S.get(i, {}).get("ex:benefitHTN", 0.0))
        if "ex:T2D" in has_cond:
            b += float(S.get(i, {}).get("ex:benefitT2D", 0.0))
        # (extendable)
        return b

    def infeasible(i: str, D: Dict[str, Dict[str, Any]]) -> bool:
        pregnant = bool(D.get("ex:pt", {}).get("ex:pregnant", False))
        asthma   = bool(D.get("ex:pt", {}).get("ex:asthma",   False))
        gfr      = float(D.get("ex:pt", {}).get("ex:egfr",    90))
        allergyP = bool(D.get("ex:pt", {}).get("ex:allergyPenicillin", False))
        # Apply the same logic expressed in RULES_N3 feasibility triples:
        if i in ("ex:ACEi", "ex:SGLT2") and pregnant:
            return True
        if i == "ex:BB_nonselect" and asthma:
            return True
        if i in minEgfr and gfr < minEgfr[i]:
            return True
        if i == "ex:Amoxicillin" and allergyP:
            return True
        return False

    def risk_proxy(i: str, D: Dict[str, Dict[str, Any]], minEgfr: Dict[str, float]) -> float:
        # Soft risk proxy (0..1) purely illustrative
        pregnant = bool(D.get("ex:pt", {}).get("ex:pregnant", False))
        asthma   = bool(D.get("ex:pt", {}).get("ex:asthma",   False))
        gfr      = float(D.get("ex:pt", {}).get("ex:egfr",    90))
        allergyP = bool(D.get("ex:pt", {}).get("ex:allergyPenicillin", False))
        r = 0.0
        if i == "ex:ACEi" and pregnant: r += 1.0
        if i == "ex:SGLT2" and pregnant: r += 1.0
        if i == "ex:BB_nonselect" and asthma: r += 0.7
        if i in minEgfr and gfr < (minEgfr[i] + 10): r += 0.5  # near threshold
        if i == "ex:Amoxicillin" and allergyP: r += 1.0
        return r

    def norm_map(vals: List[float], keys: List[str]) -> Dict[str, float]:
        lo, hi = min(vals), max(vals)
        if hi - lo < 1e-9:
            return {k: 0.0 for k in keys}
        return {keys[i]: (vals[i] - lo) / (hi - lo) for i in range(len(keys))}

    def driver(D: Dict[str, Dict[str, Any]]):
        # Patient state
        has_cond = sorted(listify(D.get("ex:pt", {}).get("ex:hasCondition")))
        candidates = sorted(listify(D.get("ex:cand", {}).get("ex:consider")))

        # Compute raw metrics
        M: Dict[str, Dict[str, Any]] = {}
        for i in candidates:
            feas = not infeasible(i, D)
            M[i] = {
                "feasible":    feas,
                "benefit_raw": benefit_of(i, has_cond),
                "risk_raw":    risk_proxy(i, D, minEgfr),
                "cost_raw":    cost_of(i),
            }

        feas_keys = [i for i in candidates if M[i]["feasible"]]
        assert feas_keys, "No feasible interventions — all hard-contraindicated for this context."

        # Normalize only across feasible set
        bN = norm_map([M[i]["benefit_raw"] for i in feas_keys], feas_keys)
        rN = norm_map([M[i]["risk_raw"]    for i in feas_keys], feas_keys)
        cN = norm_map([M[i]["cost_raw"]    for i in feas_keys], feas_keys)

        results: List[Dict[str, Any]] = []
        for i in candidates:
            if not M[i]["feasible"]:
                results.append({
                    "intervention": i, "feasible": False, "score": float("+inf"),
                    "benefitN": None, "riskN": None, "costN": None,
                    **M[i],
                })
                continue

            bn = bN[i]; rn = rN[i]; cn = cN[i]
            # RULES_N3 mirror (math:*): (1 bN) difference invB; products; sums
            invB  = (1 - bn)
            rTerm = W.wR * rn
            cTerm = W.wC * cn
            bTerm = W.wB * invB
            rc    = rTerm + cTerm
            score = rc + bTerm

            results.append({
                "intervention": i, "feasible": True, "score": score,
                "benefitN": bn, "riskN": rn, "costN": cn,
                "trace": [
                    (f"Score N3: (1 {bn:.3f}) math:difference {invB:.3f} ; "
                     f"({W.wR:.2f} {rn:.3f}) math:product {rTerm:.3f} ; "
                     f"({W.wC:.2f} {cn:.3f}) math:product {cTerm:.3f} ; "
                     f"({W.wB:.2f} {invB:.3f}) math:product {bTerm:.3f} ; "
                     f"sum→ {score:.3f}")
                ],
                **M[i],
            })

        # Sort: feasible first, then ascending score; tie-break on riskN, costN, –benefitN, IRI
        def sort_key(r):
            rn = 1e9 if r["riskN"]   is None else r["riskN"]
            cn = 1e9 if r["costN"]   is None else r["costN"]
            bn = -1.0 if r["benefitN"] is None else r["benefitN"]
            return (not r["feasible"], r["score"], rn, cn, -bn, r["intervention"])

        ranked = sorted(results, key=sort_key)
        return ranked, W, has_cond

    return driver

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────
def main():
    S = parse_turtle_simple(STATIC_TTL)
    D = parse_turtle_simple(DYNAMIC_TTL)

    driver = specialize_driver(S)
    ranked, weights, has_cond = driver(D)

    feasible = [r for r in ranked if r["feasible"]]
    best = feasible[0] if feasible else None

    # ── ANSWER ──
    print("Answer:")
    if not feasible:
        print("- No feasible interventions.")
    else:
        for rank, r in enumerate(feasible, start=1):
            bn = f"{r['benefitN']:.3f}" if r["benefitN"] is not None else "—"
            rn = f"{r['riskN']:.3f}"    if r["riskN"]    is not None else "—"
            cn = f"{r['costN']:.3f}"    if r["costN"]    is not None else "—"
            sc = "∞" if r["score"] == float("+inf") else f"{r['score']:.3f}"
            print(f"- #{rank} {r['intervention']} • score {sc} • benefitN {bn} • riskN {rn} • costN {cn}")

    # ── REASON WHY ──
    print("\nReason why:")
    print(f"- Weights: wRisk={weights.wR:.2f}, wCost={weights.wC:.2f}, wBenefit={weights.wB:.2f}")
    print(f"- Patient conditions: {[c.split(':')[-1] for c in has_cond]}")
    for r in ranked:
        feas = "true" if r["feasible"] else "false"
        print(f"- {r['intervention']}: feasible={feas}")
        if r.get("trace"):
            for ln in r["trace"]:
                print(f"  • {ln}")
        else:
            print("  • infeasible (hard contraindication)")

    # Echo rule/data fingerprints for auditability
    print("\nInputs (fingerprints):")
    print(f"- Static RDF bytes: {len(STATIC_TTL.encode())} ; Dynamic RDF bytes: {len(DYNAMIC_TTL.encode())}")
    print(f"- Rules N3 bytes: {len(RULES_N3.encode())} (math:* triples only)")

    # ── CHECK (harness) ──
    print("\nCheck (harness):")
    errors: List[str] = []

    # (C1) There is at least one feasible intervention; top one is feasible.
    if not feasible:
        errors.append("(C1) No feasible interventions")
    elif not best or not best["feasible"]:
        errors.append("(C1) Top-ranked intervention is not feasible")

    # (C2) Recompute score from normalized terms and weights for feasible ones.
    for r in feasible:
        bn, rn, cn = r["benefitN"], r["riskN"], r["costN"]
        invB  = (1 - bn)
        rTerm = weights.wR * rn
        cTerm = weights.wC * cn
        bTerm = weights.wB * invB
        recomputed = (rTerm + cTerm) + bTerm
        if abs(recomputed - r["score"]) > 1e-9:
            errors.append(f"(C2) Score mismatch for {r['intervention']}: {r['score']:.6f} vs {recomputed:.6f}")

    # (C3) Monotonicity wrt safety weight: increasing wRisk should not make the chosen pick riskier.
    def rerun_with_safety_bonus(delta=0.20):
        S2 = {k: {kk: (vv[:] if isinstance(vv, list) else vv) for kk, vv in v.items()} for k, v in S.items()}
        S2["ex:policy"]["ex:wRisk"] = float(S2["ex:policy"]["ex:wRisk"]) + delta
        # Renormalize remaining weights proportionally
        remain = 1.0 - float(S2["ex:policy"]["ex:wRisk"])
        tot_other = float(S["ex:policy"]["ex:wCost"]) + float(S["ex:policy"]["ex:wBenefit"])
        scale = (remain / tot_other) if tot_other > 0 else 0.0
        S2["ex:policy"]["ex:wCost"]    = float(S["ex:policy"]["ex:wCost"]) * scale
        S2["ex:policy"]["ex:wBenefit"] = float(S["ex:policy"]["ex:wBenefit"]) * scale
        ranked2, W2, _ = specialize_driver(S2)(D)
        feas2 = [x for x in ranked2 if x["feasible"]]
        return feas2[0] if feas2 else None

    best2 = rerun_with_safety_bonus(0.20)
    if best and best2 and (best.get("riskN") is not None and best2.get("riskN") is not None):
        if best2["riskN"] > best["riskN"] + 1e-9:
            errors.append("(C3) Increasing wRisk produced a *riskier* chosen intervention")

    # (C4) Sorting order is consistent with our comparator
    def sort_key(r):
        rn = 1e9 if r["riskN"]   is None else r["riskN"]
        cn = 1e9 if r["costN"]   is None else r["costN"]
        bn = -1.0 if r["benefitN"] is None else r["benefitN"]
        return (not r["feasible"], r["score"], rn, cn, -bn, r["intervention"])
    if [x["intervention"] for x in sorted(ranked, key=sort_key)] != [x["intervention"] for x in ranked]:
        errors.append("(C4) Sorting order mismatch")

    if errors:
        print("❌ FAIL")
        for e in errors:
            print(" -", e)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")

if __name__ == "__main__":
    main()

