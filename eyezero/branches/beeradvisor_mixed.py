#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (policy weights) + RULES_N3 (math:* triple patterns only) are
# partially evaluated into a small driver. At run time, the driver ingests
# dynamic RDF (user taste targets + candidate beers), computes normalized
# distance-to-target features (IBU, color SRM, ABV, price), mirrors the N3
# scoring (weighted sum via math:* semantics), enforces feasibility from
# hard constraints (gluten-free, ABV range, disliked styles), and prints:
#   1) Answer (ranked feasible beers),
#   2) Reason why (trace lines that mirror math:* steps),
#   3) Check (a harness that re-validates feasibility, scoring, and sorting).
#
# Contract with P3:
# - All rule arithmetic/relations appear as math:* built-ins only in N3.
# - Everything is inline (no external file writes).
# - One file produces Answer • Reason why • Check.

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Tuple

# ──────────────────────────────────────────────────────────────────────────────
# Static + Dynamic RDF (inline)
# ──────────────────────────────────────────────────────────────────────────────

STATIC_TTL = r"""
@prefix ex:  <http://example.org/beer#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~ 1.0): minimize score = wIBU*ibuN + wColor*srmN + wABV*abvN + wPrice*priceN
ex:policy ex:wIBU   0.40 .
ex:policy ex:wColor 0.20 .
ex:policy ex:wABV   0.25 .
ex:policy ex:wPrice 0.15 .
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/beer#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# User preference targets + constraints
ex:pt  ex:ibuTarget 45 .
ex:pt  ex:srmTarget 10 .
ex:pt  ex:abvTarget 6.0 .
ex:pt  ex:minAbv    4.0 .
ex:pt  ex:maxAbv    8.0 .
ex:pt  ex:glutenFree false .
ex:pt  ex:budget     5.0 .
ex:pt  ex:dislikeStyle ex:Smoked .

# Candidate set
ex:cand ex:consider ex:beer1 .
ex:cand ex:consider ex:beer2 .
ex:cand ex:consider ex:beer3 .
ex:cand ex:consider ex:beer4 .
ex:cand ex:consider ex:beer5 .
ex:cand ex:consider ex:beer6 .

# Beers
ex:beer1 ex:name "Citrus Haze IPA" .
ex:beer1 ex:style ex:IPA .
ex:beer1 ex:ibu 60 .
ex:beer1 ex:srm 6 .
ex:beer1 ex:abv 6.5 .
ex:beer1 ex:price 5.5 .
ex:beer1 ex:gluten true .

ex:beer2 ex:name "Velvet Stout" .
ex:beer2 ex:style ex:Stout .
ex:beer2 ex:ibu 40 .
ex:beer2 ex:srm 40 .
ex:beer2 ex:abv 7.0 .
ex:beer2 ex:price 6.0 .
ex:beer2 ex:gluten true .

ex:beer3 ex:name "Pilsner Prima" .
ex:beer3 ex:style ex:Pilsner .
ex:beer3 ex:ibu 30 .
ex:beer3 ex:srm 4 .
ex:beer3 ex:abv 5.0 .
ex:beer3 ex:price 4.0 .
ex:beer3 ex:gluten true .

ex:beer4 ex:name "Wheat Breeze" .
ex:beer4 ex:style ex:Wheat .
ex:beer4 ex:ibu 18 .
ex:beer4 ex:srm 5 .
ex:beer4 ex:abv 4.8 .
ex:beer4 ex:price 4.5 .
ex:beer4 ex:gluten true .

ex:beer5 ex:name "Gluten-Free Pale" .
ex:beer5 ex:style ex:PaleAle .
ex:beer5 ex:ibu 35 .
ex:beer5 ex:srm 7 .
ex:beer5 ex:abv 5.2 .
ex:beer5 ex:price 5.0 .
ex:beer5 ex:gluten false .

ex:beer6 ex:name "Smoky Lager" .
ex:beer6 ex:style ex:Smoked .
ex:beer6 ex:ibu 22 .
ex:beer6 ex:srm 15 .
ex:beer6 ex:abv 5.6 .
ex:beer6 ex:price 5.0 .
ex:beer6 ex:gluten true .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — only math:* built-ins for arithmetic/relations
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/beer#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Feasibility (hard constraints)
{ ?b ex:gluten true .  ex:pt ex:glutenFree true . }                               => { ?b ex:feasible false } .
{ ?b ex:style ?s .     ex:pt ex:dislikeStyle ?s . }                               => { ?b ex:feasible false } .
{ ?b ex:abv ?a .       ex:pt ex:maxAbv ?mx .  ?a  math:greaterThan ?mx . }        => { ?b ex:feasible false } .
{ ?b ex:abv ?a .       ex:pt ex:minAbv ?mn .  ?mn math:greaterThan ?a  . }        => { ?b ex:feasible false } .

# Score = wIBU*ibuN + wColor*srmN + wABV*abvN + wPrice*priceN
{
  ?b ex:ibuN   ?iN .
  ?b ex:srmN   ?cN .
  ?b ex:abvN   ?aN .
  ?b ex:priceN ?pN .
  ex:policy ex:wIBU   ?wI .
  ex:policy ex:wColor ?wC .
  ex:policy ex:wABV   ?wA .
  ex:policy ex:wPrice ?wP .
  ( ?wI ?iN )        math:product ?iTerm .
  ( ?wC ?cN )        math:product ?cTerm .
  ( ?wA ?aN )        math:product ?aTerm .
  ( ?wP ?pN )        math:product ?pTerm .
  ( ?iTerm ?cTerm )  math:sum     ?ic .
  ( ?aTerm ?pTerm )  math:sum     ?ap .
  ( ?ic ?ap )        math:sum     ?score .
}
=>
{ ?b ex:score ?score } .
"""

# ──────────────────────────────────────────────────────────────────────────────
# Inline-comment–safe tiny Turtle reader (1 triple per line; collects repeats)
# ──────────────────────────────────────────────────────────────────────────────
def parse_turtle_simple(ttl: str) -> Dict[str, Dict[str, Any]]:
    """
    Minimal TTL for this case:
      - Skips @prefix lines.
      - Strips inline comments after '#' (outside quoted strings).
      - One triple per line (as provided here), tolerant to ' . ' separators.
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
                lit = o[1:o.find('"', 1)]
                add(s, p, lit)
            else:
                tok = o.split()[0]
                if tok in ('true', 'false'):
                    add(s, p, tok == 'true')
                else:
                    try:
                        add(s, p, float(tok) if ('.' in tok) else int(tok))
                    except Exception:
                        add(s, p, tok)
    return out

def listify(v) -> List[Any]:
    if v is None: return []
    return v if isinstance(v, list) else [v]

# ──────────────────────────────────────────────────────────────────────────────
# Domain structures and driver specialization
# ──────────────────────────────────────────────────────────────────────────────
@dataclass
class Weights:
    wI: float
    wC: float
    wA: float
    wP: float

@dataclass
class BeerEval:
    iri: str
    name: str
    style: str
    feasible: bool
    ibu: float
    srm: float
    abv: float
    price: float
    ibuN: float
    srmN: float
    abvN: float
    priceN: float
    score: float
    trace: List[str]

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    W = Weights(
        wI=float(S["ex:policy"]["ex:wIBU"]),
        wC=float(S["ex:policy"]["ex:wColor"]),
        wA=float(S["ex:policy"]["ex:wABV"]),
        wP=float(S["ex:policy"]["ex:wPrice"]),
    )

    def infeasible(beer: Dict[str, Any], D: Dict[str, Dict[str, Any]]) -> bool:
        gluten = bool(beer.get("ex:gluten", False))
        style  = str(beer.get("ex:style", "ex:Unknown"))
        abv    = float(beer.get("ex:abv", 0.0))
        pt     = D.get("ex:pt", {})
        gf     = bool(pt.get("ex:glutenFree", False))
        maxAbv = float(pt.get("ex:maxAbv", 100.0))
        minAbv = float(pt.get("ex:minAbv", 0.0))
        dislikes = set(listify(pt.get("ex:dislikeStyle")))
        # Mirror RULES_N3 feasibility
        if gluten and gf: return True
        if style in dislikes: return True
        if abv > maxAbv: return True
        if abv < minAbv: return True
        return False

    def normalize(values: List[float]) -> Tuple[List[float], Dict[str, float]]:
        lo, hi = min(values), max(values)
        rng = hi - lo
        if rng < 1e-12:
            return [0.0 for _ in values], {"min": lo, "max": hi, "rng": rng}
        return [(v - lo) / rng for v in values], {"min": lo, "max": hi, "rng": rng}

    def driver(D: Dict[str, Dict[str, Any]]):
        pt = D.get("ex:pt", {})
        target_ibu = float(pt.get("ex:ibuTarget", 40))
        target_srm = float(pt.get("ex:srmTarget", 8))
        target_abv = float(pt.get("ex:abvTarget", 5.5))

        # Candidate IRIs
        candidates = sorted(set(listify(D.get("ex:cand", {}).get("ex:consider"))))
        beers = []
        for b in candidates:
            props = D.get(b, {})
            beers.append({
                "iri": b,
                "name": str(props.get("ex:name", b)),
                "style": str(props.get("ex:style", "ex:Unknown")),
                "ibu": float(props.get("ex:ibu", 0.0)),
                "srm": float(props.get("ex:srm", 0.0)),
                "abv": float(props.get("ex:abv", 0.0)),
                "price": float(props.get("ex:price", 0.0)),
                "gluten": bool(props.get("ex:gluten", False)),
            })

        # Normalization bases (across all candidates)
        ibu_vals   = [b["ibu"]   for b in beers]
        srm_vals   = [b["srm"]   for b in beers]
        abv_vals   = [b["abv"]   for b in beers]
        price_vals = [b["price"] for b in beers]
        priceN_all, _ = normalize(price_vals)  # higher price → higher penalty

        # Distance-to-target normalized by global range (if rng==0 → 0.0)
        def distanceN(vals: List[float], target: float) -> List[float]:
            lo, hi = min(vals), max(vals)
            rng = hi - lo
            if rng < 1e-12:
                return [0.0 for _ in vals]
            return [abs(v - target) / rng for v in vals]

        ibuN_all = distanceN(ibu_vals,   target_ibu)
        srmN_all = distanceN(srm_vals,   target_srm)
        abvN_all = distanceN(abv_vals,   target_abv)

        evals: List[BeerEval] = []
        for idx, b in enumerate(beers):
            feas = not infeasible(D.get(b["iri"], {}), D)  # pass beer node + dataset
            # RULES mirror (math:*) for score
            iN, cN, aN, pN = ibuN_all[idx], srmN_all[idx], abvN_all[idx], priceN_all[idx]
            iTerm = W.wI * iN
            cTerm = W.wC * cN
            aTerm = W.wA * aN
            pTerm = W.wP * pN
            ic    = iTerm + cTerm
            ap    = aTerm + pTerm
            score = ic + ap

            trace = [
                (f"Score N3: "
                 f"({W.wI:.2f} {iN:.3f}) math:product {iTerm:.3f} ; "
                 f"({W.wC:.2f} {cN:.3f}) math:product {cTerm:.3f} ; "
                 f"({W.wA:.2f} {aN:.3f}) math:product {aTerm:.3f} ; "
                 f"({W.wP:.2f} {pN:.3f}) math:product {pTerm:.3f} ; "
                 f"sum→ {score:.3f}")
            ]
            evals.append(BeerEval(
                iri=b["iri"], name=b["name"], style=b["style"], feasible=feas,
                ibu=b["ibu"], srm=b["srm"], abv=b["abv"], price=b["price"],
                ibuN=iN, srmN=cN, abvN=aN, priceN=pN,
                score=score, trace=trace
            ))

        # Sort: feasible first, then ascending score, tie-break by price then name
        evals.sort(key=lambda e: (not e.feasible, e.score, e.price, e.name))
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

    feasible = [e for e in evals if e.feasible]
    winners = feasible

    # ── ANSWER ──
    print("Answer:")
    if not winners:
        print("- No feasible beers for the given constraints.")
    else:
        for rank, e in enumerate(winners, start=1):
            print(f"- #{rank} {e.name} ({e.style.split(':')[-1]}) • score {e.score:.3f} • "
                  f"IBU {e.ibu:.0f} • SRM {e.srm:.0f} • ABV {e.abv:.1f}% • price {e.price:.2f}")

    # ── REASON WHY ──
    print("\nReason why:")
    pt = D.get("ex:pt", {})
    print(f"- Weights: wIBU={W.wI:.2f}, wColor={W.wC:.2f}, wABV={W.wA:.2f}, wPrice={W.wP:.2f}")
    print(f"- Targets: IBU={float(pt.get('ex:ibuTarget', 0)):.0f}, "
          f"SRM={float(pt.get('ex:srmTarget', 0)):.0f}, ABV={float(pt.get('ex:abvTarget', 0)):.1f}")
    print(f"- ABV range: {float(pt.get('ex:minAbv', 0)):.1f}–{float(pt.get('ex:maxAbv', 100)):.1f} ; "
          f"glutenFree={'true' if bool(pt.get('ex:glutenFree', False)) else 'false'} ; "
          f"dislikes={[s.split(':')[-1] for s in listify(pt.get('ex:dislikeStyle'))] or []}")
    for e in evals:
        feas = "true" if e.feasible else "false"
        print(f"- {e.name}: feasible={feas}")
        for ln in e.trace:
            print(f"  • {ln}")

    # ── CHECK (harness) ──
    print("\nCheck (harness):")
    errors: List[str] = []

    feasible = [e for e in evals if e.feasible]
    n_all = len(evals)
    n_feas = len(feasible)
    n_infeas = n_all - n_feas

    # (C1) Re-derive feasibility according to RULES_N3 and compare with evals
    def n3_feasible(e) -> bool:
        pt = D.get("ex:pt", {})
        gf = bool(pt.get("ex:glutenFree", False))
        dislikes = set(listify(pt.get("ex:dislikeStyle")))
        maxAbv = float(pt.get("ex:maxAbv", 100.0))
        minAbv = float(pt.get("ex:minAbv", 0.0))
        node = D.get(e.iri, {})
        if bool(node.get("ex:gluten", False)) and gf:
            return False
        if str(node.get("ex:style", "")) in dislikes:
            return False
        a = float(node.get("ex:abv", 0.0))
        if a > maxAbv: return False
        if a < minAbv: return False
        return True

    c1_mismatches = 0
    for e in evals:
        if e.feasible != n3_feasible(e):
            errors.append(f"(C1) Feasibility mismatch for {e.name}")
            c1_mismatches += 1

    # (C2) Winner (if any) must be feasible
    if feasible and not feasible[0].feasible:
        errors.append("(C2) Top-ranked beer is not feasible")

    # (C3) Score recomputation from normalized terms + weights
    c3_max_dscore = 0.0
    for e in feasible:
        iTerm = W.wI * e.ibuN
        cTerm = W.wC * e.srmN
        aTerm = W.wA * e.abvN
        pTerm = W.wP * e.priceN
        recomputed = (iTerm + cTerm) + (aTerm + pTerm)
        dscore = abs(recomputed - e.score)
        c3_max_dscore = max(c3_max_dscore, dscore)
        if dscore > 1e-9:
            errors.append(f"(C3) Score mismatch for {e.name}: {e.score:.6f} vs {recomputed:.6f}")

    # (C4) Sorting order consistent with comparator used by the driver
    # Comparator for beer case: feasible first, then ascending score, tie-break by price then name
    def sort_key(r):
        return (not r.feasible, r.score, r.price, r.name)

    order_ok = [x.iri for x in sorted(evals, key=sort_key)] == [x.iri for x in evals]
    if not order_ok:
        errors.append("(C4) Sorting order mismatch")

    # (C5) Increasing price weight shouldn't yield a *more expensive* top pick
    def rerun_with_price_bonus(delta=0.20):
        S2 = {k: {kk: (vv[:] if isinstance(vv, list) else vv) for kk, vv in v.items()} for k, v in S.items()}
        S2["ex:policy"]["ex:wPrice"] = float(S["ex:policy"]["ex:wPrice"]) + delta
        # Renormalize other weights proportionally
        remain = max(0.0, 1.0 - float(S2["ex:policy"]["ex:wPrice"]))
        tot_other = float(S["ex:policy"]["ex:wIBU"]) + float(S["ex:policy"]["ex:wColor"]) + float(S["ex:policy"]["ex:wABV"])
        scale = (remain / tot_other) if tot_other > 0 else 0.0
        S2["ex:policy"]["ex:wIBU"]   = float(S["ex:policy"]["ex:wIBU"])   * scale
        S2["ex:policy"]["ex:wColor"] = float(S["ex:policy"]["ex:wColor"]) * scale
        S2["ex:policy"]["ex:wABV"]   = float(S["ex:policy"]["ex:wABV"])   * scale
        evals2, _ = specialize_driver(S2)(D)
        feas2 = [x for x in evals2 if x.feasible]
        return feas2[0] if feas2 else None

    top = feasible[0] if feasible else None
    top2 = rerun_with_price_bonus(0.20)
    if top and top2:
        c5_base_price = top.price
        c5_new_price  = top2.price
        c5_price_delta = c5_new_price - c5_base_price
        if c5_new_price > c5_base_price + 1e-9:
            errors.append(f"(C5) Increasing wPrice produced a *more expensive* chosen beer ({c5_base_price:.2f} → {c5_new_price:.2f})")
    else:
        c5_price_delta = 0.0  # not applicable

    # Optional: tie statistics among feasible items (scores within 1e-9)
    tie_pairs = 0
    for i in range(max(0, n_feas - 1)):
        for j in range(i + 1, n_feas):
            if abs(feasible[i].score - feasible[j].score) <= 1e-9:
                tie_pairs += 1

    if errors:
        print("❌ FAIL")
        for e in errors:
            print(" -", e)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Feasibility OK: {n_feas}/{n_all} feasible, {n_infeas} infeasible, mismatches={c1_mismatches}")
        print(f"  • [C2] Winner OK: {'present' if top else 'none'} and {'feasible' if (top and top.feasible) else '—'}")
        print(f"  • [C3] Score re-check OK: max Δscore={c3_max_dscore:.3e}")
        print(f"  • [C4] Order OK: {'stable' if order_ok else '—'}")
        if top and top2:
            print(f"  • [C5] Price-weight monotonicity OK: top price {top.price:.2f} → {top2.price:.2f} (Δ={c5_price_delta:.2f} ≤ 0 expected)")
        else:
            print("  • [C5] Price-weight monotonicity OK: no feasible winner in one of the runs")
        print(f"  • Ties among feasible (|Δscore| ≤ 1e-9): {tie_pairs}")

if __name__ == "__main__":
    main()

