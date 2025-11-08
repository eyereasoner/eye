#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF policy (weights, epsilon exploration, caps, pheromone dynamics) +
# RULES_N3 (math:* triple patterns only) are partially evaluated into a compact
# Driver. At runtime, the Driver ingests dynamic RDF (trails with food/hazard/
# distance, pheromone, capacity, block flags, rain), normalizes features,
# mirrors the N3 scoring (weighted sum), splits ants into exploration+exploitation,
# honors caps/blocks/hazard limits, and updates pheromone with evaporation+deposit.
# The program prints:
#   1) Answer — per-trail ant counts, fractions, expected food, pheromone update,
#   2) Reason why — trace lines mirroring math:* steps/comparisons,
#   3) Check (harness) — revalidates score math, conservation/caps, block rules,
#      pheromone update, and two monotonicity probes.
#
# Contract with P3:
# - All arithmetic/relations in RULES_N3 use math:* built-ins only.
# - Everything is inline (no external file writes).
# - One file produces Answer • Reason why • Check.

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Tuple
import math

# ──────────────────────────────────────────────────────────────────────────────
# Static + Dynamic RDF (inline)
# ──────────────────────────────────────────────────────────────────────────────

STATIC_TTL = r"""
@prefix ex:  <http://example.org/ants#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Colony policy
ex:policy ex:maxAnts        120 .    # total outbound foragers this tick
ex:policy ex:epsilon        0.15 .   # exploration fraction (uniform over operable trails)
ex:policy ex:wFood          0.45 .
ex:policy ex:wPher          0.25 .
ex:policy ex:wSafe          0.20 .   # prefers low hazard
ex:policy ex:wDist          0.10 .   # prefers short distance (uses 1 - distN)
ex:policy ex:maxHazard      0.80 .   # hazard threshold; above → inoperable
ex:policy ex:foodPerAnt     1.00 .   # expected food units per ant at foodN=1.0
ex:policy ex:evapRate       0.20 .   # pheromone evaporates by this fraction
ex:policy ex:depositPerFood 0.30 .   # pheromone units deposited per food unit

# Note: RULES_N3 expresses the math:* relations for invHazard, rain effect,
# effective pheromone, and score composition.

"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/ants#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Environment (this tick)
ex:env ex:rainFactor 0.30 .   # 0..1. Higher → more rain → pheromone effectiveness reduced.

# Colony has trails to consider
ex:colony ex:hasTrail ex:T1 .
ex:colony ex:hasTrail ex:T2 .
ex:colony ex:hasTrail ex:T3 .
ex:colony ex:hasTrail ex:T4 .

# Trails: foodN [0..1], hazardN [0..1], distance_m (>0), pheromone units (≥0),
# per-trail cap (ants), and block flag.
ex:T1 ex:name "Creek"        . ex:T1 ex:foodN 0.9 . ex:T1 ex:hazardN 0.3 . ex:T1 ex:distance_m 120 . ex:T1 ex:pher  4.0 . ex:T1 ex:capAnts 60 . ex:T1 ex:blocked false .
ex:T2 ex:name "Oak Path"     . ex:T2 ex:foodN 0.6 . ex:T2 ex:hazardN 0.5 . ex:T2 ex:distance_m 180 . ex:T2 ex:pher  6.0 . ex:T2 ex:capAnts 50 . ex:T2 ex:blocked false .
ex:T3 ex:name "Fence Corner" . ex:T3 ex:foodN 0.4 . ex:T3 ex:hazardN 0.2 . ex:T3 ex:distance_m  80 . ex:T3 ex:pher  2.0 . ex:T3 ex:capAnts 25 . ex:T3 ex:blocked false .
ex:T4 ex:name "Driveway"     . ex:T4 ex:foodN 0.3 . ex:T4 ex:hazardN 0.9 . ex:T4 ex:distance_m 140 . ex:T4 ex:pher 10.0 . ex:T4 ex:capAnts 40 . ex:T4 ex:blocked true  .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — math:* built-ins only (correct arities)
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/ants#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# R1: invHazard = (1 - hazardN)
{ ?t ex:hazardN ?h . ( 1 ?h ) math:difference ?invH . } => { ?t ex:invHazard ?invH } .

# R2: rainEffect = (1 - rainFactor)
{ ex:env ex:rainFactor ?r . ( 1 ?r ) math:difference ?eff . } => { ex:env ex:rainEffect ?eff } .

# R3: effective pheromone = pher * rainEffect
{ ?t ex:pher ?p . ex:env ex:rainEffect ?eff . ( ?p ?eff ) math:product ?peff . } => { ?t ex:pherEff ?peff } .

# R4: Score composition (normalized features supplied by the driver):
# score = wFood*foodN + wPher*pherN + wSafe*invHazard + wDist*(1 - distN)
{
  ?t ex:foodN ?f .
  ?t ex:pherN ?pn .
  ?t ex:invHazard ?ih .
  ?t ex:invDist ?id .
  ex:policy ex:wFood ?wF .
  ex:policy ex:wPher ?wP .
  ex:policy ex:wSafe ?wS .
  ex:policy ex:wDist ?wD .
  ( ?wF ?f )  math:product ?fTerm .
  ( ?wP ?pn ) math:product ?pTerm .
  ( ?wS ?ih ) math:product ?sTerm .
  ( ?wD ?id ) math:product ?dTerm .
  ( ?fTerm ?pTerm ) math:sum ?fp .
  ( ?sTerm ?dTerm ) math:sum ?sd .
  ( ?fp ?sd )      math:sum ?score .
}
=> { ?t ex:score ?score } .

# R5: Operability gates:
# blocked ⇒ allowedMaxFrac = 0
{ ?t ex:blocked true . } => { ?t ex:allowedMaxFrac 0 } .
# hazardN > maxHazard ⇒ allowedMaxFrac = 0
{ ?t ex:hazardN ?h . ex:policy ex:maxHazard ?mx . ?h math:greaterThan ?mx . } => { ?t ex:allowedMaxFrac 0 } .
# Cap as fraction of total ants: capFrac = capAnts / maxAnts (upper bound)
{ ?t ex:capAnts ?c . ex:policy ex:maxAnts ?M . ( ?c ?M ) math:quotient ?capF . } => { ?t ex:capFrac ?capF } .

# (Evaporation + deposit shown as math:* relations; driver applies the update)
# newPher = oldPher*(1-evapRate) + depositPerFood*foodCollected
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
class TrailEval:
    iri: str
    name: str
    foodN: float
    hazardN: float
    invHazard: float
    distance_m: float
    distN: float
    invDist: float
    pher: float
    pherEff: float
    pherN: float
    score: float
    operable: bool
    capAnts: float
    expl_frac: float
    expl_ants: float
    base_frac: float
    base_ants: float
    ants: float
    food_collected: float
    new_pher: float
    trace: List[str]

def minmax(vals: List[float]) -> Tuple[List[float], Tuple[float, float]]:
    lo, hi = min(vals), max(vals)
    if hi - lo < 1e-12:
        return [0.0]*len(vals), (lo, hi)
    return [(v - lo)/(hi - lo) for v in vals], (lo, hi)

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    # Policy parameters
    maxAnts   = float(S["ex:policy"]["ex:maxAnts"])
    epsilon   = float(S["ex:policy"]["ex:epsilon"])
    wFood     = float(S["ex:policy"]["ex:wFood"])
    wPher     = float(S["ex:policy"]["ex:wPher"])
    wSafe     = float(S["ex:policy"]["ex:wSafe"])
    wDist     = float(S["ex:policy"]["ex:wDist"])
    maxHaz    = float(S["ex:policy"]["ex:maxHazard"])
    foodPerAnt= float(S["ex:policy"]["ex:foodPerAnt"])
    evap      = float(S["ex:policy"]["ex:evapRate"])
    deposit   = float(S["ex:policy"]["ex:depositPerFood"])

    def driver(D: Dict[str, Dict[str, Any]]):
        rain = float(D["ex:env"]["ex:rainFactor"])
        rainEff = 1.0 - rain  # R2
        trails = [str(t) for t in listify(D["ex:colony"]["ex:hasTrail"])]

        # Collect raw values
        rows = []
        for iri in trails:
            P = D[iri]
            rows.append({
                "iri": iri,
                "name": str(P.get("ex:name", iri.split("#")[-1])),
                "foodN": float(P.get("ex:foodN", 0.0)),
                "hazardN": float(P.get("ex:hazardN", 1.0)),
                "distance_m": float(P.get("ex:distance_m", 1.0)),
                "pher": float(P.get("ex:pher", 0.0)),
                "capAnts": float(P.get("ex:capAnts", maxAnts)),
                "blocked": bool(P.get("ex:blocked", False)),
            })

        # Normalize distance and pheromone (after rain attenuation)
        dist_vals = [r["distance_m"] for r in rows]
        distN, _ = minmax(dist_vals)          # larger → farther (worse), we will use invDist = 1 - distN
        pherEff = [r["pher"] * rainEff for r in rows]
        pherN, _ = minmax(pherEff)            # normalized effective pheromone

        evals: List[TrailEval] = []
        for i, r in enumerate(rows):
            invHaz = 1.0 - r["hazardN"]       # R1
            invDist = 1.0 - distN[i]
            score = (wFood * r["foodN"]) + (wPher * pherN[i]) + (wSafe * invHaz) + (wDist * invDist)  # R4
            oper = (not r["blocked"]) and (r["hazardN"] <= maxHaz)  # R5

            evals.append(TrailEval(
                iri=r["iri"], name=r["name"], foodN=r["foodN"], hazardN=r["hazardN"], invHazard=invHaz,
                distance_m=r["distance_m"], distN=distN[i], invDist=invDist,
                pher=r["pher"], pherEff=pherEff[i], pherN=pherN[i],
                score=score, operable=oper, capAnts=r["capAnts"],
                expl_frac=0.0, expl_ants=0.0, base_frac=0.0, base_ants=0.0,
                ants=0.0, food_collected=0.0, new_pher=0.0, trace=[]
            ))

        # Compute base exploitation fractions among operable trails using positive weights
        operables = [e for e in evals if e.operable]
        if operables:
            min_sc = min(e.score for e in operables)
            weights = [(e.score - min_sc + 1e-9) for e in operables]
            wsum = sum(weights)
            base_fracs = [w/wsum for w in weights] if wsum > 0 else [1.0/len(operables)]*len(operables)
        else:
            base_fracs = []

        # Exploration fraction uniformly over operables
        # Final desired fraction = (1-eps)*base + eps*(1/N_oper)
        desired = {}
        if operables:
            N = len(operables)
            for i, e in enumerate(operables):
                bf = base_fracs[i]
                exf = 1.0/N
                final_f = (1.0 - epsilon) * bf + epsilon * exf
                desired[e.iri] = final_f
        # Allocate ants with caps via simple waterfilling
        ants = {e.iri: 0.0 for e in evals}
        # Initial desire
        for e in operables:
            ants[e.iri] = desired.get(e.iri, 0.0) * maxAnts
        # Cap and compute leftover
        leftover = 0.0
        for e in operables:
            if ants[e.iri] > e.capAnts:
                leftover += ants[e.iri] - e.capAnts
                ants[e.iri] = e.capAnts
        # Redistribute leftover to operables with headroom proportional to base_fracs
        if leftover > 1e-9:
            headroom = [max(0.0, e.capAnts - ants[e.iri]) for e in operables]
            hsum = sum(headroom)
            if hsum > 0:
                for i, e in enumerate(operables):
                    take = leftover * (headroom[i] / hsum)
                    ants[e.iri] += take
                    # ensure not exceeding cap due to rounding
                    ants[e.iri] = min(ants[e.iri], e.capAnts)
            # else: no headroom, leftover remains unassigned

        # Fill evals, expected food, pheromone update, and traces
        total_assigned = sum(ants.values())
        for e in evals:
            e.ants = ants[e.iri] if e.operable else 0.0
            # back-compute pieces for trace if operable
            if e.operable and operables:
                N = len(operables)
                idx = [x.iri for x in operables].index(e.iri)
                min_sc = min(x.score for x in operables)
                w_i = e.score - min_sc + 1e-9
                wsum = sum((x.score - min_sc + 1e-9) for x in operables)
                bf = (w_i/wsum) if wsum > 0 else 1.0/N
                exf = 1.0/N
                e.base_frac = bf
                e.expl_frac = exf
                e.base_ants = bf * (1.0 - epsilon) * maxAnts
                e.expl_ants = exf * epsilon * maxAnts
            # Food and pheromone update
            e.food_collected = e.ants * (foodPerAnt * e.foodN)
            e.new_pher = (e.pher * (1.0 - evap)) + (deposit * e.food_collected)

            e.trace = [
                f"R1: invHazard = (1 {e.hazardN:.3f}) math:difference → {e.invHazard:.3f}",
                f"R2: rainEffect = (1 {rain:.3f}) math:difference → {rainEff:.3f}",
                f"R3: pherEff = ({e.pher:.2f} {rainEff:.2f}) math:product → {e.pherEff:.2f}; normalized pherN={e.pherN:.3f}",
                (f"R4: score = "
                 f"({wFood:.2f} {e.foodN:.3f})· + ({wPher:.2f} {e.pherN:.3f})· + "
                 f"({wSafe:.2f} {e.invHazard:.3f})· + ({wDist:.2f} {e.invDist:.3f}) → {e.score:.3f}"),
                (f"Split: base={(1-epsilon):.2f}*{e.base_frac:.3f} + eps={epsilon:.2f}*{e.expl_frac if e.operable and operables else 0.0:.3f} "
                 f"→ desired ants≈{(desired.get(e.iri,0.0)*maxAnts) if e.operable else 0.0:.2f}"),
                (f"Caps/blocks: cap={e.capAnts:.1f}, blocked={str(not e.operable).lower()} ⇒ assigned={e.ants:.2f}"),
                (f"Phero update: ({e.pher:.2f} (1 {evap:.2f}) math:difference→{e.pher*(1-evap):.2f}) "
                 f"+ ({deposit:.2f} {e.food_collected:.2f}) math:product→{deposit*e.food_collected:.2f} "
                 f"= {e.new_pher:.2f}")
            ]

        # Sort by name for tidy output
        evals.sort(key=lambda z: z.name)
        totals = {
            "maxAnts": maxAnts,
            "assignedAnts": total_assigned,
            "foodTotal": sum(e.food_collected for e in evals),
            "rain": rain,
            "rainEff": rainEff,
        }
        ctx = {
            "epsilon": epsilon, "weights": (wFood, wPher, wSafe, wDist),
            "maxHazard": maxHaz, "evap": evap, "deposit": deposit, "foodPerAnt": foodPerAnt
        }
        return evals, totals, ctx

    return driver

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────
def main():
    S = parse_turtle_simple(STATIC_TTL)
    D = parse_turtle_simple(DYNAMIC_TTL)

    driver = specialize_driver(S)
    evals, totals, ctx = driver(D)

    # ── ANSWER ──
    print("Answer:")
    for e in evals:
        status = "operable" if e.operable else "blocked"
        print(f"- {e.name:12s} [{status}] • ants {e.ants:6.2f} "
              f"({(e.ants/totals['maxAnts']*100):5.1f}%) • food {e.food_collected:6.2f} "
              f"• pher {e.pher:5.2f} → {e.new_pher:5.2f}")
    print(f"  • Totals: ants {totals['assignedAnts']:.2f}/{totals['maxAnts']:.2f} ; "
          f"food {totals['foodTotal']:.2f} ; rainEff {totals['rainEff']:.2f}")

    # ── REASON WHY ──
    print("\nReason why:")
    wf, wp, ws, wd = ctx["weights"]
    print(f"- Weights: wFood={wf:.2f}, wPher={wp:.2f}, wSafe={ws:.2f}, wDist={wd:.2f}; epsilon={ctx['epsilon']:.2f}; maxHazard={ctx['maxHazard']:.2f}")
    for e in evals:
        print(f"- {e.name}:")
        for ln in e.trace:
            print(f"  • {ln}")

    # ── CHECK (harness) ─
    print("\nCheck (harness):")
    errors: List[str] = []

    wf, wp, ws, wd = ctx["weights"]
    maxAnts = totals["maxAnts"]
    epsilon = ctx["epsilon"]

    # (C1) Score recomputation from stored normalized features + weights (exact mirror)
    c1_max_ds = 0.0
    for e in evals:
        s_re = (wf * e.foodN) + (wp * e.pherN) + (ws * e.invHazard) + (wd * e.invDist)
        ds = abs(s_re - e.score)
        c1_max_ds = max(c1_max_ds, ds)
        if ds > 1e-9:
            errors.append(f"(C1) Score mismatch for {e.name}: {e.score:.6f} vs {s_re:.6f}")

    # (C2) Conservation & caps: total ants = min(maxAnts, sum cap of operables); per-trail ants ≤ cap; blocked ⇒ zero
    target = maxAnts
    cap_sum = sum(e.capAnts for e in evals if e.operable)
    expected_total = min(target, cap_sum)
    total_assigned = sum(e.ants for e in evals)
    if abs(total_assigned - expected_total) > 1e-6:
        errors.append(f"(C2) Total ants {total_assigned:.6f} ≠ expected {expected_total:.6f}")

    for e in evals:
        if e.ants - e.capAnts > 1e-9:
            errors.append(f"(C2) Cap exceeded at {e.name}: {e.ants:.3f} > {e.capAnts:.3f}")
        if (not e.operable) and e.ants > 1e-9:
            errors.append(f"(C2) Blocked trail had nonzero ants: {e.name}")

    # (C3) Pheromone update equation
    evap = ctx["evap"]; deposit = ctx["deposit"]
    for e in evals:
        new_re = (e.pher * (1.0 - evap)) + (deposit * e.food_collected)
        if abs(new_re - e.new_pher) > 1e-9:
            errors.append(f"(C3) Pheromone update mismatch for {e.name}: {e.new_pher:.6f} vs {new_re:.6f}")

    # Helper to recompute allocations (for monotonicity probes) from stored normals
    def recompute_allocation(weights_tuple, eps):
        wf2, wp2, ws2, wd2 = weights_tuple
        ops = [e for e in evals if e.operable]
        if not ops:
            return {e.iri: 0.0 for e in evals}
        min_sc = min((wf2*e.foodN + wp2*e.pherN + ws2*e.invHazard + wd2*e.invDist) for e in ops)
        wts = [ (wf2*e.foodN + wp2*e.pherN + ws2*e.invHazard + wd2*e.invDist) - min_sc + 1e-9 for e in ops ]
        wsum = sum(wts)
        base_fracs = [w/wsum for w in wts] if wsum > 0 else [1.0/len(ops)]*len(ops)
        N = len(ops)
        desired = [ (1.0-eps)*base_fracs[i] + eps*(1.0/N) for i in range(N) ]
        ants_map = {e.iri: 0.0 for e in evals}
        for i, e in enumerate(ops):
            ants_map[e.iri] = desired[i] * maxAnts
        # cap + redistribute
        leftover = 0.0
        for i, e in enumerate(ops):
            if ants_map[e.iri] > e.capAnts:
                leftover += ants_map[e.iri] - e.capAnts
                ants_map[e.iri] = e.capAnts
        if leftover > 1e-9:
            headroom = [max(0.0, ops[i].capAnts - ants_map[ops[i].iri]) for i in range(N)]
            hsum = sum(headroom)
            if hsum > 0:
                for i, e in enumerate(ops):
                    take = leftover * (headroom[i] / hsum)
                    ants_map[e.iri] += take
                    ants_map[e.iri] = min(ants_map[e.iri], e.capAnts)
        return ants_map

    # (C4) Safety-weight monotonicity: increasing wSafe should not raise ants-weighted avg hazard
    def renorm_weights(wf, wp, ws, wd, bump_ws=0.20):
        ws2 = ws + bump_ws
        remain = max(0.0, 1.0 - ws2)
        other = wf + wp + wd
        scale = (remain / other) if other > 1e-12 else 0.0
        return (wf*scale, wp*scale, ws2, wd*scale)

    ants_map0 = {e.iri: e.ants for e in evals}
    avg_hazard0 = (sum(e.hazardN * ants_map0[e.iri] for e in evals) /
                   max(1e-12, sum(ants_map0.values())))
    wf2, wp2, ws2, wd2 = renorm_weights(wf, wp, ws, wd, 0.20)
    ants_map1 = recompute_allocation((wf2, wp2, ws2, wd2), epsilon)
    avg_hazard1 = (sum(e.hazardN * ants_map1.get(e.iri, 0.0) for e in evals) /
                   max(1e-12, sum(ants_map1.values())))
    if avg_hazard1 > avg_hazard0 + 1e-9:
        errors.append(f"(C4) Increasing wSafe raised avg hazard ({avg_hazard0:.4f} → {avg_hazard1:.4f})")

    # (C5) Exploration monotonicity: increasing epsilon should not *increase* concentration (HHI)
    def hhi(ants_map):
        total = max(1e-12, sum(ants_map.values()))
        ops = [e for e in evals if e.operable]
        if not ops:
            return 1.0
        shares = [ants_map.get(e.iri, 0.0)/total for e in ops]
        return sum(s*s for s in shares)

    ants_map_eps0 = ants_map0
    ants_map_eps1 = recompute_allocation((wf, wp, ws, wd), min(1.0, epsilon + 0.20))
    if hhi(ants_map_eps1) > hhi(ants_map_eps0) + 1e-9:
        errors.append(f"(C5) Increasing epsilon increased concentration (HHI {hhi(ants_map_eps0):.4f} → {hhi(ants_map_eps1):.4f})")

    # Outcome
    if errors:
        print("❌ FAIL")
        for er in errors:
            print(" -", er)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Score re-check OK: max Δscore={c1_max_ds:.3e}")
        print(f"  • [C2] Conservation & caps OK: assigned {total_assigned:.2f} / expected {expected_total:.2f} ants")
        print(f"  • [C3] Pheromone update OK (evap={evap:.2f}, deposit={deposit:.2f})")
        print(f"  • [C4] Safety-weight monotonicity OK: avg hazard {avg_hazard0:.4f} → {avg_hazard1:.4f} (expected Δ≤0)")
        print(f"  • [C5] Exploration monotonicity OK: HHI {hhi(ants_map_eps0):.4f} → {hhi(ants_map_eps1):.4f} (expected Δ≤0)")

if __name__ == "__main__":
    main()

