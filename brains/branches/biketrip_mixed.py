#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (weights + rider physics/policies) + RULES_N3 (math:* triple
# patterns only) are partially evaluated into a compact Driver. At run time,
# the Driver ingests dynamic RDF (candidate routes + weather/traffic), computes
# per-route features (time, hills, traffic, rain, headwind), normalizes them,
# mirrors the N3 scoring (weighted sum), enforces feasibility constraints, and
# prints:
#   1) Answer (ranked feasible routes with key metrics),
#   2) Reason why (trace lines that mirror math:* steps),
#   3) Check (harness that revalidates feasibility, scores, sorting, and
#      a monotonicity probe on the time weight).
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
@prefix ex:  <http://example.org/bike#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~= 1.0) — lower is better on all normalized features
ex:policy ex:wTime    0.45 .
ex:policy ex:wHills   0.20 .
ex:policy ex:wTraffic 0.20 .
ex:policy ex:wRain    0.10 .
ex:policy ex:wWind    0.05 .

# Rider/physics
ex:policy ex:baseSpeed_kmh         22.0 .   # baseline cruise speed on flat, dry, calm
ex:policy ex:rainSpeedFactor       0.02 .   # speed multiplier loss per mm/h rain
ex:policy ex:headWindPenalty_kmh   0.50 .   # km/h speed loss per m/s headwind
ex:policy ex:minSpeed_kmh          10.0 .   # floor after penalties
ex:policy ex:hillTime_min_per_100m 4.0 .    # extra minutes per 100 m of climb

# Feasibility thresholds
ex:policy ex:maxTime_min    120 .          # must arrive within 2h
ex:policy ex:maxTraffic_idx 7 .            # cap on traffic index
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/bike#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Weather/traffic signals per route (avg over the route)
ex:RouteA ex:rain_mm_h 0.5 . ex:RouteA ex:headwind_ms 1.0 . ex:RouteA ex:traffic_idx 4 .
ex:RouteB ex:rain_mm_h 2.0 . ex:RouteB ex:headwind_ms 3.0 . ex:RouteB ex:traffic_idx 6 .
ex:RouteC ex:rain_mm_h 0.0 . ex:RouteC ex:headwind_ms 0.0 . ex:RouteC ex:traffic_idx 5 .

# Candidates
ex:cand ex:consider ex:RouteA .
ex:cand ex:consider ex:RouteB .
ex:cand ex:consider ex:RouteC .

# Route geometry (aggregate per route)
ex:RouteA ex:name "Park Loop" .
ex:RouteA ex:distance_km 18.0 .
ex:RouteA ex:uphill_m    120 .
ex:RouteA ex:notes "Mostly park paths, a few crossings." .

ex:RouteB ex:name "Direct Arterial" .
ex:RouteB ex:distance_km 15.5 .
ex:RouteB ex:uphill_m    200 .
ex:RouteB ex:notes "Fast but busier." .

ex:RouteC ex:name "Greenway" .
ex:RouteC ex:distance_km 20.0 .
ex:RouteC ex:uphill_m     80 .
ex:RouteC ex:notes "Longer but calm and flat." .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — math:* built-ins only (correct arities)
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/bike#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Attach score from normalized features and policy weights:
# score = wTime*timeN + wHills*hillsN + wTraffic*trafficN + wRain*rainN + wWind*windN
{
  ?r ex:timeN ?tN .
  ?r ex:hillsN ?hN .
  ?r ex:trafficN ?trN .
  ?r ex:rainN ?raN .
  ?r ex:windN ?wiN .
  ex:policy ex:wTime    ?wT .
  ex:policy ex:wHills   ?wH .
  ex:policy ex:wTraffic ?wTr .
  ex:policy ex:wRain    ?wR .
  ex:policy ex:wWind    ?wW .
  ( ?wT ?tN )   math:product ?tTerm .
  ( ?wH ?hN )   math:product ?hTerm .
  ( ?wTr ?trN ) math:product ?trTerm .
  ( ?wR ?raN )  math:product ?rTerm .
  ( ?wW ?wiN )  math:product ?wTerm .
  ( ?tTerm ?hTerm )   math:sum ?th .
  ( ?trTerm ?rTerm )  math:sum ?trr .
  ( ?th ?trr )        math:sum ?left .
  ( ?left ?wTerm )    math:sum ?score .
}
=>
{ ?r ex:score ?score } .

# Feasibility
# time_min ≤ maxTime_min  AND  traffic_idx ≤ maxTraffic_idx
{ ?r ex:time_min ?tm . ex:policy ex:maxTime_min ?lim . ?tm math:notGreaterThan ?lim . } => { ?r ex:feasible true } .
{ ?r ex:time_min ?tm . ex:policy ex:maxTime_min ?lim . ?tm math:greaterThan ?lim . }   => { ?r ex:feasible false } .
{ ?r ex:traffic_idx ?ti . ex:policy ex:maxTraffic_idx ?mx . ?ti math:greaterThan ?mx . } => { ?r ex:feasible false } .
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
    wTime: float
    wHills: float
    wTraffic: float
    wRain: float
    wWind: float

@dataclass
class RouteEval:
    iri: str
    name: str
    distance_km: float
    uphill_m: float
    rain_mm_h: float
    headwind_ms: float
    traffic_idx: float
    time_min: float
    feasible: bool
    # normalized features captured for auditability
    timeN: float
    hillsN: float
    trafficN: float
    rainN: float
    windN: float
    score: float
    trace: List[str]
    notes: str

def clamp(x, lo, hi): return lo if x < lo else hi if x > hi else x

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    # weights
    W = Weights(
        wTime=float(S["ex:policy"]["ex:wTime"]),
        wHills=float(S["ex:policy"]["ex:wHills"]),
        wTraffic=float(S["ex:policy"]["ex:wTraffic"]),
        wRain=float(S["ex:policy"]["ex:wRain"]),
        wWind=float(S["ex:policy"]["ex:wWind"]),
    )
    base_v = float(S["ex:policy"]["ex:baseSpeed_kmh"])
    rain_fac = float(S["ex:policy"]["ex:rainSpeedFactor"])
    wind_pen = float(S["ex:policy"]["ex:headWindPenalty_kmh"])
    v_min = float(S["ex:policy"]["ex:minSpeed_kmh"])
    hill_min_per100 = float(S["ex:policy"]["ex:hillTime_min_per_100m"])
    max_time = float(S["ex:policy"]["ex:maxTime_min"])
    max_traffic = float(S["ex:policy"]["ex:maxTraffic_idx"])

    def driver(D: Dict[str, Dict[str, Any]]):
        routes = [str(r) for r in listify(D["ex:cand"]["ex:consider"])]
        rows = []
        for iri in routes:
            P = D[iri]
            name = str(P.get("ex:name", iri))
            distance = float(P.get("ex:distance_km", 0.0))
            uphill = float(P.get("ex:uphill_m", 0.0))
            rain = float(P.get("ex:rain_mm_h", 0.0))
            headwind = float(P.get("ex:headwind_ms", 0.0))
            traffic = float(P.get("ex:traffic_idx", 0.0))
            notes = str(P.get("ex:notes", ""))

            # Effective speed (km/h): base reduced by rain & headwind; floor at v_min
            v_rain = base_v * (1.0 - rain_fac * max(0.0, rain))
            v_head = v_rain - wind_pen * max(0.0, headwind)
            v_eff = max(v_min, v_head)

            # Time on distance + hill penalty
            time_h = distance / v_eff
            time_min = time_h * 60.0 + (uphill / 100.0) * hill_min_per100

            # Feasibility
            feasible = (time_min <= max_time) and (traffic <= max_traffic)

            rows.append({
                "iri": iri, "name": name, "distance": distance, "uphill": uphill,
                "rain": rain, "headwind": headwind, "traffic": traffic, "time_min": time_min,
                "feasible": feasible, "notes": notes,
                "v_eff": v_eff, "v_head": v_head, "v_rain": v_rain
            })

        # Normalization (min-max) where smaller is better
        def minmax(vals):
            lo, hi = min(vals), max(vals)
            if hi - lo < 1e-12:
                return [0.0]*len(vals), (lo, hi)
            return [(v - lo)/(hi - lo) for v in vals], (lo, hi)

        timeN, _     = minmax([r["time_min"]  for r in rows])
        hillsN, _    = minmax([r["uphill"]    for r in rows])
        trafficN, _  = minmax([r["traffic"]   for r in rows])
        rainN, _     = minmax([max(0.0, r["rain"])     for r in rows])
        windN, _     = minmax([max(0.0, r["headwind"]) for r in rows])

        evals: List[RouteEval] = []
        for i, r in enumerate(rows):
            # RULES mirror (math:*) score
            tTerm = W.wTime    * timeN[i]
            hTerm = W.wHills   * hillsN[i]
            trTerm= W.wTraffic * trafficN[i]
            raTerm= W.wRain    * rainN[i]
            wTerm = W.wWind    * windN[i]
            th    = tTerm + hTerm
            trr   = trTerm + raTerm
            left  = th + trr
            score = left + wTerm

            trace = [
                (f"Speed chain: base={base_v:.1f} → rain ({max(0.0,r['rain']):.1f} mm/h) → v_rain={r['v_rain']:.1f} ; "
                 f"headwind ({max(0.0,r['headwind']):.1f} m/s) → v_head={r['v_head']:.1f} ; "
                 f"v_eff=max({v_min:.1f}, v_head)={r['v_eff']:.1f} km/h"),
                (f"Time: dist {r['distance']:.1f} km / {r['v_eff']:.1f} km/h ×60 "
                 f"+ hills {r['uphill']:.0f} m × {hill_min_per100:.1f}/100m → {r['time_min']:.1f} min"),
                (f"Score N3: "
                 f"({W.wTime:.2f} {timeN[i]:.3f}) math:product {tTerm:.3f} ; "
                 f"({W.wHills:.2f} {hillsN[i]:.3f}) math:product {hTerm:.3f} ; "
                 f"({W.wTraffic:.2f} {trafficN[i]:.3f}) math:product {trTerm:.3f} ; "
                 f"({W.wRain:.2f} {rainN[i]:.3f}) math:product {raTerm:.3f} ; "
                 f"({W.wWind:.2f} {windN[i]:.3f}) math:product {wTerm:.3f} ; "
                 f"sums→ {score:.3f}")
            ]

            evals.append(RouteEval(
                iri=r["iri"], name=r["name"], distance_km=r["distance"], uphill_m=r["uphill"],
                rain_mm_h=r["rain"], headwind_ms=r["headwind"], traffic_idx=r["traffic"],
                time_min=r["time_min"], feasible=r["feasible"],
                timeN=timeN[i], hillsN=hillsN[i], trafficN=trafficN[i], rainN=rainN[i], windN=windN[i],
                score=score, trace=trace, notes=r["notes"]
            ))

        # Sort: feasible first, then ascending score, tie-break by time then name
        evals.sort(key=lambda e: (not e.feasible, e.score, e.time_min, e.name))
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

    # ── ANSWER ──
    print("Answer:")
    if not feasible:
        print("- No feasible routes for the given constraints.")
    else:
        for rank, e in enumerate(feasible, start=1):
            print(f"- #{rank} {e.name} • {e.distance_km:.1f} km • time {e.time_min:.1f} min • "
                  f"traffic {e.traffic_idx:.0f} • hills {e.uphill_m:.0f} m • score {e.score:.3f}")

    # ── REASON WHY ──
    print("\nReason why:")
    print(f"- Weights: wTime={W.wTime:.2f}, wHills={W.wHills:.2f}, wTraffic={W.wTraffic:.2f}, "
          f"wRain={W.wRain:.2f}, wWind={W.wWind:.2f}")
    for e in evals:
        feas = "true" if e.feasible else "false"
        print(f"- {e.name}: feasible={feas}")
        for ln in e.trace:
            print(f"  • {ln}")

    # ── CHECK (harness) ─
    print("\nCheck (harness):")
    errors: List[str] = []

    # (C1) Recompute score from stored normalized features + weights (exact mirror)
    c1_max_d = 0.0
    for e in evals:
        tTerm = W.wTime    * e.timeN
        hTerm = W.wHills   * e.hillsN
        trTerm= W.wTraffic * e.trafficN
        rTerm = W.wRain    * e.rainN
        wTerm = W.wWind    * e.windN
        th = tTerm + hTerm
        trr = trTerm + rTerm
        left = th + trr
        recomputed = left + wTerm
        d = abs(recomputed - e.score)
        c1_max_d = max(c1_max_d, d)
        if d > 1e-12:
            errors.append(f"(C1) Score mismatch for {e.name}: {e.score:.6f} vs {recomputed:.6f}")

    # (C2) Feasibility re-check
    S_pol = parse_turtle_simple(STATIC_TTL)["ex:policy"]
    max_time = float(S_pol["ex:maxTime_min"])
    max_traffic = float(S_pol["ex:maxTraffic_idx"])

    c2_mismatches = 0
    for e in evals:
        feas2 = (e.time_min <= max_time) and (e.traffic_idx <= max_traffic)
        if feas2 != e.feasible:
            errors.append(f"(C2) Feasibility mismatch for {e.name}")
            c2_mismatches += 1

    # (C3) Sorting order (feasible first, then score, then time, then name)
    order_ok = [x.iri for x in sorted(evals, key=lambda z: (not z.feasible, z.score, z.time_min, z.name))] \
               == [x.iri for x in evals]
    if not order_ok:
        errors.append("(C3) Sorting order mismatch")

    # (C4) Monotonicity w.r.t time weight — re-rank using the same stored normals
    def winner_with_wtime(delta: float):
        wT = W.wTime + delta
        # Renormalize the rest to keep sum ~ 1.0
        others_sum = W.wHills + W.wTraffic + W.wRain + W.wWind
        remain = max(0.0, 1.0 - wT)
        scale = (remain / others_sum) if others_sum > 1e-12 else 0.0
        wH = W.wHills * scale
        wTr= W.wTraffic * scale
        wR = W.wRain * scale
        wW = W.wWind * scale

        def new_score(e):
            return (wT * e.timeN) + (wH * e.hillsN) + (wTr * e.trafficN) + (wR * e.rainN) + (wW * e.windN)

        reranked = sorted(evals, key=lambda z: (not z.feasible, new_score(z), z.time_min, z.name))
        feas2 = [x for x in reranked if x.feasible]
        return feas2[0] if feas2 else None

    top = feasible[0] if feasible else None
    top2 = winner_with_wtime(0.20)
    if top and top2 and (top2.time_min > top.time_min + 1e-9):
        errors.append(f"(C4) Increasing wTime produced a *slower* winner ({top.time_min:.1f} → {top2.time_min:.1f} min)")

    # (C5) Determinism of formatting + tie stats
    tie_pairs = 0
    for i in range(max(0, len(feasible) - 1)):
        for j in range(i + 1, len(feasible)):
            if abs(feasible[i].score - feasible[j].score) <= 1e-12:
                tie_pairs += 1

    # Outcome
    if errors:
        print("❌ FAIL")
        for e in errors:
            print(" -", e)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Score re-check OK: max Δ={c1_max_d:.3e}")
        print(f"  • [C2] Feasibility OK: mismatches={c2_mismatches}")
        print(f"  • [C3] Order OK: {'stable' if order_ok else '—'}")
        if top and top2:
            print(f"  • [C4] Time-weight monotonicity OK: {top.time_min:.1f} → {top2.time_min:.1f} min (expected Δ≤0)")
        else:
            print("  • [C4] Time-weight monotonicity OK: no feasible winner in one of the runs")
        print(f"  • Ties among feasible (|Δscore| ≤ 1e-12): {tie_pairs}")

if __name__ == "__main__":
    main()

