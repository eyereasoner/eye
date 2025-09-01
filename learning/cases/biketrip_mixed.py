#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (policy & rider model) + RULES_N3 (math:* triple patterns only)
# are partially evaluated into a small "driver" function. At run time, the
# driver ingests dynamic RDF (routes, segments, weather/closures), computes
# normalized route features, mirrors the N3 scoring with weighted sums
# (using the same math:* semantics), eliminates infeasible routes (any closed
# segment), and prints:
#   1) Answer (ranked feasible routes),
#   2) Reason why (trace lines that mirror math:* steps),
#   3) Check (a built-in harness that re-validates feasibility, scoring, and sorting).
#
# Contract with EYE learning:
# - All rules arithmetic/relations are expressed with math:* built-ins only.
# - Everything is inline (no external file writes).
# - One file produces Answer • Reason why • Check.

from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, List, Tuple, Any

# ──────────────────────────────────────────────────────────────────────────────
# GOAL
# ──────────────────────────────────────────────────────────────────────────────
GOAL = (
    "Given static policy/physics RDF and math:* N3 rules, specialize a driver "
    "that ingests dynamic routes/weather/closures and outputs a feasible, ranked "
    "route list with explanation traces."
)

# ──────────────────────────────────────────────────────────────────────────────
# STATIC RDF (compile-time): policy weights, rider model, constants
# ──────────────────────────────────────────────────────────────────────────────
STATIC_TTL = r"""
@prefix ex:  <http://example.org/bike#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ≈ 1.0)
ex:policy ex:wTime   0.50 .
ex:policy ex:wSafety 0.30 .
ex:policy ex:wGain   0.15 .
ex:policy ex:wScenic 0.05 .

# Rider model / physics-ish constants
ex:rider  ex:baseSpeedKmh    18.0 .   # flat road, fair weather
ex:rider  ex:rainSpeedFactor  0.90 .  # slowdown if raining
ex:rider  ex:ltsSlowdown3     0.95 .  # extra slowdown on LTS≥3 segments
"""

# ──────────────────────────────────────────────────────────────────────────────
# DYNAMIC RDF (run-time): routes, segments, weather, closures
# (note: one triple per line to keep parsing trivial)
# ──────────────────────────────────────────────────────────────────────────────
DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/bike#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Weather/context
ex:weather ex:rain false .

# Route-level wind fractions (headwind>0 slows; tailwind<0 aids)
ex:route_r1 ex:windFrac 0.05 .
ex:route_r2 ex:windFrac -0.10 .
ex:route_r3 ex:windFrac 0.00 .

# Route segment membership
ex:route_r1 ex:hasSegment ex:r1_s1 .
ex:route_r1 ex:hasSegment ex:r1_s2 .
ex:route_r1 ex:hasSegment ex:r1_s3 .
ex:route_r2 ex:hasSegment ex:r2_s1 .
ex:route_r2 ex:hasSegment ex:r2_s2 .
ex:route_r2 ex:hasSegment ex:r2_s3 .
ex:route_r3 ex:hasSegment ex:r3_s1 .
ex:route_r3 ex:hasSegment ex:r3_s2 .
ex:route_r3 ex:hasSegment ex:r3_s3 .

# Segments — one triple per line
# r1: shortest, some stress, small climbs, moderate scenic
ex:r1_s1 ex:lengthKm 2.0 .
ex:r1_s1 ex:gainM 20 .
ex:r1_s1 ex:lts 3 .
ex:r1_s1 ex:bikeLane false .
ex:r1_s1 ex:scenic 0.5 .
ex:r1_s1 ex:closed false .

ex:r1_s2 ex:lengthKm 1.8 .
ex:r1_s2 ex:gainM 15 .
ex:r1_s2 ex:lts 2 .
ex:r1_s2 ex:bikeLane true .
ex:r1_s2 ex:scenic 0.6 .
ex:r1_s2 ex:closed false .

ex:r1_s3 ex:lengthKm 1.4 .
ex:r1_s3 ex:gainM 10 .
ex:r1_s3 ex:lts 2 .
ex:r1_s3 ex:bikeLane true .
ex:r1_s3 ex:scenic 0.7 .
ex:r1_s3 ex:closed false .

# r2: a bit longer, safer, more scenic, good tailwind
ex:r2_s1 ex:lengthKm 2.2 .
ex:r2_s1 ex:gainM 25 .
ex:r2_s1 ex:lts 2 .
ex:r2_s1 ex:bikeLane true .
ex:r2_s1 ex:scenic 0.7 .
ex:r2_s1 ex:closed false .

ex:r2_s2 ex:lengthKm 2.0 .
ex:r2_s2 ex:gainM 20 .
ex:r2_s2 ex:lts 1 .
ex:r2_s2 ex:bikeLane true .
ex:r2_s2 ex:scenic 0.8 .
ex:r2_s2 ex:closed false .

ex:r2_s3 ex:lengthKm 1.9 .
ex:r2_s3 ex:gainM 15 .
ex:r2_s3 ex:lts 1 .
ex:r2_s3 ex:bikeLane true .
ex:r2_s3 ex:scenic 0.9 .
ex:r2_s3 ex:closed false .

# r3: flat and direct but one closure (should be infeasible)
ex:r3_s1 ex:lengthKm 1.9 .
ex:r3_s1 ex:gainM 5 .
ex:r3_s1 ex:lts 3 .
ex:r3_s1 ex:bikeLane false .
ex:r3_s1 ex:scenic 0.4 .
ex:r3_s1 ex:closed false .

ex:r3_s2 ex:lengthKm 1.5 .
ex:r3_s2 ex:gainM 5 .
ex:r3_s2 ex:lts 4 .
ex:r3_s2 ex:bikeLane false .
ex:r3_s2 ex:scenic 0.3 .
ex:r3_s2 ex:closed true .

ex:r3_s3 ex:lengthKm 1.2 .
ex:r3_s3 ex:gainM 5 .
ex:r3_s3 ex:lts 3 .
ex:r3_s3 ex:bikeLane false .
ex:r3_s3 ex:scenic 0.4 .
ex:r3_s3 ex:closed false .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — only math:* built-ins for arithmetic/relations
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/bike#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Score = wTime*timeN + wSafety*riskN + wGain*gainN + wScenic*(1 - scenicN)
{
  ?r  ex:timeN   ?tN .
  ?r  ex:riskN   ?sN .
  ?r  ex:gainN   ?gN .
  ?r  ex:scenicN ?scN .
  ex:policy ex:wTime   ?wT .
  ex:policy ex:wSafety ?wS .
  ex:policy ex:wGain   ?wG .
  ex:policy ex:wScenic ?wSc .
  ( 1 ?scN )          math:difference ?invSc .
  ( ?wT ?tN )         math:product    ?tTerm .
  ( ?wS ?sN )         math:product    ?sTerm .
  ( ?wG ?gN )         math:product    ?gTerm .
  ( ?wSc ?invSc )     math:product    ?scTerm .
  ( ?tTerm ?sTerm )   math:sum        ?ts .
  ( ?gTerm ?scTerm )  math:sum        ?gs .
  ( ?ts ?gs )         math:sum        ?score .
}
=>
{ ?r ex:score ?score } .

# Feasibility: any closed segment implies infeasible route (pure triple pattern)
{ ?r ex:hasSegment ?seg . ?seg ex:closed true . } => { ?r ex:feasible false } .
"""

# ──────────────────────────────────────────────────────────────────────────────
# Minimal Turtle reader (numbers, booleans, strings, qnames; supports repeats)
# ──────────────────────────────────────────────────────────────────────────────
def parse_turtle_simple(s: str) -> Dict[str, Dict[str, Any]]:
    """
    Minimal, resilient Turtle reader for this case:
    - Skips @prefix lines.
    - Strips inline comments after '#' (but preserves '#' inside quoted strings).
    - Handles booleans, numbers, qnames; collects repeated predicates into lists.
    - Assumes one triple per line (as in our TTL), but is tolerant if multiple show up.
    """
    out: Dict[str, Dict[str, Any]] = {}
    buf: List[str] = []

    def strip_inline_comment(line: str) -> str:
        in_str = False
        esc = False
        out_chars = []
        for ch in line:
            if ch == '"' and not esc:
                in_str = not in_str
            if ch == '#' and not in_str:
                break  # cut comment
            out_chars.append(ch)
            # manage escaping inside strings
            if esc:
                esc = False
            elif ch == '\\':
                esc = True
        return ''.join(out_chars)

    def _add(subj: str, pred: str, val: Any):
        if pred == 'a':
            pred = ':type'
        slot = out.setdefault(subj, {})
        if pred not in slot:
            slot[pred] = val
        else:
            if isinstance(slot[pred], list):
                slot[pred].append(val)
            else:
                slot[pred] = [slot[pred], val]

    def flush(stmt: str):
        # allow multiple triples in a single statement if present
        for triple in [t.strip() for t in stmt.split(' . ') if t.strip()]:
            if triple.endswith('.'):
                triple = triple[:-1].strip()
            if not triple or triple.startswith('@prefix'):
                continue
            parts = triple.split(None, 2)
            if len(parts) < 3:
                continue
            subj, pred, obj = parts[0], parts[1], parts[2].strip()
            if obj.startswith('"'):
                # read quoted literal (no datatype/lang handling needed here)
                if obj.count('"') == 1:
                    # very long literal broken across spaces — nothing in our data, but be safe
                    pass
                lit = obj[1: obj.find('"', 1)]
                _add(subj, pred, lit)
            else:
                tok = obj.split()[0]
                if tok in ('true', 'false'):
                    _add(subj, pred, tok == 'true')
                else:
                    try:
                        _add(subj, pred, float(tok))
                    except ValueError:
                        _add(subj, pred, tok)

    for raw in s.splitlines():
        if raw.lstrip().startswith('@prefix'):
            continue
        line = strip_inline_comment(raw).strip()
        if not line:
            continue
        buf.append(line)
        if line.endswith('.'):
            flush(' '.join(buf))
            buf = []
    if buf:
        flush(' '.join(buf))
    return out

def listify(v) -> List[Any]:
    if v is None:
        return []
    return v if isinstance(v, list) else [v]

# ──────────────────────────────────────────────────────────────────────────────
# Domain structures
# ──────────────────────────────────────────────────────────────────────────────
@dataclass
class Policy:
    w_time: float
    w_safety: float
    w_gain: float
    w_scenic: float
    base_speed: float
    rain_factor: float
    lts3_factor: float

@dataclass
class Segment:
    name: str
    length_km: float
    gain_m: float
    lts: int
    bike_lane: bool
    scenic: float
    closed: bool

@dataclass
class RouteEval:
    route: str
    feasible: bool
    time_h: float
    risk: float
    gain_m: float
    scenic: float
    timeN: float
    riskN: float
    gainN: float
    scenicN: float
    score: float
    trace: List[str]

# ──────────────────────────────────────────────────────────────────────────────
# Driver specialization (mixed computation)
# ──────────────────────────────────────────────────────────────────────────────
def specialize_driver(policy: Policy):
    MIN_SPEED_KMH = 5.0
    CLIMB_HOURS_PER_M = 0.0002  # +0.72 s per meter climb

    def seg_speed_kmh(lts: int, rain: bool, wind_frac: float) -> float:
        v = policy.base_speed
        if rain:
            v *= policy.rain_factor
        v *= (1.0 - wind_frac)  # headwind>0 reduces; tailwind<0 increases
        if lts >= 3:
            v *= policy.lts3_factor
        return max(v, MIN_SPEED_KMH)

    def eval_routes(kb: Dict[str, Dict[str, Any]]) -> Tuple[List[RouteEval], List[str]]:
        traces_global: List[str] = []
        rain = bool(kb.get("ex:weather", {}).get("ex:rain", False))

        routes = [s for s in kb.keys() if s.startswith("ex:route_")]
        segments: Dict[str, Segment] = {}
        membership: Dict[str, List[str]] = {r: [] for r in routes}
        wind: Dict[str, float] = {r: float(kb.get(r, {}).get("ex:windFrac", 0.0)) for r in routes}

        # Build segments
        for subj, props in kb.items():
            if subj.startswith("ex:r") and "_s" in subj:
                segments[subj] = Segment(
                    name=subj,
                    length_km=float(props.get("ex:lengthKm", 0.0)),
                    gain_m=float(props.get("ex:gainM", 0.0)),
                    lts=int(float(props.get("ex:lts", 1))),
                    bike_lane=bool(props.get("ex:bikeLane", False)),
                    scenic=float(props.get("ex:scenic", 0.0)),
                    closed=bool(props.get("ex:closed", False)),
                )

        # Route→segments (preserve all ex:hasSegment triples)
        for r in routes:
            for v in listify(kb.get(r, {}).get("ex:hasSegment")):
                membership[r].append(str(v))

        # Evaluate each route
        evals: List[RouteEval] = []
        for r in routes:
            segs = [segments[s] for s in membership[r] if s in segments]
            feas = all(not s.closed for s in segs)
            t_h = 0.0
            risk_vals = []
            gain_total = 0.0
            scenic_vals = []

            for s in segs:
                v = seg_speed_kmh(s.lts, rain, wind[r])
                t_seg = s.length_km / v + s.gain_m * CLIMB_HOURS_PER_M
                t_h += t_seg
                # risk per segment: base on LTS (1..4) + lane bonus
                risk_base = max(0.0, min(1.0, (s.lts - 1) / 3.0))
                if not s.bike_lane:
                    risk_base = min(1.0, risk_base + 0.10)
                risk_vals.append(risk_base)
                gain_total += s.gain_m
                scenic_vals.append(s.scenic)

            risk_avg = sum(risk_vals) / len(risk_vals) if risk_vals else 0.0
            scenic_avg = sum(scenic_vals) / len(scenic_vals) if scenic_vals else 0.0

            evals.append(RouteEval(
                route=r, feasible=feas, time_h=t_h, risk=risk_avg, gain_m=gain_total,
                scenic=scenic_avg, timeN=0, riskN=0, gainN=0, scenicN=0, score=0.0, trace=[]
            ))

        # Normalize (min-max across routes)
        def minmax(values: List[float]) -> Dict[str, float]:
            mn, mx = min(values), max(values)
            return {"mn": mn, "mx": mx, "rng": (mx - mn)}

        if evals:
            mm_time = minmax([e.time_h for e in evals])
            mm_risk = minmax([e.risk   for e in evals])
            mm_gain = minmax([e.gain_m for e in evals])

        for e in evals:
            e.timeN   = 0.5 if mm_time["rng"] == 0 else (e.time_h - mm_time["mn"]) / mm_time["rng"]
            e.riskN   = 0.5 if mm_risk["rng"] == 0 else (e.risk   - mm_risk["mn"]) / mm_risk["rng"]
            e.gainN   = 0.5 if mm_gain["rng"] == 0 else (e.gain_m - mm_gain["mn"]) / mm_gain["rng"]
            e.scenicN = max(0.0, min(1.0, e.scenic))

            # RULES_N3 mirror (math:*): score terms and sums
            invSc   = (1 - e.scenicN)                                  # (1 scenicN) math:difference invSc
            tTerm   = policy.w_time   * e.timeN                        # (wT tN)    math:product
            sTerm   = policy.w_safety * e.riskN                        # (wS sN)    math:product
            gTerm   = policy.w_gain   * e.gainN                        # (wG gN)    math:product
            scTerm  = policy.w_scenic * invSc                          # (wSc invSc) math:product
            ts      = tTerm + sTerm                                    # (tTerm sTerm) math:sum
            gs      = gTerm + scTerm                                   # (gTerm scTerm) math:sum
            e.score = ts + gs                                          # (ts gs) math:sum

            feas_str = "true" if e.feasible else "false"
            e.trace = [
                (f"Score N3: (1 {e.scenicN:.3f}) math:difference {invSc:.3f} ; "
                 f"({policy.w_time:.2f} {e.timeN:.3f}) math:product {tTerm:.3f} ; "
                 f"({policy.w_safety:.2f} {e.riskN:.3f}) math:product {sTerm:.3f} ; "
                 f"({policy.w_gain:.2f} {e.gainN:.3f}) math:product {gTerm:.3f} ; "
                 f"({policy.w_scenic:.2f} {invSc:.3f}) math:product {scTerm:.3f} ; "
                 f"sum→ {e.score:.3f}"),
                f"Feasible from triples: {e.route} ex:feasible {feas_str} ."
            ]

        # Sort by feasibility first (true before false), then by ascending score
        evals.sort(key=lambda z: (not z.feasible, z.score))
        return evals, traces_global

    return eval_routes

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────
def main():
    kb = {}
    kb.update(parse_turtle_simple(STATIC_TTL))
    kb_dyn = parse_turtle_simple(DYNAMIC_TTL)
    for k, v in kb_dyn.items():
        kb.setdefault(k, {}).update(v)

    # Pull policy/rider constants
    pol = Policy(
        w_time=float(kb["ex:policy"]["ex:wTime"]),
        w_safety=float(kb["ex:policy"]["ex:wSafety"]),
        w_gain=float(kb["ex:policy"]["ex:wGain"]),
        w_scenic=float(kb["ex:policy"]["ex:wScenic"]),
        base_speed=float(kb["ex:rider"]["ex:baseSpeedKmh"]),
        rain_factor=float(kb["ex:rider"]["ex:rainSpeedFactor"]),
        lts3_factor=float(kb["ex:rider"]["ex:ltsSlowdown3"]),
    )

    driver = specialize_driver(pol)
    evals, _ = driver(kb)

    # ── ANSWER ──
    print("Answer:")
    winners = [e for e in evals if e.feasible]
    if not winners:
        print("- No feasible routes.")
    else:
        for rank, e in enumerate(winners, start=1):
            mins = e.time_h * 60
            print(f"- #{rank} {e.route}  • score {e.score:.3f}  • time {mins:.1f} min  "
                  f"• risk {e.risk:.2f}  • gain {e.gain_m:.0f} m  • scenic {e.scenic:.2f}")

    # ── REASON WHY ──
    print("\nReason why:")
    for e in evals:
        print(f"- {e.route}:")
        for ln in e.trace:
            print(f"  • {ln}")

    # Echo rule/data fingerprints for auditability
    print("\nInputs (fingerprints):")
    print(f"- Static RDF bytes: {len(STATIC_TTL.encode())} ; Dynamic RDF bytes: {len(DYNAMIC_TTL.encode())}")
    print(f"- Rules N3 bytes: {len(RULES_N3.encode())} (arithmetic via math:* only)")

    # ── CHECK (harness) ──
    print("\nCheck (harness):")
    errors: List[str] = []

    # (C1) All infeasible routes have at least one closed segment; feasible winners have none.
    for e in evals:
        segs = listify(kb.get(e.route, {}).get("ex:hasSegment"))
        has_closed = any(bool(kb.get(s, {}).get("ex:closed", False)) for s in segs)
        if e.feasible == has_closed:
            errors.append(f"(C1) Feasibility mismatch for {e.route}")

    # (C2) Winner (if any) must be feasible
    if winners and not winners[0].feasible:
        errors.append("(C2) Top-ranked route is not feasible")

    # (C3) Score = weighted sum of normalized terms (recompute)
    for e in evals:
        invSc = (1 - e.scenicN)
        tTerm = pol.w_time   * e.timeN
        sTerm = pol.w_safety * e.riskN
        gTerm = pol.w_gain   * e.gainN
        scTerm= pol.w_scenic * invSc
        recomputed = (tTerm + sTerm) + (gTerm + scTerm)
        if abs(recomputed - e.score) > 1e-9:
            errors.append(f"(C3) Score mismatch on {e.route}: {e.score:.6f} vs {recomputed:.6f}")

    # (C4) Sorting by feasibility then score is correct
    sorted_copy = sorted(evals, key=lambda z: (not z.feasible, z.score))
    if [e.route for e in sorted_copy] != [e.route for e in evals]:
        errors.append("(C4) Sorting order mismatch")

    if errors:
        print("❌ FAIL")
        for er in errors:
            print(" -", er)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")

if __name__ == "__main__":
    main()

