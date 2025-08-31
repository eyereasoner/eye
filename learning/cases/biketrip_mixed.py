from __future__ import annotations
"""
BikeTrip Planner — EYE-style mixed computation (final)
=====================================================
Static RDF + N3 intent → Agent (partial eval, Ershov) → specialized Driver → ranked routes.

One-liner: a compact EYE-style planner that partially evaluates static policy/physics RDF plus
N3 rule intent into a specialized driver which consumes dynamic route/weather/closure signals
and outputs a feasible, safety/time/elevation-aware ranked list with explanation traces.

ASCII schematic
---------------
  ME (rider) ---------------------+
                                  |         +-------------------+
                                  |         |   Behaviors       |  (rule docs)
                                  |         |   (N3 templates)  |
                                  |         +-------------------+
                                  |                  ^    ^
                                  |                  |    |
                                  v                  |    | context (dynamic)
+-------------------+   AC    +----------+           |    |
| data (RDF, static)| <-----> | context  |-----------+    |
| policy, physics   | (access)+----------+                |
+-------------------+                                     v
                         Agent (partial evaluator / specialization)
                                     |
                                     v
                           Driver (specialized scorer)
                                     |
                                     v
             +-------------------------------------------------------------+
             | Targets / Capabilities / Context (routes & weather)         | --> ranked routes
             +-------------------------------------------------------------+
                                     |
                                     v
                          actionable insight + feedback (trace)
"""

import os, csv, json, math
from typing import Any, Dict, List, Tuple

# -----------------------------------------------------------------------------
# Artifact directory
# -----------------------------------------------------------------------------
BASE = os.path.join(os.path.dirname(__file__), "output/biketrip_artifacts")
os.makedirs(BASE, exist_ok=True)

EX = "http://example.org/bike#"

# -----------------------------------------------------------------------------
# STATIC RDF (compile-time): policy weights, rider model, constants
# -----------------------------------------------------------------------------
STATIC_TTL = """@prefix ex: <http://example.org/bike#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~ 1.0)
ex:policy ex:wTime   0.50 .
ex:policy ex:wSafety 0.30 .
ex:policy ex:wGain   0.15 .
ex:policy ex:wScenic 0.05 .

# Rider model / physics-ish constants
ex:rider ex:baseSpeedKmh 18.0 .      # flat road, fair weather
ex:rider ex:rainSpeedFactor 0.90 .   # slow-down if raining
ex:rider ex:ltsSlowdown3 0.95 .      # speed factor on LTS>=3 segments
"""

# -----------------------------------------------------------------------------
# DYNAMIC RDF (run-time): routes, segments, weather, closures
# 3 routes with 3 segments each, differing in stress/elevation/scenic.
# -----------------------------------------------------------------------------
def make_dynamic_ttl() -> str:
    ln = [
        "@prefix ex: <http://example.org/bike#> .",
        "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
        "",
        "# Weather/context",
        "ex:weather ex:rain false .",
        "",
        "# Routes and route-level wind fractions",
        "ex:route_r1 ex:windFrac  0.05 .",
        "ex:route_r2 ex:windFrac -0.10 .",
        "ex:route_r3 ex:windFrac  0.00 .",
        "",
        "# Route segment membership",
        "ex:route_r1 ex:hasSegment ex:r1_s1 .",
        "ex:route_r1 ex:hasSegment ex:r1_s2 .",
        "ex:route_r1 ex:hasSegment ex:r1_s3 .",
        "ex:route_r2 ex:hasSegment ex:r2_s1 .",
        "ex:route_r2 ex:hasSegment ex:r2_s2 .",
        "ex:route_r2 ex:hasSegment ex:r2_s3 .",
        "ex:route_r3 ex:hasSegment ex:r3_s1 .",
        "ex:route_r3 ex:hasSegment ex:r3_s2 .",
        "ex:route_r3 ex:hasSegment ex:r3_s3 .",
        "",
        "# Segments: one triple per line to keep parsing trivial",
        # r1: shortest, some stress, small climbs, moderate scenic
        "ex:r1_s1 ex:lengthKm 2.0 .",
        "ex:r1_s1 ex:gainM 20 .",
        "ex:r1_s1 ex:lts 3 .",
        "ex:r1_s1 ex:bikeLane false .",
        "ex:r1_s1 ex:scenic 0.5 .",
        "ex:r1_s1 ex:closed false .",
        "ex:r1_s2 ex:lengthKm 1.8 .",
        "ex:r1_s2 ex:gainM 15 .",
        "ex:r1_s2 ex:lts 2 .",
        "ex:r1_s2 ex:bikeLane true .",
        "ex:r1_s2 ex:scenic 0.6 .",
        "ex:r1_s2 ex:closed false .",
        "ex:r1_s3 ex:lengthKm 1.4 .",
        "ex:r1_s3 ex:gainM 10 .",
        "ex:r1_s3 ex:lts 2 .",
        "ex:r1_s3 ex:bikeLane true .",
        "ex:r1_s3 ex:scenic 0.7 .",
        "ex:r1_s3 ex:closed false .",
        # r2: a bit longer, safer, more scenic, mild tailwind
        "ex:r2_s1 ex:lengthKm 2.2 .",
        "ex:r2_s1 ex:gainM 25 .",
        "ex:r2_s1 ex:lts 2 .",
        "ex:r2_s1 ex:bikeLane true .",
        "ex:r2_s1 ex:scenic 0.7 .",
        "ex:r2_s1 ex:closed false .",
        "ex:r2_s2 ex:lengthKm 2.0 .",
        "ex:r2_s2 ex:gainM 20 .",
        "ex:r2_s2 ex:lts 1 .",
        "ex:r2_s2 ex:bikeLane true .",
        "ex:r2_s2 ex:scenic 0.8 .",
        "ex:r2_s2 ex:closed false .",
        "ex:r2_s3 ex:lengthKm 1.9 .",
        "ex:r2_s3 ex:gainM 15 .",
        "ex:r2_s3 ex:lts 1 .",
        "ex:r2_s3 ex:bikeLane true .",
        "ex:r2_s3 ex:scenic 0.9 .",
        "ex:r2_s3 ex:closed false .",
        # r3: flat and direct but one closure
        "ex:r3_s1 ex:lengthKm 1.9 .",
        "ex:r3_s1 ex:gainM 5 .",
        "ex:r3_s1 ex:lts 3 .",
        "ex:r3_s1 ex:bikeLane false .",
        "ex:r3_s1 ex:scenic 0.4 .",
        "ex:r3_s1 ex:closed false .",
        "ex:r3_s2 ex:lengthKm 1.5 .",
        "ex:r3_s2 ex:gainM 5 .",
        "ex:r3_s2 ex:lts 4 .",
        "ex:r3_s2 ex:bikeLane false .",
        "ex:r3_s2 ex:scenic 0.3 .",
        "ex:r3_s2 ex:closed true .",
        "ex:r3_s3 ex:lengthKm 1.2 .",
        "ex:r3_s3 ex:gainM 5 .",
        "ex:r3_s3 ex:lts 3 .",
        "ex:r3_s3 ex:bikeLane false .",
        "ex:r3_s3 ex:scenic 0.4 .",
        "ex:r3_s3 ex:closed false .",
    ]
    return "\n".join(ln)

DYNAMIC_TTL = make_dynamic_ttl()

# -----------------------------------------------------------------------------
# Behaviors as N3 (valid triple-only math built-ins)
# -----------------------------------------------------------------------------
RULES_N3 = """@prefix ex:   <http://example.org/bike#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Score = wTime*timeN + wSafety*riskN + wGain*gainN + wScenic*(1 - scenicN)
{ ?r ex:timeN ?tN .
  ?r ex:riskN ?sN .
  ?r ex:gainN ?gN .
  ?r ex:scenicN ?scN .
  ex:policy ex:wTime   ?wT .
  ex:policy ex:wSafety ?wS .
  ex:policy ex:wGain   ?wG .
  ex:policy ex:wScenic ?wSc .
  ( 1 ?scN ) math:difference ?invSc .
  ( ?wT ?tN )  math:product ?tTerm .
  ( ?wS ?sN )  math:product ?sTerm .
  ( ?wG ?gN )  math:product ?gTerm .
  ( ?wSc ?invSc ) math:product ?scTerm .
  ( ?tTerm ?sTerm ) math:sum ?ts .
  ( ?gTerm ?scTerm ) math:sum ?gs .
  ( ?ts ?gs ) math:sum ?score .
} => { ?r ex:score ?score } .

# Feasibility: any closed segment makes route infeasible
{ ?r ex:hasSegment ?seg . ?seg ex:closed true . } => { ?r ex:feasible false } .
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
        """Split a line into statements at '.' outside quotes/IRIs (not within numbers like 2.0)."""
        stmts, buf = [], []
        in_quotes = False
        in_angle = False
        for i, ch in enumerate(line):
            if ch == '"' and not in_angle:
                in_quotes = not in_quotes
                buf.append(ch); continue
            if ch == '<' and not in_quotes:
                in_angle = True; buf.append(ch); continue
            if ch == '>' and not in_quotes:
                in_angle = False; buf.append(ch); continue
            if ch == '.' and not in_quotes and not in_angle:
                # Look ahead to ensure this '.' is a terminator (next non-space is not a digit of the same token)
                j = i + 1
                while j < len(line) and line[j].isspace():
                    j += 1
                # Terminate statement
                stmts.append("".join(buf).strip())
                buf = []
                continue
            buf.append(ch)
        if buf and "".join(buf).strip():
            stmts.append("".join(buf).strip())
        # Drop empties
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

        # Support either one triple per line, or multiple separated by '.'
        candidates = split_statements(line)

        for stmt in candidates:
            # Tokenize (space-splitting outside quotes)
            toks, buf = [], ""
            in_quotes = False
            for ch in stmt.strip():
                if ch == '"':
                    in_quotes = not in_quotes
                    buf += ch
                elif ch == ' ' and not in_quotes:
                    if buf:
                        toks.append(buf); buf = ""
                else:
                    buf += ch
            if buf:
                toks.append(buf)
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
    # static bindings (baked into closure)
    wT = float(get1(S, EX+"policy", EX+"wTime",   0.5))
    wS = float(get1(S, EX+"policy", EX+"wSafety", 0.3))
    wG = float(get1(S, EX+"policy", EX+"wGain",   0.15))
    wSc= float(get1(S, EX+"policy", EX+"wScenic", 0.05))

    base_speed   = float(get1(S, EX+"rider", EX+"baseSpeedKmh", 18.0))
    rain_factor  = float(get1(S, EX+"rider", EX+"rainSpeedFactor", 0.90))
    lts_slow3    = float(get1(S, EX+"rider", EX+"ltsSlowdown3", 0.95))

    def route_metrics(D) -> Dict[str, Any]:
        # Collect routes by presence of ex:hasSegment
        routes = [r for r, props in D.items() if EX+"hasSegment" in props]
        metrics = {}
        raining = bool(get1(D, EX+"weather", EX+"rain", False))

        for r in routes:
            segs = D[r][EX+"hasSegment"]
            wind_frac = float(get1(D, r, EX+"windFrac", 0.0))
            eff_speed_base = base_speed * (1 - wind_frac) * (rain_factor if raining else 1.0)

            length_km = 0.0
            gain_m = 0.0
            risk_km = 0.0
            scenic_weighted = 0.0
            scenic_weight = 0.0
            time_min = 0.0
            feasible = True
            max_lts = 1

            seg_details = []

            for s in segs:
                L = float(get1(D, s, EX+"lengthKm", 0.0))
                G = float(get1(D, s, EX+"gainM", 0.0))
                lts = int(get1(D, s, EX+"lts", 2))
                lane = bool(get1(D, s, EX+"bikeLane", False))
                sc  = float(get1(D, s, EX+"scenic", 0.5))
                closed = bool(get1(D, s, EX+"closed", False))
                if closed:
                    feasible = False
                # speed on segment
                seg_speed = eff_speed_base * (lts_slow3 if lts >= 3 else 1.0)
                seg_time_min = (L / max(seg_speed, 1e-6)) * 60.0

                length_km += L
                gain_m    += G
                # risk proxy: L * normalized LTS (1..4 -> 0..1)
                risk_km   += L * ((lts-1)/3.0)
                scenic_weighted += sc * L
                scenic_weight   += L
                time_min  += seg_time_min
                max_lts = max(max_lts, lts)

                seg_details.append({
                    "segment": s, "length_km": L, "gain_m": G, "lts": lts,
                    "bike_lane": lane, "scenic": sc, "closed": closed,
                    "time_min": seg_time_min, "seg_speed_kmh": seg_speed
                })

            scenic_avg = (scenic_weighted / scenic_weight) if scenic_weight > 0 else 0.5
            metrics[r] = {
                "route": r, "feasible": feasible, "time_min": time_min,
                "length_km": length_km, "gain_m": gain_m, "risk_km": risk_km,
                "scenic_avg": scenic_avg, "max_lts": max_lts, "wind_frac": wind_frac,
                "raining": raining, "segments": seg_details
            }
        return metrics

    def score_and_rank(D):
        M = route_metrics(D)
        routes = list(M.keys())
        # at least one route must exist
        assert routes, "No routes found in dynamic RDF"

        feas = [r for r in routes if M[r]["feasible"]]
        assert feas, "No feasible routes (all have closures)."

        # Normalize features across feasible routes
        times  = [M[r]["time_min"]  for r in feas]
        risks  = [M[r]["risk_km"]   for r in feas]
        gains  = [M[r]["gain_m"]    for r in feas]
        scenics= [M[r]["scenic_avg"]for r in feas]

        tN = dict(zip(feas, norm(times)))
        sN = dict(zip(feas, norm(risks)))
        gN = dict(zip(feas, norm(gains)))
        scN= dict(zip(feas, norm(scenics)))

        # composite: wT*timeN + wS*riskN + wG*gainN + wSc*(1 - scenicN)
        results = []
        for r in routes:
            m = M[r]
            if not m["feasible"]:
                score = float("+inf")
                note = "infeasible (closure)"
            else:
                score = (wT*tN[r] + wS*sN[r] + wG*gN[r] + wSc*(1.0 - scN[r]))
                # explanation
                reasons = []
                if m["raining"]: reasons.append("rain slow-down")
                if abs(m["wind_frac"]) > 0.001:
                    reasons.append("tailwind" if m["wind_frac"] < 0 else "headwind")
                if m["max_lts"] >= 3: reasons.append("some higher-stress links")
                if gN[r] <= 0.15: reasons.append("low climb")
                if sN[r] <= 0.15: reasons.append("safer")
                if tN[r] <= 0.15: reasons.append("fast")
                if scN[r] >= 0.85: reasons.append("scenic")
                note = "; ".join(reasons)
            results.append({
                "route": r, "feasible": m["feasible"], "score": score, "note": note,
                **m, "timeN": tN.get(r, None), "riskN": sN.get(r, None),
                "gainN": gN.get(r, None), "scenicN": scN.get(r, None),
            })

        ranked = sorted(results, key=lambda x: x["score"])
        return ranked, results, {"wTime":wT,"wSafety":wS,"wGain":wG,"wScenic":wSc}

    return score_and_rank

# Build specialized driver
driver = make_driver(S)

# -----------------------------------------------------------------------------
# Execute
# -----------------------------------------------------------------------------
ranked, all_results, weights = driver(D)

# Basic checks
feasible = [r for r in ranked if r["feasible"]]
assert feasible, "No feasible routes after scoring."

best = feasible[0]
# Monotonicity wrt time-weight: increasing wTime should not increase chosen time
def rerun_with_time_bonus(delta=0.20):
    S_mod = {k:{kk:list(vv) for kk,vv in props.items()} for k,props in S.items()}
    S_mod[EX+"policy"][EX+"wTime"] = [min(0.99, weights["wTime"] + delta)]
    # renormalize the other weights proportionally (keep total ~ 1.0)
    remain = 1.0 - S_mod[EX+"policy"][EX+"wTime"][0]
    others = [weights["wSafety"], weights["wGain"], weights["wScenic"]]
    tot_others = sum(others)
    scale = remain / tot_others if tot_others > 0 else 0.0
    S_mod[EX+"policy"][EX+"wSafety"] = [weights["wSafety"] * scale]
    S_mod[EX+"policy"][EX+"wGain"]   = [weights["wGain"]   * scale]
    S_mod[EX+"policy"][EX+"wScenic"] = [weights["wScenic"] * scale]
    ranked2, _, _ = make_driver(S_mod)(D)
    best2 = [r for r in ranked2 if r["feasible"]][0]
    return best2

best_after = rerun_with_time_bonus(0.20)
assert best_after["time_min"] <= best["time_min"] + 1e-9, \
    "Increasing time weight should not produce a slower chosen route."

# Closure sensitivity: close a segment on the chosen route → planner must pick another route if available
def close_one_segment_and_rerun():
    # Find a segment of the best route and mark closed
    best_route = best["route"]
    segs = D[best_route][EX+"hasSegment"]
    # Copy D
    D_mod = {k:{kk:list(vv) for kk,vv in props.items()} for k,props in D.items()}
    # Mark first segment closed
    s0 = segs[0]
    D_mod[s0][EX+"closed"] = [True]
    ranked2, _, _ = driver(D_mod)
    feas2 = [r for r in ranked2 if r["feasible"]]
    return ranked2, feas2

ranked_closed, feas_closed = close_one_segment_and_rerun()
if feas_closed:  # if any feasible remains, ensure it's not the original route
    assert feas_closed[0]["route"] != best["route"], \
        "If the best route becomes partially closed, planner should switch if any alternative feasible route exists."

# -----------------------------------------------------------------------------
# Emit routes.csv (score breakdown) + machine- and human-readable traces
# -----------------------------------------------------------------------------
csv_path = os.path.join(BASE, "routes.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow([
        "rank","route","feasible",
        "time_min","length_km","gain_m","risk_km","scenic_avg",
        "timeN","riskN","gainN","scenicN","score","note"
    ])
    for i,r in enumerate(ranked, start=1):
        w.writerow([
            i, r["route"].split("#")[-1], r["feasible"],
            f"{r['time_min']:.1f}", f"{r['length_km']:.2f}", int(r["gain_m"]), f"{r['risk_km']:.2f}", f"{r['scenic_avg']:.2f}",
            (None if r["timeN"]   is None else f"{r['timeN']:.3f}"),
            (None if r["riskN"]   is None else f"{r['riskN']:.3f}"),
            (None if r["gainN"]   is None else f"{r['gainN']:.3f}"),
            (None if r["scenicN"] is None else f"{r['scenicN']:.3f}"),
            (None if math.isinf(r["score"]) else f"{r['score']:.3f}"),
            r["note"]
        ])

# Machine-readable trace (JSON)
trace = {
    "weights": weights,
    "chosen": best["route"],
    "routes": [
        {
            "route": r["route"],
            "feasible": r["feasible"],
            "time_min": r["time_min"],
            "length_km": r["length_km"],
            "gain_m": r["gain_m"],
            "risk_km": r["risk_km"],
            "scenic_avg": r["scenic_avg"],
            "timeN": r["timeN"], "riskN": r["riskN"], "gainN": r["gainN"], "scenicN": r["scenicN"],
            "score": (None if math.isinf(r["score"]) else r["score"]),
            "wind_frac": r["wind_frac"], "raining": r["raining"],
            "segments": r["segments"],
            "note": r["note"]
        } for r in ranked
    ]
}
with open(os.path.join(BASE, "trace.json"), "w", encoding="utf-8") as jf:
    json.dump(trace, jf, indent=2)

# Human-readable “Reason why”
with open(os.path.join(BASE, "reason-why.txt"), "w", encoding="utf-8") as tf:
    tf.write("Reason why / explanation summary — BikeTrip\n")
    tf.write("-----------------------------------------\n")
    tf.write(f"Weights: wTime={weights['wTime']}, wSafety={weights['wSafety']}, "
             f"wGain={weights['wGain']}, wScenic={weights['wScenic']}\n")
    tf.write(f"Chosen route: {best['route']}\n")
    tf.write(f"Time/Length/Gain/Risk/Scenic: {best['time_min']:.1f} min, "
             f"{best['length_km']:.2f} km, {int(best['gain_m'])} m, "
             f"{best['risk_km']:.2f} risk-km, scenic {best['scenic_avg']:.2f}\n")
    tf.write(f"Notes: {best['note']}\n\n")
    tf.write("All routes (ranked):\n")
    for i,r in enumerate([x for x in ranked if x["feasible"]], start=1):
        tf.write(f"  {i}. {r['route']}  score={r['score']:.3f}  "
                 f"time={r['time_min']:.1f}m  risk={r['risk_km']:.2f}  "
                 f"gain={int(r['gain_m'])}m  scenic={r['scenic_avg']:.2f}\n")

print("ALL TESTS PASSED")
print("Artifacts in:", BASE)
print("CSV:", csv_path)

