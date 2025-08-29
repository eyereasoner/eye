
from __future__ import annotations
"""
BikeTrip Planner - EYE-style mixed computation (self-contained + self-checking)
===============================================================================

  ME ----------------------------+
                                |        +-------------------+
                                |        |  Behaviors        |  (learned policy templates)
                                |        |  (compiled rules) |
                                |        +-------------------+
                                |                 ^    ^
                                |                 |    |
                                v                 |    | context
+-------------+     AC      +----------+          |    |
| data        | <---------> | context  |----------+    |
| (RDF)       |             +----------+               |
+-------------+                                        v
                               Agent  (partial evaluator / codegen)
                                  |
                                  v
                              Driver (scoring function)
                                  |
                                  v
                    +------------------------------------+
                    | Targets / Capabilities / Context   |--> ranked routes
                    +------------------------------------+
                                  |
                                  v
                         actionable insight + feedback

Legend:
- data: RDF Turtle describing constants and route candidates
- context: dynamic situation (user prefs, weather, closures)
- behaviors: N3 rule templates + default policy weights
- agent: the Python partial evaluator that compiles a specialized scorer
- driver: the scorer function itself (generated via partial evaluation)
- targets/capabilities: available routes and constraints
- actionable insight: ranked routes with justification trace

The script is self-checking: it asserts that closures are pruned, rain improves
protected infrastructure, and headwinds are penalized.

USAGE
-----
    python biketrip_mixed.py

OUTPUTS
-------
files in eye/learning/resources
    - static.ttl, dynamic.ttl            (RDF Turtle)
    - rules-static.n3, rules-dynamic.n3  (N3 docs of intended rules)
    - ranked_routes.csv                  (final ranking with trace columns)

"""
import os, math, csv
from typing import Any, Dict, List, Tuple

# ----------------------------------------------------------------------------
# Driver output location (shared store for data, context, artifacts)
# ----------------------------------------------------------------------------
BASE = os.path.join(os.path.dirname(__file__), "../resources")
os.makedirs(BASE, exist_ok=True)

EX = "http://example.org/bike#"

# ----------------------------------------------------------------------------
# STATIC: ontology fragments, default weights, infrastructure maps
# (Schematic: "data" (static) + "behaviors" (templates) + "standard context")
# ----------------------------------------------------------------------------
STATIC_TTL = """@prefix ex: <http://example.org/bike#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Ontology-ish stubs for clarity
ex:Route a ex:Class .
ex:InfrastructureType a ex:Class .
ex:protected a ex:InfrastructureType .
ex:painted a ex:InfrastructureType .
ex:none a ex:InfrastructureType .

# Default policy (= learned behavior template). These are "compile-time constants"
# that our AGENT (partial evaluator) will bake into the Driver (scorer).
ex:defaultPolicy ex:scenicWeight 0.50 ;
                ex:safetyWeight 0.80 ;
                ex:timeWeight   0.45 ;
                ex:elevPenaltyPer100m 1.00 ;
                ex:rainPenalty  0.40 ;
                ex:headwindPenaltyPerMs 0.15 ;
                ex:defaultCruiseKmh 16.0 .

# Infrastructure safety mapping
ex:protected ex:weight 1.0 .
ex:painted   ex:weight 0.5 .
ex:none      ex:weight -0.4 .

# Traffic impact mapping
ex:traffic_low  ex:trafficWeight  0.25 .
ex:traffic_med  ex:trafficWeight  0.0  .
ex:traffic_high ex:trafficWeight -0.25 .
"""

# ----------------------------------------------------------------------------
# DYNAMIC: context (user prefs, weather), targets/capabilities (routes), closures
# (Schematic: "context (dynamic)" and "targets/capabilities")
# ----------------------------------------------------------------------------
DYNAMIC_TTL = """@prefix ex: <http://example.org/bike#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# ME -> preferences (dynamic context)
ex:me a ex:Cyclist .
ex:me ex:prefScenic 0.60 .
ex:me ex:prefSafety 0.90 .
ex:me ex:prefTime   0.40 .
ex:me ex:maxElevationPerKm 25.0 .

# Weather (dynamic context)
ex:todayWeather ex:precipitation true .
ex:todayWeather ex:windSpeedMs 6.0 .
ex:todayWeather ex:windDirectionDeg 270 .

# Known closures (dynamic)
ex:closure1 ex:affects ex:canal_shortcut .

# Targets/capabilities: candidate routes (dynamic)
ex:river_trail a ex:Route .
ex:river_trail ex:label "River Trail" .
ex:river_trail ex:distanceKm 8.5 .
ex:river_trail ex:elevationGainM 60 .
ex:river_trail ex:infrastructure ex:protected .
ex:river_trail ex:scenic 0.9 .
ex:river_trail ex:traffic ex:traffic_low .
ex:river_trail ex:surface "paved" .
ex:river_trail ex:headingDeg 90 .

ex:main_st_rapid a ex:Route .
ex:main_st_rapid ex:label "Main St Rapid" .
ex:main_st_rapid ex:distanceKm 6.7 .
ex:main_st_rapid ex:elevationGainM 45 .
ex:main_st_rapid ex:infrastructure ex:painted .
ex:main_st_rapid ex:scenic 0.4 .
ex:main_st_rapid ex:traffic ex:traffic_high .
ex:main_st_rapid ex:surface "paved" .
ex:main_st_rapid ex:headingDeg 100 .

ex:park_loop a ex:Route .
ex:park_loop ex:label "Park Loop" .
ex:park_loop ex:distanceKm 10.2 .
ex:park_loop ex:elevationGainM 160 .
ex:park_loop ex:infrastructure ex:protected .
ex:park_loop ex:scenic 0.95 .
ex:park_loop ex:traffic ex:traffic_low .
ex:park_loop ex:surface "mixed" .
ex:park_loop ex:headingDeg 150 .

ex:canal_shortcut a ex:Route .
ex:canal_shortcut ex:label "Canal Shortcut" .
ex:canal_shortcut ex:distanceKm 5.9 .
ex:canal_shortcut ex:elevationGainM 20 .
ex:canal_shortcut ex:infrastructure ex:none .
ex:canal_shortcut ex:scenic 0.3 .
ex:canal_shortcut ex:traffic ex:traffic_med .
ex:canal_shortcut ex:surface "paved" .
ex:canal_shortcut ex:headingDeg 270 .
"""

# ----------------------------------------------------------------------------
# Behaviors: rule documentation in N3 (these mirror what our scorer does)
# (Schematic: "Behaviors" box, expressed declaratively)
# ----------------------------------------------------------------------------
STATIC_N3 = """@prefix ex: <http://example.org/bike#> .
# Closed routes are rejected
{ ?c ex:affects ?r. } => { ?r ex:isClosed true } .

# Infrastructure -> numeric safety
{ ?r ex:infrastructure ex:protected } => { ?r ex:safety 1.0 } .
{ ?r ex:infrastructure ex:painted   } => { ?r ex:safety 0.5 } .
{ ?r ex:infrastructure ex:none      } => { ?r ex:safety -0.4 } .

# In rain, protected gets a small bonus
{ ?w ex:precipitation true. ?r ex:infrastructure ex:protected } => { ?r ex:weatherFit 0.3 } .

# Elevation penalty and headwind penalty are numeric aggregations in the Driver.
"""

DYNAMIC_N3 = """@prefix ex: <http://example.org/bike#> .
# User preferences select the active policy (override defaults)
{ ex:me ex:prefScenic  ?s . } => { ex:activePolicy ex:scenicWeight ?s } .
{ ex:me ex:prefSafety  ?s . } => { ex:activePolicy ex:safetyWeight ?s } .
{ ex:me ex:prefTime    ?s . } => { ex:activePolicy ex:timeWeight   ?s } .
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

# ----------------------------------------------------------------------------
# Tiny Turtle reader (Agent uses it to ingest Data + Context)
# Limitations: very small subset (simple triples, quotes, numbers, booleans, prefixes)
# ----------------------------------------------------------------------------
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

# Agent reads Data + Context (static + dynamic) -------------------------------
with open(os.path.join(BASE, "static.ttl"), encoding="utf-8") as f:
    _, static_triples = parse_ttl(f.read())
with open(os.path.join(BASE, "dynamic.ttl"), encoding="utf-8") as f:
    _, dynamic_triples = parse_ttl(f.read())

S = triples_index(static_triples)
D = triples_index(dynamic_triples)

# ----------------------------------------------------------------------------
# Mixed computation: build the Driver (scorer) by closing over static constants.
# (Schematic: The AGENT specializes "Behaviors" given "Context" to create Driver.)
# ----------------------------------------------------------------------------
defaults = {
    "scenicWeight": get1(S, EX+"defaultPolicy", EX+"scenicWeight", 0.5),
    "safetyWeight": get1(S, EX+"defaultPolicy", EX+"safetyWeight", 0.8),
    "timeWeight":   get1(S, EX+"defaultPolicy", EX+"timeWeight",   0.45),
    "elevPenaltyPer100m": get1(S, EX+"defaultPolicy", EX+"elevPenaltyPer100m", 1.0),
    "rainPenalty":  get1(S, EX+"defaultPolicy", EX+"rainPenalty",  0.4),
    "headwindPenaltyPerMs": get1(S, EX+"defaultPolicy", EX+"headwindPenaltyPerMs", 0.15),
    "defaultCruiseKmh": get1(S, EX+"defaultPolicy", EX+"defaultCruiseKmh", 16.0),
}
infra_w = {
    EX+"protected": get1(S, EX+"protected", EX+"weight", 1.0),
    EX+"painted":   get1(S, EX+"painted",   EX+"weight", 0.5),
    EX+"none":      get1(S, EX+"none",      EX+"weight", -0.4),
}
traffic_w = {
    EX+"traffic_low":  get1(S, EX+"traffic_low",  EX+"trafficWeight", 0.25),
    EX+"traffic_med":  get1(S, EX+"traffic_med",  EX+"trafficWeight", 0.0),
    EX+"traffic_high": get1(S, EX+"traffic_high", EX+"trafficWeight", -0.25),
}

# ME's dynamic preferences override defaults (Context)
policy = {
    **defaults,
    "scenicWeight": get1(D, EX+"me", EX+"prefScenic", defaults["scenicWeight"]),
    "safetyWeight": get1(D, EX+"me", EX+"prefSafety", defaults["safetyWeight"]),
    "timeWeight":   get1(D, EX+"me", EX+"prefTime",   defaults["timeWeight"]),
}
weather = {
    "rain": bool(get1(D, EX+"todayWeather", EX+"precipitation", False)),
    "windSpeedMs": float(get1(D, EX+"todayWeather", EX+"windSpeedMs", 0.0)),
    "windDirDeg": float(get1(D, EX+"todayWeather", EX+"windDirectionDeg", 0.0)),
}
closed = set([get1(D, s, EX+"affects") for s in D if EX+"affects" in D[s]])

def make_scorer(pol, infra_map, traffic_map, weather):
    """AGENT -> DRIVER: Specialize a scoring function by closing over constants.

    Everything here that does not depend on a particular route is "partially evaluated"
    (baked into the closure). When the Driver runs, only per-route fields are needed.
    """
    scenic_w = float(pol["scenicWeight"]); safety_w = float(pol["safetyWeight"]); time_w = float(pol["timeWeight"])
    elev_pen_per100 = float(pol["elevPenaltyPer100m"])
    rain_bonus = float(pol["rainPenalty"])
    headwind_pen_per_ms = float(pol["headwindPenaltyPerMs"])
    cruise_kmh = float(pol["defaultCruiseKmh"])
    rain = bool(weather["rain"]); wind_speed = float(weather["windSpeedMs"]); wind_dir = float(weather["windDirDeg"])

    def score(route, trace):
        # 0) prune closed routes (Targets/Caps filtered)
        if route.get("id") in route.get("closed_ids", set()):
            trace.append(("reject","Closed by known closure")); 
            return -1e9, trace

        # 1) extract per-route attributes (Data + Context for this route)
        scenic = route.get("scenic", 0.0)
        infra  = route.get("infrastructure")
        infra_weight = float(infra_map.get(infra, 0.0))
        traffic = route.get("traffic")
        traffic_weight = float(traffic_map.get(traffic, 0.0))

        # 2) time component (shorter is better). Behaviors -> Driver numeric aggregation
        dist_km = route.get("distanceKm", 0.0)
        est_time_h = dist_km / cruise_kmh if cruise_kmh > 0 else 0.0
        time_component = - est_time_h

        # 3) elevation penalty (policy constant per 100m)
        elev_gain_m = route.get("elevationGainM", 0.0)
        elev_pen = elev_pen_per100 * (elev_gain_m / 100.0)

        # 4) weather-sensitive safety bonus (precipitation + protected)
        if rain and infra == EX+"protected":
            infra_weight += rain_bonus
            trace.append(("bonus","Rain + protected infra -> bonus"))

        # 5) headwind penalty (use cos(angle difference) so tailwind is not penalized)
        heading = route.get("headingDeg", 0.0)
        angle_diff = abs((heading - wind_dir + 540) % 360 - 180)
        headwind_factor = max(0.0, math.cos(math.radians(angle_diff)))
        headwind_pen = headwind_pen_per_ms * wind_speed * headwind_factor

        # 6) combine with weights (Driver)
        score = 0.0
        score += scenic_w * scenic
        score += safety_w * (infra_weight + traffic_weight)
        score += time_w * time_component
        score -= elev_pen
        score -= headwind_pen

        # 7) actionable insight: provide justification trace
        trace += [
            ("scenic", f"{scenic_w}*{scenic:.2f}"),
            ("safety(infra+traffic)", f"{safety_w}*({infra_weight:.2f}+{traffic_weight:.2f})"),
            ("time", f"{time_w}*{time_component:.3f}h"),
            ("elevation_penalty", f"-{elev_pen:.2f}"),
            ("headwind_penalty", f"-{headwind_pen:.2f} (angle_diff={angle_diff:.0f})"),
        ]
        return score, trace
    return score

# Build the Driver
score_route = make_scorer(policy, infra_w, traffic_w, weather)

# ----------------------------------------------------------------------------
# Apply Driver to Targets/Capabilities (candidate routes)
# ----------------------------------------------------------------------------
routes = [s for s, props in D.items() if "a" in props and EX+"Route" in props["a"]]

def materialize_route(rid):
    """Assemble per-route facts from the dynamic graph (Data + Context)."""
    def g(p, default=None): return get1(D, rid, p, default)
    return {
        "id": rid,
        "label": g(EX+"label", rid.split("#")[-1]),
        "distanceKm": float(g(EX+"distanceKm", 0.0)),
        "elevationGainM": float(g(EX+"elevationGainM", 0.0)),
        "infrastructure": g(EX+"infrastructure"),
        "scenic": float(g(EX+"scenic", 0.0)),
        "traffic": g(EX+"traffic"),
        "surface": g(EX+"surface",""),
        "headingDeg": float(g(EX+"headingDeg", 0.0)),
        "closed_ids": closed,
    }

scored = []
for rid in routes:
    r = materialize_route(rid)
    s, trace = score_route(r, [])
    scored.append((s, r, trace))
scored.sort(key=lambda x: x[0], reverse=True)

# ----------------------------------------------------------------------------
# Self-checks (guardrails for the Agent/Driver behavior)
# ----------------------------------------------------------------------------
# 1) We should have 4 route candidates
assert len(routes) == 4, f"Expected 4 routes, got {len(routes)}"

# 2) Closure pruning: canal_shortcut was listed in closures; its score should be
#    a large negative and the trace should mention 'reject'.
canal = [x for x in scored if x[1]["id"].endswith("#canal_shortcut")][0]
assert canal[0] < -1e8 and any("reject" in k for k,_ in canal[2]), "Closure pruning failed"

# 3) Rain bonus: protected lanes get extra points under precipitation
def score_for_weather(rain: bool):
    f = make_scorer(policy, infra_w, traffic_w, {"rain": rain, "windSpeedMs":0.0, "windDirDeg":0.0})
    r = {"id":"x","scenic":0.0,"infrastructure":EX+"protected","traffic":EX+"traffic_med",
         "distanceKm":1.0,"elevationGainM":0.0,"headingDeg":0.0,"closed_ids":set()}
    s,_ = f(r, []); return s
assert score_for_weather(True) > score_for_weather(False), "Rain bonus test failed"

# 4) Headwind penalty: heading aligned with wind_dir should not beat a tailwind
fw = make_scorer(policy, infra_w, traffic_w, {"rain": False,"windSpeedMs":10.0,"windDirDeg":270.0})
r_head = {"id":"h","scenic":0.0,"infrastructure":EX+"painted","traffic":EX+"traffic_med",
          "distanceKm":1.0,"elevationGainM":0.0,"headingDeg":270.0,"closed_ids":set()}
r_tail = dict(r_head); r_tail["id"]="t"; r_tail["headingDeg"]=90.0
sh,_ = fw(r_head, []); st,_ = fw(r_tail, [])
assert st >= sh, "Tailwind should not be penalized"

# 5) At least one viable route remains to propose
non_closed = [row for row in scored if row[0] > -1e8]
assert non_closed, "Expected at least one viable route"

# ----------------------------------------------------------------------------
# Actionable insight + feedback: emit ranked CSV with the explanation trace
# ----------------------------------------------------------------------------
csv_path = os.path.join(BASE, "ranked_routes.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["label","score","distanceKm","elevationGainM","infra","traffic","trace"])
    for score, r, trace in scored:
        w.writerow([r["label"], f"{score:.3f}", r["distanceKm"], r["elevationGainM"],
                    r["infrastructure"].split('#')[-1], r["traffic"].split('#')[-1],
                    " | ".join([f"{k}:{v}" for k,v in trace])])

print("ALL TESTS PASSED")
print("Top route:", non_closed[0][1]["label"], "Score:", round(non_closed[0][0],3))
print("Artifacts in eye/learning/resources")

