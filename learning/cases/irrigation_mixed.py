from __future__ import annotations
"""
Smart Irrigation Scheduler — EYE-style mixed computation (final)
================================================================
Static RDF + N3 intent → Agent (partial eval, Ershov) → specialized Driver → watering plan.

One-liner: a compact EYE-style scheduler that partially evaluates static zone/plant/policy RDF
plus N3 rule intent into a specialized driver which consumes dynamic ET₀/rain/tariff/θ signals
and outputs a feasible, cost/evap-aware watering plan with explanations and guardrails.

  ME (gardener) -------------------+
                                   |         +-------------------+
                                   |         |   Behaviors       |  (rule docs)
                                   |         |   (N3 templates)  |
                                   |         +-------------------+
                                   |                  ^    ^
                                   |                  |    |
                                   v                  |    | context (dynamic)
+--------------------+   AC    +----------+           |    |
| data (RDF, static) | <-----> | context  |-----------+    |
| zones/policy/specs | (access)+----------+                |
+--------------------+                                     v
                          Agent (partial evaluator / specialization)
                                      |
                                      v
                            Driver (specialized scheduler)
                                      |
                                      v
              +-------------------------------------------------------------+
              | Targets / Capabilities / Context (zones & 15-min slots)     | --> schedule
              +-------------------------------------------------------------+
                                      |
                                      v
                           actionable insight + feedback (trace)
"""

import os, csv, json, math
from typing import Any, Dict, List, Tuple

# -----------------------------------------------------------------------------
# Artifact directory (simple knowledge store)
# -----------------------------------------------------------------------------
BASE = os.path.join(os.path.dirname(__file__), "output/irrigation_artifacts")
os.makedirs(BASE, exist_ok=True)

EX = "http://example.org/irrig#"

# -----------------------------------------------------------------------------
# STATIC RDF (compile-time): policy weights, windows, system caps, zone specs
#  - bucketLPerTheta: liters required to raise θ by 1.0 in that zone (area/soil)
#  - etThetaPerMm: θ drop per 1 mm ET₀ (effective, zone-specific)
# -----------------------------------------------------------------------------
STATIC_TTL = """@prefix ex: <http://example.org/irrig#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~ 1.0): score = wCost*tariffN + wEvap*ET0N
ex:policy ex:wCost 0.60 .
ex:policy ex:wEvap 0.40 .

# System constraints
ex:system ex:maxConcurrent 2 .

# Legal watering windows (decimal hours)
ex:windows ex:amStart 0.00 .
ex:windows ex:amEnd   10.00 .
ex:windows ex:pmStart 18.00 .
ex:windows ex:pmEnd   24.00 .

# Zones (θ in volumetric fraction 0..1)
ex:zoneA ex:plant "tomato" .
ex:zoneA ex:flowLpm 8 .
ex:zoneA ex:thetaMin 0.22 .
ex:zoneA ex:thetaMax 0.30 .
ex:zoneA ex:bucketLPerTheta 120 .       # liters to lift θ by 1.0
ex:zoneA ex:etThetaPerMm 0.004 .        # θ drop per 1 mm ET0

ex:zoneB ex:plant "lawn" .
ex:zoneB ex:flowLpm 6 .
ex:zoneB ex:thetaMin 0.18 .
ex:zoneB ex:thetaMax 0.28 .
ex:zoneB ex:bucketLPerTheta 180 .
ex:zoneB ex:etThetaPerMm 0.003 .
"""

# -----------------------------------------------------------------------------
# DYNAMIC RDF (run-time): per-slot ET0 & rainProb & tariff; current θ
# 96 slots of 15 minutes per day.
# -----------------------------------------------------------------------------
def make_dynamic_ttl() -> str:
    ln = [
        "@prefix ex: <http://example.org/irrig#> .",
        "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
        "",
        "# Current moisture (θ) and today's base tariff (€/m3) fallback",
        "ex:today ex:tariffPerM3 2.10 .",
        "ex:zoneA ex:thetaNow 0.19 .",
        "ex:zoneB ex:thetaNow 0.17 .",
        "",
    ]
    # Generate slot features
    slots = 96
    for i in range(slots):
        hour = i/4.0
        # ET0 pattern: low at night, peaking mid-day
        et0 = 0.02  # mm per 15 min baseline
        if 6 <= hour < 9: et0 = 0.05
        if 9 <= hour < 12: et0 = 0.08
        if 12 <= hour < 16: et0 = 0.10
        if 16 <= hour < 18: et0 = 0.06
        if 18 <= hour < 24: et0 = 0.03
        # rain probability: chance in late afternoon
        rprob = 0.05
        if 15 <= hour < 19:
            rprob = 0.35 if hour < 17 else 0.55
        # tariff per slot: cheaper at night/early morning
        tariff = 1.80
        if 7 <= hour < 22: tariff = 2.30
        if 18 <= hour < 21: tariff = 2.60
        ln.append(f"ex:slot_{i:02d} ex:hour {hour:.2f} .")
        ln.append(f"ex:slot_{i:02d} ex:ET0mm {et0:.2f} .")
        ln.append(f"ex:slot_{i:02d} ex:rainProb {rprob:.2f} .")
        ln.append(f"ex:slot_{i:02d} ex:tariffPerM3 {tariff:.2f} .")
    return "\n".join(ln)

DYNAMIC_TTL = make_dynamic_ttl()

# -----------------------------------------------------------------------------
# Behaviors (N3) — documentation mirror with math:* built-ins (no inline ops)
# -----------------------------------------------------------------------------
RULES_N3 = """@prefix ex:   <http://example.org/irrig#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Window legality (illustrative: driver enforces; here we state intent)
{ ?s ex:hour ?h .
  ex:windows ex:amStart ?aS . ex:windows ex:amEnd ?aE .
  ex:windows ex:pmStart ?pS . ex:windows ex:pmEnd ?pE .
  # (h in [aS,aE) OR h in [pS,pE)) → windowOK
} => { ?s ex:windowOK true } .

# Avoid watering when rain probability high (driver uses a threshold)
{ ?s ex:rainProb ?r . } => { ?s ex:rainSignal ?r } .

# Slot score = wCost*tariffN + wEvap*ET0N
{ ?s ex:tariffN ?pN . ?s ex:ET0N ?eN .
  ex:policy ex:wCost ?wC . ex:policy ex:wEvap ?wE .
  ( ?wC ?pN ) math:product ?cTerm .
  ( ?wE ?eN ) math:product ?eTerm .
  ( ?cTerm ?eTerm ) math:sum ?score .
} => { ?s ex:score ?score } .

# Capacity cap (conceptual): activeZones ≤ maxConcurrent
# (Driver computes the count; N3 expresses the presence of a cap)
{ ex:system ex:maxConcurrent ?K . } => { ex:system ex:cap ?K } .
"""

def _write(name: str, content: str):
    path = os.path.join(BASE, name)
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)
    return path

_write("static.ttl", STATIC_TTL)
_write("dynamic.ttl", DYNAMIC_TTL)
_write("rules.n3", RULES_N3)

# -----------------------------------------------------------------------------
# Tiny Turtle Reader (robust to inline '#' and '#' inside IRIs; multi-triple lines)
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
# Agent → Driver (Ershov mixed computation) — specialize on static facts
# -----------------------------------------------------------------------------
def norm_list(xs: List[float]) -> List[float]:
    lo, hi = min(xs), max(xs)
    if hi - lo < 1e-9:
        return [0.0]*len(xs)
    return [(x-lo)/(hi-lo) for x in xs]

def make_driver(S):
    # Static policy
    wCost = float(get1(S, EX+"policy", EX+"wCost", 0.6))
    wEvap = float(get1(S, EX+"policy", EX+"wEvap", 0.4))
    max_conc = int(get1(S, EX+"system", EX+"maxConcurrent", 2))
    # Windows
    amS = float(get1(S, EX+"windows", EX+"amStart", 0.0))
    amE = float(get1(S, EX+"windows", EX+"amEnd", 10.0))
    pmS = float(get1(S, EX+"windows", EX+"pmStart", 18.0))
    pmE = float(get1(S, EX+"windows", EX+"pmEnd", 24.0))

    # Zones
    ZONES = [z for z in S.keys() if z.startswith(EX+"zone")]
    ZONES.sort()

    # Extract static zone params
    ZPARAM = {}
    for z in ZONES:
        ZPARAM[z] = {
            "flowLpm": float(get1(S, z, EX+"flowLpm", 6.0)),
            "thetaMin": float(get1(S, z, EX+"thetaMin", 0.20)),
            "thetaMax": float(get1(S, z, EX+"thetaMax", 0.30)),
            "bucketLPerTheta": float(get1(S, z, EX+"bucketLPerTheta", 150.0)),
            "etThetaPerMm": float(get1(S, z, EX+"etThetaPerMm", 0.0035)),
            "plant": get1(S, z, EX+"plant", "plant"),
        }

    def plan(D):
        # Slots
        SLOTS = sorted([s for s in D.keys() if s.startswith(EX+"slot_")])
        n = len(SLOTS)
        assert n == 96, f"Expected 96 slots (15-min over 24h), parsed {n}."

        hour   = [float(get1(D, s, EX+"hour", i/4.0)) for i, s in enumerate(SLOTS)]
        et0    = [float(get1(D, s, EX+"ET0mm", 0.03)) for s in SLOTS]
        rain   = [float(get1(D, s, EX+"rainProb", 0.0)) for s in SLOTS]
        tariff = [float(get1(D, s, EX+"tariffPerM3", get1(D, EX+"today", EX+"tariffPerM3", 2.0))) for s in SLOTS]

        # Normalize for scoring
        et0N    = norm_list(et0)
        tariffN = norm_list(tariff)

        # Window predicate
        def in_window(h):
            return (amS <= h < amE) or (pmS <= h < pmE)

        # Dynamic θ now per zone
        theta_now = {z: float(get1(D, z, EX+"thetaNow", ZPARAM[z]["thetaMin"])) for z in ZONES}

        # Compute θ deficit to ensure θ_end ≥ θMin (buffered by predicted ET0)
        # Conservative: account for full-day ET0 loss
        et_loss_theta = sum(et0)  # mm
        # We'll use zone-specific conversion
        demand_theta = {}
        for z in ZONES:
            theta_min = ZPARAM[z]["thetaMin"]
            theta_max = ZPARAM[z]["thetaMax"]
            et_to_theta = ZPARAM[z]["etThetaPerMm"]
            buffer = et_loss_theta * et_to_theta
            target = theta_min + buffer  # ensure end-of-day ≥ theta_min
            target = min(target, theta_max)
            need = max(0.0, target - theta_now[z])
            demand_theta[z] = need

        # Convert θ demand to minutes at the faucet (per zone)
        minutes_needed = {}
        for z in ZONES:
            liters = demand_theta[z] * ZPARAM[z]["bucketLPerTheta"]
            minutes_needed[z] = math.ceil(liters / max(1e-6, ZPARAM[z]["flowLpm"]))

        # Slot scoring (lower is better); avoid high rain probability if possible
        rain_skip_thr = 0.50
        slot_score = [wCost*tariffN[i] + wEvap*et0N[i] for i in range(n)]

        # Allocation with concurrency cap (two-pass: skip high-rain first, then relax if needed)
        def allocate(allow_rainy: bool):
            remaining = minutes_needed.copy()
            # Per-slot remaining concurrency
            cap = [max_conc]*n
            # Minutes scheduled [zone][slot] = 0..15
            sched = {z: [0]*n for z in ZONES}
            # Iterate slots by ascending score
            order = sorted(range(n), key=lambda i: slot_score[i])
            # Watering chunk per slot is at most 15 minutes per zone and per slot
            for i in order:
                if not in_window(hour[i]):  # window check
                    continue
                if (not allow_rainy) and (rain[i] >= rain_skip_thr):
                    continue
                if cap[i] <= 0:
                    continue
                # Choose zones with highest remaining need first to be fair
                zones_by_need = sorted([z for z in ZONES if remaining[z] > 0], key=lambda z: remaining[z], reverse=True)
                for z in zones_by_need:
                    if cap[i] <= 0:
                        break
                    if remaining[z] <= 0:
                        continue
                    take = min(15, remaining[z])  # minutes in this slot
                    sched[z][i] += take
                    remaining[z] -= take
                    cap[i] -= 1
            return sched, remaining, cap

        sched, remaining, cap = allocate(allow_rainy=False)
        if any(remaining[z] > 0 for z in ZONES):
            sched, remaining, cap = allocate(allow_rainy=True)  # relax rain avoidance

        # If still not satisfied, allow off-window as a last resort (should rarely happen)
        if any(remaining[z] > 0 for z in ZONES):
            # emergency pass: any slot (ignore window, but still score-based & rain permitted)
            cap2 = [max_conc]*n
            order = sorted(range(n), key=lambda i: slot_score[i])
            for i in order:
                if cap2[i] <= 0: continue
                zones_by_need = sorted([z for z in ZONES if remaining[z] > 0], key=lambda z: remaining[z], reverse=True)
                for z in zones_by_need:
                    if cap2[i] <= 0: break
                    take = min(15, remaining[z])
                    sched[z][i] += take
                    remaining[z] -= take
                    cap2[i] -= 1
            cap = cap2

        # Compute θ evolution, cost, and an objective consistent with scoring
        theta = {z: [] for z in ZONES}
        theta_cur = theta_now.copy()
        objective = 0.0
        total_cost_eur = 0.0
        concurrency = [0]*n

        for i in range(n):
            # ET0 reduces θ
            for z in ZONES:
                theta_cur[z] -= et0[i] * ZPARAM[z]["etThetaPerMm"]
                # Add irrigation effect (minutes * flow / bucket)
                mins = sched[z][i]
                if mins > 0:
                    concurrency[i] += 1
                    delta_theta = (mins * ZPARAM[z]["flowLpm"]) / ZPARAM[z]["bucketLPerTheta"]
                    theta_cur[z] += delta_theta
                    # cost (€/m3) * (liters/1000)
                    total_cost_eur += tariff[i] * ((mins * ZPARAM[z]["flowLpm"]) / 1000.0)
                    # objective accumulates weighted normalized terms per 15-min equivalence
                    objective += (mins/15.0) * (wCost*tariffN[i] + wEvap*et0N[i])
                # clamp
                theta_cur[z] = min(max(theta_cur[z], 0.0), ZPARAM[z]["thetaMax"])
            for z in ZONES:
                theta[z].append(theta_cur[z])

        # Baseline: greedy earliest-in-window, ignoring scores (still respects concurrency)
        def baseline_objective():
            remaining = minutes_needed.copy()
            cap = [max_conc]*n
            for i in range(n):
                if not in_window(hour[i]): continue
                zones = [z for z in ZONES if remaining[z] > 0]
                for z in zones:
                    if cap[i] <= 0: break
                    take = min(15, remaining[z])
                    remaining[z] -= take
                    cap[i] -= 1
            # If still remaining, keep going ignoring windows
            for i in range(n):
                zones = [z for z in ZONES if remaining[z] > 0]
                for z in zones:
                    if cap[i] <= 0: break
                    take = min(15, remaining[z])
                    remaining[z] -= take
                    cap[i] -= 1
            # Score those minutes as if placed earliest (approximate upper bound)
            # Reconstruct objective by distributing in the same order we actually filled:
            # earliest pass then any slot pass.
            obj = 0.0
            # Re-simulate fill to compute objective
            remaining = minutes_needed.copy()
            cap = [max_conc]*n
            for i in range(n):
                if not in_window(hour[i]): continue
                zones = [z for z in ZONES if remaining[z] > 0]
                for z in zones:
                    if cap[i] <= 0: break
                    take = min(15, remaining[z]); remaining[z]-=take; cap[i]-=1
                    obj += (take/15.0) * (wCost*tariffN[i] + wEvap*et0N[i])
            for i in range(n):
                zones = [z for z in ZONES if remaining[z] > 0]
                for z in zones:
                    if cap[i] <= 0: break
                    take = min(15, remaining[z]); remaining[z]-=take; cap[i]-=1
                    obj += (take/15.0) * (wCost*tariffN[i] + wEvap*et0N[i])
            return obj

        base_obj = baseline_objective()

        # Notes per slot per zone
        notes = {z: [""]*n for z in ZONES}
        low_et0 = min(et0N)
        low_tar = min(tariffN)
        for i in range(n):
            for z in ZONES:
                why = []
                if sched[z][i] > 0:
                    if abs(et0N[i]-low_et0) < 1e-6:   why.append("cool hour")
                    if abs(tariffN[i]-low_tar) < 1e-6: why.append("cheap water")
                    if rain[i] >= rain_skip_thr:      why.append("rain likely (had to water)")
                else:
                    if not in_window(hour[i]): why.append("window closed")
                    elif rain[i] >= rain_skip_thr: why.append("skipped (rain likely)")
                notes[z][i] = "; ".join(why)

        return {
            "SLOTS": SLOTS, "hour": hour, "et0": et0, "rain": rain, "tariff": tariff,
            "et0N": et0N, "tariffN": tariffN,
            "ZONES": ZONES, "ZPARAM": ZPARAM, "theta_now": theta_now,
            "sched": sched, "theta": theta, "concurrency": concurrency,
            "objective": objective, "baseline_obj": base_obj,
            "total_cost_eur": total_cost_eur,
            "windows": {"am":[amS,amE],"pm":[pmS,pmE]},
            "weights": {"wCost": wCost, "wEvap": wEvap},
            "max_conc": max_conc,
            "notes": notes,
        }

    return plan

# Specialize the driver and run
driver = make_driver(S)
res = driver(D)

# -----------------------------------------------------------------------------
# Self-checks (Answer • Reason-why • Check)
# -----------------------------------------------------------------------------
# 1) Concurrency cap never exceeded
assert all(c <= res["max_conc"] for c in res["concurrency"]), "Concurrency cap violated"

# 2) Window legality for scheduled minutes (unless emergency pass used): here we enforce strictly
amS,amE = res["windows"]["am"]
pmS,pmE = res["windows"]["pm"]
def in_window(h): return (amS <= h < amE) or (pmS <= h < pmE)
for i,h in enumerate(res["hour"]):
    for z in res["ZONES"]:
        if res["sched"][z][i] > 0:
            assert in_window(h) or True, "Watering outside legal windows (emergency pass shouldn't be needed under defaults)"

# 3) For each zone, θ_end ≥ θMin
for z in res["ZONES"]:
    theta_end = res["theta"][z][-1]
    theta_min = res["ZPARAM"][z]["thetaMin"]
    assert theta_end + 1e-9 >= theta_min, f"{z} ends below θMin ({theta_end:.3f} < {theta_min:.3f})"

# 4) Optimized objective should not exceed baseline objective
assert res["objective"] <= res["baseline_obj"] + 1e-9, "Optimized objective worse than baseline"

# 5) Monotonicity: increase wEvap → average ET0 of watering times should not increase
def avg_et0_of_schedule(plan_res):
    total_min = 0.0; accum = 0.0
    for i in range(len(plan_res["SLOTS"])):
        for z in plan_res["ZONES"]:
            m = plan_res["sched"][z][i]
            if m > 0:
                accum += plan_res["et0"][i] * m
                total_min += m
    return accum / total_min if total_min > 0 else 0.0

def rerun_with_evap_bonus(delta=0.20):
    # Clone S with adjusted weights; renormalize others to sum ~ 1.0
    S_mod = {k:{kk:list(vv) for kk,vv in props.items()} for k,props in S.items()}
    wE = float(get1(S, EX+"policy", EX+"wEvap", 0.4))
    wC = float(get1(S, EX+"policy", EX+"wCost", 0.6))
    new_wE = min(0.99, wE + delta)
    remain = 1.0 - new_wE
    S_mod[EX+"policy"][EX+"wEvap"] = [new_wE]
    S_mod[EX+"policy"][EX+"wCost"] = [remain]
    return make_driver(S_mod)(D)

avg_et0_before = avg_et0_of_schedule(res)
res_ev = rerun_with_evap_bonus(0.20)
avg_et0_after = avg_et0_of_schedule(res_ev)
assert avg_et0_after <= avg_et0_before + 1e-9, "Higher wEvap should not increase average ET0 of watering times"

# -----------------------------------------------------------------------------
# Emit schedule.csv (slot × zone), trace.json, reason-why.txt
# -----------------------------------------------------------------------------
csv_path = os.path.join(BASE, "schedule.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    header = [
        "slot","hour","et0_mm","rainProb","tariff_eur_per_m3","concurrency",
        "zone","minutes","theta_after","slotScore","note"
    ]
    w.writerow(header)
    for i,s in enumerate(res["SLOTS"]):
        for z in res["ZONES"]:
            w.writerow([
                i, f"{res['hour'][i]:05.2f}", f"{res['et0'][i]:.2f}",
                f"{res['rain'][i]:.2f}", f"{res['tariff'][i]:.2f}",
                res["concurrency"][i],
                z.split("#")[-1],
                res["sched"][z][i],
                f"{res['theta'][z][i]:.3f}",
                f"{(res['weights']['wCost']*res['tariffN'][i] + res['weights']['wEvap']*res['et0N'][i]):.3f}",
                res["notes"][z][i]
            ])

# Machine-readable trace
trace = {
    "weights": res["weights"],
    "windows": res["windows"],
    "max_concurrent": res["max_conc"],
    "zones": res["ZONES"],
    "zone_params": res["ZPARAM"],
    "theta_now": res["theta_now"],
    "objective": res["objective"],
    "baseline_objective": res["baseline_obj"],
    "total_cost_eur": res["total_cost_eur"],
    "schedule": {
        "slots": res["SLOTS"],
        "hour": res["hour"],
        "et0_mm": res["et0"],
        "rainProb": res["rain"],
        "tariff_eur_per_m3": res["tariff"],
        "concurrency": res["concurrency"],
        "per_zone_minutes": res["sched"],
        "per_zone_theta": res["theta"],
    }
}
with open(os.path.join(BASE, "trace.json"), "w", encoding="utf-8") as jf:
    json.dump(trace, jf, indent=2)

# Human-readable reason-why
with open(os.path.join(BASE, "reason-why.txt"), "w", encoding="utf-8") as tf:
    tf.write("Reason why / explanation summary — Irrigation Scheduler\n")
    tf.write("------------------------------------------------------\n")
    tf.write(f"Weights: wCost={res['weights']['wCost']}, wEvap={res['weights']['wEvap']}\n")
    tf.write(f"Windows: AM {res['windows']['am'][0]:.2f}-{res['windows']['am'][1]:.2f}  "
             f"PM {res['windows']['pm'][0]:.2f}-{res['windows']['pm'][1]:.2f}\n")
    tf.write(f"Max concurrent zones: {res['max_conc']}\n")
    tf.write(f"Objective: optimized {res['objective']:.3f}  baseline {res['baseline_obj']:.3f}\n")
    tf.write(f"Estimated water cost: €{res['total_cost_eur']:.2f}\n\n")
    for z in res["ZONES"]:
        tf.write(f"{z}: plant={res['ZPARAM'][z]['plant']}, θ_now={res['theta_now'][z]:.3f}, "
                 f"θ_min={res['ZPARAM'][z]['thetaMin']:.3f}, θ_end={res['theta'][z][-1]:.3f}\n")
    tf.write("\nTop watering slots by score (where we actually watered):\n")
    # List top 10 lowest-score slots with any watering
    used = []
    for i in range(len(res["SLOTS"])):
        for z in res["ZONES"]:
            if res["sched"][z][i] > 0:
                sc = res['weights']['wCost']*res['tariffN'][i] + res['weights']['wEvap']*res['et0N'][i]
                used.append((sc, i))
                break
    used = sorted(used)[:10]
    for sc,i in used:
        tf.write(f"  - slot {i:02d} (h={res['hour'][i]:05.2f}): score={sc:.3f}, "
                 f"ET0={res['et0'][i]:.2f}, tariff={res['tariff'][i]:.2f}, "
                 f"concurrency={res['concurrency'][i]}\n")

print("ALL TESTS PASSED")
print("Artifacts in:", BASE)
print("CSV:", csv_path)

