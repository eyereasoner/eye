from __future__ import annotations
"""
Carbon- & Price-Aware Home Energy / EV-Charging Scheduler — final
=================================================================
EYE-style pipeline: RDF Data + N3 Behaviors → Agent (partial eval, Ershov) → Driver → schedule.

One-liner: a compact EYE-style scheduler that partially evaluates static device/policy RDF plus
N3 rule intent into a specialized driver which consumes dynamic price/carbon/base-load signals
and outputs a peak-safe, deadline-feasible EV/DW plan with explanations.

ASCII schematic
---------------
  ME (homeowner) -----------------+
                                  |         +-------------------+
                                  |         |   Behaviors       |  (rule docs)
                                  |         |   (N3 templates)  |
                                  |         +-------------------+
                                  |                  ^    ^
                                  |                  |    |
                                  v                  |    | context (dynamic)
+-------------------+   AC    +----------+           |    |
| data (RDF, static)| <-----> | context  |-----------+    |
| facts, defaults   | (access)+----------+                |
+-------------------+                                      v
                         Agent (partial evaluator / specialization)
                                     |
                                     v
                           Driver (specialized scheduler)
                                     |
                                     v
             +-------------------------------------------------------------+
             | Targets / Capabilities / Context (devices & time slots)     | --> schedule
             +-------------------------------------------------------------+
                                     |
                                     v
                          actionable insight + feedback (trace)

Legend (aligned with EYE-learning.md + Ershov mixed computation)
----------------------------------------------------------------
- Data (RDF, static): device specs (EV, charger, dishwasher), policy weights, peak limit.
- Context (RDF, dynamic): price & carbon forecasts, base load, initial SoC, deadlines.
- Behaviors (N3): declarative intent (prefer cheap/green slots, respect peak/deadlines).
- Agent: partially evaluates static facts to produce a specialized Driver (by closure).
- Driver: consumes only dynamic arrays and emits a feasible plan + explanation trace.
- Answer • Reason-why • Check: schedule CSV, per-slot notes, and assertions/guardrails.
"""

import os, csv, math, json
from typing import Any, Dict, List

# -----------------------------------------------------------------------------
# Artifact directory (simple knowledge store)
# -----------------------------------------------------------------------------
BASE = os.path.join(os.path.dirname(__file__), "output/evscheduler_artifacts")
os.makedirs(BASE, exist_ok=True)

EX = "http://example.org/ev#"

# -----------------------------------------------------------------------------
# STATIC (compile-time) RDF (keep triples clean; comments allowed on separate lines)
# -----------------------------------------------------------------------------
STATIC_TTL = """@prefix ex: <http://example.org/ev#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (combine price + carbon; add peak proximity penalty term)
ex:policy ex:wPrice 0.7 .
ex:policy ex:wCarbon 0.3 .
ex:policy ex:wPeakPenalty 0.2 .

# Household constraints
ex:grid ex:peakLimitKw 8.0 .

# EV device specs
ex:ev ex:capacityKwh 60.0 .
ex:ev ex:chargerMaxKw 7.4 .
ex:ev ex:minSoc 0.20 .
ex:ev ex:targetSoc 0.80 .

# Dishwasher specs (as a representative flexible load)
ex:dishwasher ex:powerKw 1.2 .
ex:dishwasher ex:cycleSlots 4 .
"""

# -----------------------------------------------------------------------------
# DYNAMIC (run-time) RDF — synthetic 24h forecast with 15-min slots (96 slots)
# -----------------------------------------------------------------------------
def make_dynamic_ttl() -> str:
    lines = [
        "@prefix ex: <http://example.org/ev#> .",
        "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
        "",
        "# EV state & deadlines",
        "ex:session ex:initialSoc 0.35 .",
        "ex:session ex:deadlineSlot 28 .",
        "",
        "# Dishwasher latest finish (same as EV deadline for simplicity)",
        "ex:dishwasher ex:latestFinishSlot 28 .",
        "",
    ]
    slots = 96
    price, carbon, baseload = [], [], []
    for s in range(slots):
        hour = s/4.0
        # price: base 0.20 €/kWh + day hump + evening peak; cheaper after midnight
        p = 0.20 + 0.05*max(0, math.sin((hour-12)/12*math.pi)) + 0.12*max(0, math.sin((hour-18)/6*math.pi))
        if 0 <= hour < 6: p -= 0.05
        price.append(round(p, 3))
        # carbon intensity: base 300 g/kWh + correlation with price peaks; lower at night
        c = 300 + 80*max(0, math.sin((hour-12)/12*math.pi)) + 120*max(0, math.sin((hour-18)/6*math.pi))
        if 0 <= hour < 6: c -= 60
        carbon.append(int(round(c)))
        # base load: 0.6 kW night, breakfast 1.2 kW, evening 1.8 kW peak
        b = 0.6
        if 6 <= hour < 9: b = 1.2
        if 18 <= hour < 22: b = 1.8
        baseload.append(round(b, 2))
    for s in range(slots):
        lines.append(f"ex:slot_{s:02d} ex:price {price[s]} .")
        lines.append(f"ex:slot_{s:02d} ex:carbon {carbon[s]} .")
        lines.append(f"ex:slot_{s:02d} ex:baseKw {baseload[s]} .")
    return "\n".join(lines)

DYNAMIC_TTL = make_dynamic_ttl()

# -----------------------------------------------------------------------------
# Behaviors: Rule documentation in N3 (valid triple-only math built-ins)
# -----------------------------------------------------------------------------
RULES_N3 = """@prefix ex:   <http://example.org/ev#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# -----------------------------------------------------------------------------
# Scoring from normalized features (requires ex:pNorm, ex:cNorm per slot)
# score = wP * pNorm + wC * cNorm + wPeak * peakFrac
# -----------------------------------------------------------------------------
{ ?s ex:pNorm ?pN .
  ?s ex:cNorm ?cN .
  ?s ex:peakFrac ?pF .
  ex:policy ex:wPrice ?wP .
  ex:policy ex:wCarbon ?wC .
  ex:policy ex:wPeakPenalty ?wPk .
  ( ?wP ?pN ) math:product ?pTerm .
  ( ?wC ?cN ) math:product ?cTerm .
  ( ?wPk ?pF ) math:product ?pkTerm .
  ( ?pTerm ?cTerm ) math:sum ?pcSum .
  ( ?pcSum ?pkTerm ) math:sum ?score .
} => { ?s ex:score ?score } .

# -----------------------------------------------------------------------------
# Derive peak fraction: peakFrac = baseKw / peakLimitKw
# -----------------------------------------------------------------------------
{ ?s ex:baseKw ?b .
  ex:grid ex:peakLimitKw ?L .
  ( ?b ?L ) math:quotient ?f .
} => { ?s ex:peakFrac ?f } .

# -----------------------------------------------------------------------------
# Peak feasibility: baseKw + evKw + dwKw <= peakLimitKw
# -----------------------------------------------------------------------------
{ ?s ex:baseKw ?b .
  ?s ex:evKw   ?e .
  ?s ex:dwKw   ?d .
  ex:grid ex:peakLimitKw ?L .
  ( ?b ?e ) math:sum ?be .
  ( ?be ?d ) math:sum ?tot .
  ?tot math:notGreaterThan ?L .
} => { ?s ex:feasible true } .

# -----------------------------------------------------------------------------
# EV goal: reach target SoC by the deadline (documents the obligation)
# -----------------------------------------------------------------------------
{ ex:session ex:deadlineSlot ?T .
  ex:ev      ex:targetSoc   ?Z .
} => { ex:ev ex:mustReach [ ex:targetSoc ?Z ; ex:bySlot ?T ] } .

# -----------------------------------------------------------------------------
# Dishwasher goal: finish before latestFinishSlot
# -----------------------------------------------------------------------------
{ ex:dishwasher ex:latestFinishSlot ?D . }
  => { ex:dishwasher ex:mustFinishBefore ?D } .
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
# Tiny Turtle Reader (robust to inline '#' and to '#' inside IRIs <...#...>)
# -----------------------------------------------------------------------------
def parse_ttl(ttl_text: str):
    prefixes, triples = {}, []
    for raw in ttl_text.splitlines():
        # Strip inline comments only when outside quotes AND outside <...> IRIs
        in_quotes = False
        in_angle = False
        cleaned_chars = []
        for ch in raw.rstrip():
            if ch == '"' and not in_angle:
                in_quotes = not in_quotes
            elif ch == '<' and not in_quotes:
                in_angle = True
            elif ch == '>' and not in_quotes:
                in_angle = False
            if ch == '#' and (not in_quotes) and (not in_angle):
                break  # comment starts here
            cleaned_chars.append(ch)
        line = ''.join(cleaned_chars).strip()
        if not line:
            continue

        if line.startswith("@prefix"):
            parts = line.split()
            # e.g., @prefix ex: <http://example.org/ev#> .
            if len(parts) >= 3:
                prefixes[parts[1].rstrip(":")] = parts[2].strip("<>")
            continue

        if line.endswith("."):
            line = line[:-1].strip()

        # Tokenize (space-splitting outside quotes)
        toks, buf = [], ""
        in_quotes = False
        for ch in line:
            if ch == '"':
                in_quotes = not in_quotes
                buf += ch
                continue
            if ch == ' ' and not in_quotes:
                if buf:
                    toks.append(buf); buf = ''
            else:
                buf += ch
        if buf:
            toks.append(buf)
        if len(toks) < 3:
            continue

        s, p, o = toks[0], toks[1], ' '.join(toks[2:])

        def expand(t: str) -> str:
            if t.startswith('<') and t.endswith('>'):
                return t[1:-1]
            if ':' in t and not t.startswith('"'):
                pref, local = t.split(':', 1)
                return prefixes.get(pref, pref + ':') + local
            return t

        s_e, p_e = expand(s), expand(p)

        # Parse object
        if o.startswith('"') and o.endswith('"'):
            obj = o.strip('"')
        elif o in ('true', 'false'):
            obj = (o == 'true')
        else:
            try:
                obj = float(o) if '.' in o else int(o)
            except Exception:
                obj = expand(o)

        triples.append((s_e, p_e, obj))
    return prefixes, triples

def index_triples(triples):
    idx: Dict[str, Dict[str, List[Any]]] = {}
    for s, p, o in triples:
        idx.setdefault(s, {}).setdefault(p, []).append(o)
    return idx

def get1(idx, s, p, default=None):
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
# Agent: capture static constants, specialize a Driver via closures (Ershov)
# -----------------------------------------------------------------------------
def norm_list(xs: List[float]) -> List[float]:
    lo, hi = min(xs), max(xs)
    if hi - lo < 1e-9:
        return [0.0]*len(xs)
    return [(x-lo)/(hi-lo) for x in xs]

def make_driver(S, policy_key=EX+"policy"):
    wP = float(get1(S, policy_key, EX+"wPrice", 0.7))
    wC = float(get1(S, policy_key, EX+"wCarbon", 0.3))
    wPeak = float(get1(S, policy_key, EX+"wPeakPenalty", 0.2))
    peak = float(get1(S, EX+"grid", EX+"peakLimitKw", 8.0))
    cap = float(get1(S, EX+"ev", EX+"capacityKwh", 60.0))
    charger = float(get1(S, EX+"ev", EX+"chargerMaxKw", 7.4))
    min_soc = float(get1(S, EX+"ev", EX+"minSoc", 0.2))
    target_soc = float(get1(S, EX+"ev", EX+"targetSoc", 0.8))
    dw_power = float(get1(S, EX+"dishwasher", EX+"powerKw", 1.2))
    dw_slots = int(get1(S, EX+"dishwasher", EX+"cycleSlots", 4))

    def plan(D):
        initial_soc = float(get1(D, EX+"session", EX+"initialSoc", 0.4))
        deadline = int(get1(D, EX+"session", EX+"deadlineSlot", 28))
        latest_dw_finish = int(get1(D, EX+"dishwasher", EX+"latestFinishSlot", deadline))

        # Collect arrays
        SLOTS = sorted([s for s in D.keys() if s.startswith(EX+"slot_")])
        n = len(SLOTS)
        assert n == 96, f"Expected 96 slots (15-min over 24h), parsed {n}. Check dynamic.ttl newlines and inline comments."

        price = [float(get1(D, s, EX+"price", 0.3)) for s in SLOTS]
        carbon = [float(get1(D, s, EX+"carbon", 300.0)) for s in SLOTS]
        base = [float(get1(D, s, EX+"baseKw", 0.6)) for s in SLOTS]

        # Normalize for scoring
        pN, cN = norm_list(price), norm_list(carbon)
        peak_fraction = [b/peak for b in base]

        # EV energy requirement
        def soc_to_kwh(s): return s*cap
        kwh_needed = max(0.0, soc_to_kwh(target_soc) - soc_to_kwh(initial_soc))

        # Baseline: greedy soonest schedule at max power, then earliest dishwasher
        def baseline():
            ev_kw = [0.0]*n
            remaining = kwh_needed
            for i in range(deadline+1):
                if remaining <= 1e-9: break
                headroom = max(0.0, peak - base[i])
                ev_here = min(charger, headroom)
                ev_kwh = ev_here * 0.25
                if ev_kwh > remaining:
                    ev_here = remaining / 0.25
                    ev_kwh = remaining
                ev_kw[i] = ev_here
                remaining -= ev_kwh
            # Dishwasher earliest feasible
            dw = [0]*n
            best_start = None
            for s0 in range(0, latest_dw_finish - dw_slots + 1):
                ok = True
                for k in range(dw_slots):
                    i = s0+k
                    if base[i] + ev_kw[i] + dw_power > peak + 1e-9:
                        ok = False; break
                if ok:
                    best_start = s0; break
            if best_start is None:
                best_start = max(0, latest_dw_finish - dw_slots)
            for k in range(dw_slots): dw[best_start+k] = 1
            return ev_kw, dw

        # Optimized EV: pick cheapest/greenest slots before deadline
        # Score per slot: wP*pN + wC*cN + wPeak*peak_fraction
        score = [wP*pN[i] + wC*cN[i] + wPeak*peak_fraction[i] for i in range(n)]
        order = sorted(range(0, deadline+1), key=lambda i: (score[i], pN[i], cN[i]))
        ev_kw = [0.0]*n
        remaining = kwh_needed
        for i in order:
            if remaining <= 1e-9: break
            headroom = max(0.0, peak - base[i])
            ev_here = min(charger, headroom)  # obey peak & charger
            ev_kwh = ev_here * 0.25
            if ev_kwh > remaining:
                ev_here = remaining / 0.25
                ev_kwh = remaining
            ev_kw[i] = ev_here
            remaining -= ev_kwh

        # Dishwasher: choose cheapest feasible start (respect peak and latest finish)
        dw = [0]*n
        best = (1e9, None)  # (accumulated score, start slot)
        for s0 in range(0, latest_dw_finish - dw_slots + 1):
            ok = True; accum = 0.0
            for k in range(dw_slots):
                i = s0+k
                if base[i] + ev_kw[i] + dw_power > peak + 1e-9:
                    ok = False; break
                accum += score[i]
            if ok and accum < best[0]:
                best = (accum, s0)
        if best[1] is None:
            best = (1e9, max(0, latest_dw_finish - dw_slots))
        for k in range(dw_slots): dw[best[1]+k] = 1

        # Derived metrics
        total_kw = [base[i] + ev_kw[i] + (dw[i]*dw_power) for i in range(n)]
        total_cost = sum((ev_kw[i] + dw[i]*dw_power) * 0.25 * price[i] for i in range(n))
        total_carbon = sum((ev_kw[i] + dw[i]*dw_power) * 0.25 * carbon[i] for i in range(n))
        soc_end = initial_soc + sum(ev_kw)*0.25 / cap

        # Baseline for comparison
        base_ev_kw, base_dw = baseline()
        base_cost = sum((base_ev_kw[i] + base_dw[i]*dw_power) * 0.25 * price[i] for i in range(n))
        base_carbon = sum((base_ev_kw[i] + base_dw[i]*dw_power) * 0.25 * carbon[i] for i in range(n))

        # Per-slot notes
        notes = []
        for i in range(n):
            why = []
            if ev_kw[i] > 0:
                why.append(f"EV@{ev_kw[i]:.1f}kW (score={score[i]:.2f})")
            if dw[i] == 1:
                why.append("DW on")
            if not why and i <= deadline and score[i] == min(score[:deadline+1]):
                why.append("low score slot")
            notes.append("; ".join(why))

        return {
            "SLOTS": SLOTS,
            "price": price, "carbon": carbon, "base": base,
            "ev_kw": ev_kw, "dw": dw, "total_kw": total_kw,
            "total_cost": total_cost, "total_carbon": total_carbon,
            "baseline_cost": base_cost, "baseline_carbon": base_carbon,
            "soc_end": soc_end, "deadline": deadline,
            "dw_power": dw_power, "peak": peak,
            "initial_soc": initial_soc, "target_soc": target_soc,
            "notes": notes,
            # Expose internals for richer trace/CSV
            "score": score, "pNorm": pN, "cNorm": cN, "peak_frac": peak_fraction,
            "weights": {"wPrice": wP, "wCarbon": wC, "wPeakPenalty": wPeak},
        }

    return plan

# Agent step: specialize a scheduler Driver by closing over static facts
driver = make_driver(S)

# Driver execution: consume dynamic arrays & deadlines and produce a plan
res = driver(D)

# -----------------------------------------------------------------------------
# Self-checks (Answer • Reason-why • Check triad)
# -----------------------------------------------------------------------------
# 1) Peak never exceeded (safety constraint)
assert all(k <= res["peak"] + 1e-6 for k in res["total_kw"]), "Peak limit violated"

# 2) EV SoC meets target by deadline
assert res["soc_end"] + 1e-9 >= res["target_soc"], "EV target SoC not reached"

# 3) Dishwasher finishes by latestFinishSlot (same as deadline here)
dw_on_slots = [i for i,v in enumerate(res["dw"]) if v==1]
assert dw_on_slots, "Dishwasher never scheduled"
assert max(dw_on_slots) <= res["deadline"], "Dishwasher finishes after deadline"

# 4) Optimized vs baseline: cost and carbon should not be worse
assert res["total_cost"] <= res["baseline_cost"] + 1e-9, "Optimized cost > baseline"
assert res["total_carbon"] <= res["baseline_carbon"] + 1e-9, "Optimized carbon > baseline"

# 5) Monotonicity spot-check: if we raise peak limit, optimized cost shouldn't increase
def rerun_with_peak(delta):
    S_mod = {k: {kk:list(vv) for kk,vv in props.items()} for k,props in S.items()}
    S_mod[EX+"grid"][EX+"peakLimitKw"] = [res["peak"] + delta]
    return make_driver(S_mod)(D)

res_looser = rerun_with_peak(2.0)
assert res_looser["total_cost"] <= res["total_cost"] + 1e-9, "Looser peak unexpectedly increased cost"

# -----------------------------------------------------------------------------
# Emit per-slot CSV (extended) + machine- and human-readable traces
# -----------------------------------------------------------------------------
csv_path = os.path.join(BASE, "schedule.csv")
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow([
        "slot","hour",
        "price_eur_per_kwh","carbon_g_per_kwh","base_kw",
        "ev_kw","dw_kw","total_kw",
        "pNorm","cNorm","peakFrac","score",
        "note"
    ])
    for i,s in enumerate(res["SLOTS"]):
        hour = i/4.0
        w.writerow([
            i, f"{hour:05.2f}",
            f"{res['price'][i]:.3f}", int(res['carbon'][i]), f"{res['base'][i]:.2f}",
            f"{res['ev_kw'][i]:.2f}", f"{res['dw'][i]*res['dw_power']:.2f}", f"{res['total_kw'][i]:.2f}",
            f"{res['pNorm'][i]:.3f}", f"{res['cNorm'][i]:.3f}", f"{res['peak_frac'][i]:.3f}",
            f"{res['score'][i]:.3f}",
            res["notes"][i]
        ])

# Machine-readable trace (JSON)
trace = {
    "weights": res["weights"],
    "constraints": {"peak_limit_kw": res["peak"], "deadline_slot": res["deadline"]},
    "ev": {"initial_soc": res["initial_soc"], "target_soc": res["target_soc"], "soc_end": res["soc_end"]},
    "baseline": {"cost_eur": res["baseline_cost"], "carbon_g": res["baseline_carbon"]},
    "optimized": {"cost_eur": res["total_cost"], "carbon_g": res["total_carbon"]},
    "decisions": [
        {
            "slot": i,
            "hour": i/4.0,
            "price": res["price"][i],
            "carbon": res["carbon"][i],
            "base_kw": res["base"][i],
            "ev_kw": res["ev_kw"][i],
            "dw_kw": res["dw"][i]*res["dw_power"],
            "pNorm": res["pNorm"][i],
            "cNorm": res["cNorm"][i],
            "peakFrac": res["peak_frac"][i],
            "score": res["score"][i],
            "note": res["notes"][i],
        } for i in range(len(res["SLOTS"]))
    ]
}
with open(os.path.join(BASE, "trace.json"), "w", encoding="utf-8") as jf:
    json.dump(trace, jf, indent=2)

# Human-readable “Reason why”
with open(os.path.join(BASE, "reason-why.txt"), "w", encoding="utf-8") as tf:
    tf.write("Reason why / explanation summary\n")
    tf.write("--------------------------------\n")
    tf.write(f"Weights: wPrice={res['weights']['wPrice']}, wCarbon={res['weights']['wCarbon']}, "
             f"wPeakPenalty={res['weights']['wPeakPenalty']}\n")
    tf.write(f"Peak limit: {res['peak']} kW; Deadline slot: {res['deadline']}\n")
    tf.write(f"EV SoC: start {res['initial_soc']:.2f} → end {res['soc_end']:.2f} "
             f"(target {res['target_soc']:.2f})\n")
    tf.write(f"Cost: baseline €{res['baseline_cost']:.2f} → optimized €{res['total_cost']:.2f}\n")
    tf.write(f"CO2:  baseline {int(res['baseline_carbon'])} g → optimized {int(res['total_carbon'])} g\n\n")
    used_slots = [i for i,v in enumerate(res["ev_kw"]) if v > 0]
    top = sorted(used_slots, key=lambda i: res["score"][i])[:5]
    tf.write("Top contributing charging slots (lowest score):\n")
    for i in top:
        tf.write(f"  - slot {i:02d} (h={i/4.0:05.2f}): score={res['score'][i]:.3f}, "
                 f"price={res['price'][i]:.3f}, carbon={int(res['carbon'][i])}, "
                 f"peakFrac={res['peak_frac'][i]:.2f}, ev_kw={res['ev_kw'][i]:.1f}\n")

print("ALL TESTS PASSED")
print("Summary:")
print(" - Baseline cost:  €", round(res["baseline_cost"], 2), "  Optimized: €", round(res["total_cost"], 2))
print(" - Baseline CO2:   ", int(res["baseline_carbon"]), "g   Optimized:", int(res["total_carbon"]), "g")
print(" - Peak limit (kW):", res["peak"])
print(" - EV SoC start→end:", round(res["initial_soc"],2), "→", round(res["soc_end"],2), "(target", res["target_soc"], ")")

