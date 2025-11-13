#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (site/EV caps, slot length, scoring weights) + RULES_N3 (math:* only)
# are partially evaluated into a compact Driver. At run time, the Driver ingests
# dynamic RDF (arrival/depart window, per-slot price & greenness, curfew/availability),
# normalizes features, mirrors the N3 scoring (price vs CO2), and greedily schedules
# charging within the window subject to kW caps. It then prints:
#   1) Answer — schedule lines per hour (chronological), totals,
#   2) Reason why — traces that mirror math:* steps/comparisons,
#   3) Check (harness) — revalidates arithmetic, caps, scoring, order, and a
#      monotonicity probe (raising price weight should not raise avg €/kWh).
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
@prefix ex:  <http://example.org/ev#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Site/EV capabilities
ex:site   ex:gridCap_kW    7.2 .   # site export/connection cap
ex:ev     ex:maxCharge_kW  7.2 .   # EV max charger draw
ex:policy ex:slotHours     1.0 .   # slot length (h)

# Target energy (kWh) to deliver within window
ex:policy ex:targetEnergy_kWh 10.0 .

# Scoring weights (sum ~ 1.0): score = wPrice*priceN + wCO2*(1 - greenN)
ex:policy ex:wPrice 0.70 .
ex:policy ex:wCO2   0.30 .
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/ev#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Window
ex:pt ex:arrival 19 .   # 19:00
ex:pt ex:depart  23 .   # exclusive

# Membership
ex:window ex:hasSlot ex:h18 .
ex:window ex:hasSlot ex:h19 .
ex:window ex:hasSlot ex:h20 .
ex:window ex:hasSlot ex:h21 .
ex:window ex:hasSlot ex:h22 .
ex:window ex:hasSlot ex:h23 .

# Per-slot signals
# ex:hour, ex:price €/kWh, ex:greenN (0..1, higher = greener), flags
ex:h18 ex:hour 18 . ex:h18 ex:price 0.26 . ex:h18 ex:greenN 0.4 . ex:h18 ex:isCurfew false . ex:h18 ex:available true .
ex:h19 ex:hour 19 . ex:h19 ex:price 0.22 . ex:h19 ex:greenN 0.6 . ex:h19 ex:isCurfew false . ex:h19 ex:available true .
ex:h20 ex:hour 20 . ex:h20 ex:price 0.24 . ex:h20 ex:greenN 0.8 . ex:h20 ex:isCurfew false . ex:h20 ex:available true .
ex:h21 ex:hour 21 . ex:h21 ex:price 0.27 . ex:h21 ex:greenN 0.7 . ex:h21 ex:isCurfew false . ex:h21 ex:available true .
ex:h22 ex:hour 22 . ex:h22 ex:price 0.30 . ex:h22 ex:greenN 0.5 . ex:h22 ex:isCurfew true  . ex:h22 ex:available true .
ex:h23 ex:hour 23 . ex:h23 ex:price 0.28 . ex:h23 ex:greenN 0.3 . ex:h23 ex:isCurfew false . ex:h23 ex:available false .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — math:* only (correct arities)
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/ev#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# R1: Global kW cap = min(gridCap_kW, maxCharge_kW)  (we expose both; min is applied in driver)
{ ex:site ex:gridCap_kW ?gc . } => { ex:policy ex:gridCap_kW ?gc } .
{ ex:ev   ex:maxCharge_kW ?mc . } => { ex:policy ex:maxCharge_kW ?mc } .

# R2: Normalize price to [0,1] across window (min-max) — done numerically in driver.
# R3: invG = (1 - greenN)
{ ?s ex:greenN ?g . ( 1 ?g ) math:difference ?invG . } => { ?s ex:invG ?invG } .

# R4: Slot score = wPrice*priceN + wCO2*invG
{
  ?s ex:priceN ?pN .
  ?s ex:invG ?invG .
  ex:policy ex:wPrice ?wP .
  ex:policy ex:wCO2 ?wC .
  ( ?wP ?pN )  math:product ?pTerm .
  ( ?wC ?invG ) math:product ?gTerm .
  ( ?pTerm ?gTerm ) math:sum ?score .
}
=> { ?s ex:score ?score } .

# R5: Operability (curfew or unavailable ⇒ not operable)
{ ?s ex:isCurfew true . }   => { ?s ex:operable false } .
{ ?s ex:available false . } => { ?s ex:operable false } .
"""

# ──────────────────────────────────────────────────────────────────────────────
# Tiny Turtle reader
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
            if not stmt: continue
            parts = stmt.split(None, 2)
            if len(parts) < 3: continue
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
# Domain + driver
# ──────────────────────────────────────────────────────────────────────────────
@dataclass
class Weights:
    wPrice: float
    wCO2: float

@dataclass
class Slot:
    iri: str
    hour: int
    price: float
    greenN: float
    priceN: float
    invG: float
    score: float
    isCurfew: bool
    available: bool
    operable: bool
    trace: List[str]

@dataclass
class Allocation:
    slot: Slot
    kw: float
    grid_kwh: float
    cost: float

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    # policy/site caps
    gridCap = float(S.get("ex:site", {}).get("ex:gridCap_kW", float("inf")))
    maxCharge = float(S.get("ex:ev", {}).get("ex:maxCharge_kW", float("inf")))
    kw_cap = min(gridCap, maxCharge)
    slotHours = float(S.get("ex:policy", {}).get("ex:slotHours", 1.0))
    target_kWh = float(S.get("ex:policy", {}).get("ex:targetEnergy_kWh", 0.0))
    W = Weights(
        wPrice=float(S.get("ex:policy", {}).get("ex:wPrice", 0.5)),
        wCO2=float(S.get("ex:policy", {}).get("ex:wCO2", 0.5)),
    )

    def driver(D: Dict[str, Dict[str, Any]]):
        arrival = int(D.get("ex:pt", {}).get("ex:arrival", 0))
        depart  = int(D.get("ex:pt", {}).get("ex:depart", 24))
        slot_iris = [str(x) for x in listify(D["ex:window"]["ex:hasSlot"])]

        # Gather raw
        rows = []
        for iri in slot_iris:
            P = D[iri]
            rows.append({
                "iri": iri,
                "hour": int(P.get("ex:hour", -1)),
                "price": float(P.get("ex:price", 0.0)),
                "green": float(P.get("ex:greenN", 0.0)),
                "isCurfew": bool(P.get("ex:isCurfew", False)),
                "available": bool(P.get("ex:available", True)),
            })

        # Normalize price (min-max)
        prices = [r["price"] for r in rows]
        p_lo, p_hi = min(prices), max(prices)
        if p_hi - p_lo < 1e-12:
            priceN = [0.0]*len(rows)
        else:
            priceN = [(p - p_lo)/(p_hi - p_lo) for p in prices]

        # Build slots with score and operability
        slots: List[Slot] = []
        for i, r in enumerate(rows):
            invG = 1.0 - r["green"]
            pTerm = W.wPrice * priceN[i]
            gTerm = W.wCO2   * invG
            score = pTerm + gTerm
            oper = (not r["isCurfew"]) and r["available"]
            trace = [
                f"R3: (1 {r['green']:.3f}) math:difference invG={invG:.3f}",
                f"R4: ({W.wPrice:.2f} {priceN[i]:.3f}) math:product {pTerm:.3f}; "
                f"({W.wCO2:.2f} {invG:.3f}) math:product {gTerm:.3f}; "
                f"math:sum→ score={score:.3f}",
                f"R5: curfew={str(r['isCurfew']).lower()}, available={str(r['available']).lower()} ⇒ operable={str(oper).lower()}",
            ]
            slots.append(Slot(
                iri=r["iri"], hour=r["hour"], price=r["price"], greenN=r["green"],
                priceN=priceN[i], invG=invG, score=score,
                isCurfew=r["isCurfew"], available=r["available"], operable=oper, trace=trace
            ))

        # Sort slots by score (better first) for selection…
        slots_by_score = sorted(slots, key=lambda s: s.score)

        # Greedy schedule within [arrival, depart), only operable, capped at kw_cap
        need = target_kWh
        allocs: List[Allocation] = []
        for s in slots_by_score:
            if not (arrival <= s.hour < depart):    # outside window
                continue
            if not s.operable:
                continue
            if need <= 1e-9:
                break
            # kWh we still need → kW this slot (respecting cap)
            max_kwh = kw_cap * slotHours
            take_kwh = min(need, max_kwh)
            kw = take_kwh / slotHours
            allocs.append(Allocation(slot=s, kw=kw, grid_kwh=take_kwh, cost=take_kwh * s.price))
            need -= take_kwh

        # IMPORTANT: Present allocations **chronologically** (by hour) for tidy output/checks
        allocs.sort(key=lambda a: a.slot.hour)

        # Totals
        sum_grid = sum(a.grid_kwh for a in allocs)
        sum_cost = sum(a.cost for a in allocs)

        result = {
            "weights": W,
            "kw_cap": kw_cap,
            "slot_hours": slotHours,
            "slots": slots_by_score,  # keep score order for diagnostics
            "allocs": allocs,         # chronological for output/harness
            "need_batt": target_kWh,
            "sum_grid": sum_grid,
            "sum_batt": sum_grid,     # alias
            "sum_cost": sum_cost,
            "arrival": arrival,
            "depart": depart,
        }
        return result

    return driver

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────
def main():
    S = parse_turtle_simple(STATIC_TTL)
    D = parse_turtle_simple(DYNAMIC_TTL)

    driver = specialize_driver(S)
    result = driver(D)

    W: Weights = result["weights"]
    kw_cap      = float(result["kw_cap"])
    slot_hours  = float(result["slot_hours"])
    allocs: List[Allocation] = result["allocs"]
    slots:  List[Slot]        = result["slots"]
    need_batt   = float(result["need_batt"])
    sum_grid    = float(result["sum_grid"])
    sum_cost    = float(result["sum_cost"])
    arrival     = int(result["arrival"])
    depart      = int(result["depart"])

    # ── ANSWER ──
    print("Answer:")
    if not allocs:
        print("- No feasible charging within the window.")
    else:
        for a in allocs:  # chronological
            print(f"- h{a.slot.hour:02d} • set {a.kw:.2f} kW • {a.grid_kwh:.2f} kWh • €{a.cost:.2f} "
                  f"(price {a.slot.price:.3f} €/kWh, score {a.slot.score:.3f})")
        print(f"  • Totals: {sum_grid:.2f} kWh, €{sum_cost:.2f} (target {need_batt:.2f} kWh)")

    # ── REASON WHY ──
    print("\nReason why:")
    print(f"- Caps: site={S['ex:site']['ex:gridCap_kW']:.1f} kW, ev={S['ex:ev']['ex:maxCharge_kW']:.1f} kW ⇒ kw_cap={kw_cap:.1f} kW; slotHours={slot_hours:.1f}")
    print(f"- Weights: wPrice={W.wPrice:.2f}, wCO2={W.wCO2:.2f}; window [{arrival},{depart})")
    for s in slots:  # score order (diagnostics)
        print(f"- slot h{s.hour}:")
        for ln in s.trace:
            print(f"  • {ln}")

    # ── CHECK (harness) ─
    print("\nCheck (harness):")
    errors: List[str] = []

    # (C1) Arithmetic checks per allocation
    c1_max_dE = 0.0
    c1_max_dC = 0.0
    for a in allocs:
        e_re = a.kw * slot_hours
        de = abs(e_re - a.grid_kwh)
        c1_max_dE = max(c1_max_dE, de)
        if de > 1e-6:
            errors.append(f"(C1) Energy mismatch at h{a.slot.hour}: {a.grid_kwh:.6f} vs {e_re:.6f}")
        c_re = a.grid_kwh * a.slot.price
        dc = abs(c_re - a.cost)
        c1_max_dC = max(c1_max_dC, dc)
        if dc > 1e-6:
            errors.append(f"(C1) Cost mismatch at h{a.slot.hour}: €{a.cost:.6f} vs €{c_re:.6f}")

    # (C2) Requirement and caps/window
    if sum_grid + 1e-6 < need_batt:
        errors.append(f"(C2) Battery energy shortfall: {sum_grid:.3f} < {need_batt:.3f} kWh")

    cap_hits = 0
    for a in allocs:
        if a.kw < -1e-9:
            errors.append(f"(C2) Negative power at h{a.slot.hour}: {a.kw:.3f} kW")
        if a.kw - kw_cap > 1e-9:
            errors.append(f"(C2) Cap exceeded at h{a.slot.hour}: {a.kw:.3f} kW > {kw_cap:.3f} kW")
        if abs(a.kw - kw_cap) < 1e-9:
            cap_hits += 1
        if not (arrival <= a.slot.hour < depart):
            errors.append(f"(C2) Allocation outside window at h{a.slot.hour} ∉ [{arrival},{depart})")

    # (C3) Score recomputation per slot and score-order preservation
    c3_max_ds = 0.0
    for s in slots:
        invG = (1 - s.greenN)
        pTerm = W.wPrice * s.priceN
        gTerm = W.wCO2   * invG
        s_re = pTerm + gTerm
        ds = abs(s_re - s.score)
        c3_max_ds = max(c3_max_ds, ds)
        if ds > 1e-9:
            errors.append(f"(C3) Score mismatch h{s.hour}: {s.score:.6f} vs {s_re:.6f}")

    if [s.iri for s in sorted(slots, key=lambda z: z.score)] != [s.iri for s in slots]:
        errors.append("(C3) Slot score order mismatch")

    # (C4) Allocations must be chronological
    alloc_hours = [a.slot.hour for a in allocs]
    if alloc_hours != sorted(alloc_hours):
        errors.append(f"(C4) Allocations not in chronological order: {alloc_hours}")

    # (C5) Monotonicity wrt price weight: raising wPrice should not increase avg €/kWh
    def avg_price_with_delta(delta=0.20) -> float:
        S2 = {k: (v.copy() if isinstance(v, dict) else v) for k, v in S.items()}
        pol = S2.setdefault("ex:policy", {})
        pol["ex:wPrice"] = float(pol.get("ex:wPrice", W.wPrice)) + delta
        pol["ex:wCO2"]   = max(0.0, 1.0 - float(pol["ex:wPrice"]))
        res2 = specialize_driver(S2)(D)
        g2 = float(res2["sum_grid"])
        return (float(res2["sum_cost"]) / g2) if g2 > 1e-12 else float("inf")

    base_avg = (sum_cost / sum_grid) if sum_grid > 1e-12 else float("inf")
    new_avg  = avg_price_with_delta(0.20)
    if new_avg > base_avg + 1e-9:
        errors.append(f"(C5) Raising wPrice increased average €/kWh ({base_avg:.4f} → {new_avg:.4f})")

    # Outcome
    if errors:
        print("❌ FAIL")
        for er in errors:
            print(" -", er)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Arithmetic re-check: {len(allocs)} allocations "
              f"(max Δenergy={c1_max_dE:.3e} kWh, max Δcost={c1_max_dC:.3e} €)")
        print(f"  • [C2] Requirement/caps OK: delivered {sum_grid:.2f} / target {need_batt:.2f} kWh; "
              f"cap={kw_cap:.2f} kW; cap-hit slots={cap_hits}; window [{arrival},{depart})")
        print(f"  • [C3] Slot scoring OK: max Δscore={c3_max_ds:.3e}; order by score preserved")
        print(f"  • [C4] Order OK: allocation hours {alloc_hours if allocs else '—'}")
        if base_avg < float('inf'):
            print(f"  • [C5] Price-weight monotonicity OK: avg €/kWh {base_avg:.4f} → {new_avg:.4f} (expected Δ≤0)")
        else:
            print("  • [C5] Price-weight monotonicity OK: not applicable (no energy scheduled)")

if __name__ == "__main__":
    main()

