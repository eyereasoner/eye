#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (policy weights, charger/site caps, efficiency) + RULES_N3
# (math:* triple patterns only) are partially evaluated into a tiny "driver".
# At run time, the driver ingests dynamic RDF (EV state, availability window,
# hourly prices + renewable share), computes normalized per-slot scores, and
# chooses a charging schedule that meets the energy requirement while obeying
# power/availability constraints. It prints:
#   1) Answer (per-hour kW, grid kWh, cost — plus totals),
#   2) Reason why (math:* trace lines for slot scores),
#   3) Check (harness re-validating energy, caps, window, and scoring).
#
# Contract with EYE learning:
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
@prefix ex:  <http://example.org/ev#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy weights (sum ~ 1.0): minimize score = wPrice*priceN + wCO2*(1 - greenN)
ex:policy ex:wPrice 0.60 .
ex:policy ex:wCO2   0.40 .

# Site/charger characteristics
ex:site    ex:siteCapKw     11.0 .
ex:charger ex:maxKw          7.0 .
ex:charger ex:efficiency     0.90 .   # battery gain = grid_kWh * efficiency
ex:policy  ex:slotHours      1.0 .    # hour granularity
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/ev#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# EV state + requirement
ex:ev  ex:batteryKWh 60 .
ex:ev  ex:socNow     0.30 .
ex:ev  ex:socTarget  0.80 .

# Availability window (today), and explicit slot membership
ex:window ex:arrivalHour 18 .
ex:window ex:departHour  23 .
ex:window ex:hasSlot ex:slot_h18 .
ex:window ex:hasSlot ex:slot_h19 .
ex:window ex:hasSlot ex:slot_h20 .
ex:window ex:hasSlot ex:slot_h21 .
ex:window ex:hasSlot ex:slot_h22 .

# Hourly tariff + renewable share (0..1)
ex:slot_h18 ex:hour 18 . ex:slot_h18 ex:price 0.25 . ex:slot_h18 ex:green 0.35 .
ex:slot_h19 ex:hour 19 . ex:slot_h19 ex:price 0.20 . ex:slot_h19 ex:green 0.40 .
ex:slot_h20 ex:hour 20 . ex:slot_h20 ex:price 0.18 . ex:slot_h20 ex:green 0.55 .
ex:slot_h21 ex:hour 21 . ex:slot_h21 ex:price 0.22 . ex:slot_h21 ex:green 0.60 .
ex:slot_h22 ex:hour 22 . ex:slot_h22 ex:price 0.26 . ex:slot_h22 ex:green 0.30 .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — only math:* built-ins for arithmetic/relations
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/ev#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# Slot score = wPrice*priceN + wCO2*(1 - greenN)
{
  ?s ex:priceN ?pN .
  ?s ex:greenN ?gN .
  ex:policy ex:wPrice ?wP .
  ex:policy ex:wCO2   ?wC .
  ( 1 ?gN )        math:difference ?invG .
  ( ?wP ?pN )      math:product    ?pTerm .
  ( ?wC ?invG )    math:product    ?gTerm .
  ( ?pTerm ?gTerm ) math:sum       ?score .
}
=>
{ ?s ex:score ?score } .

# Feasibility (pure triple patterns) is handled by the driver:
# - Only slots in ex:window are considered.
# - Per-slot power ≤ min(siteCap, chargerCap).
# - Sum of battery gain ≥ required (within tolerance).
"""

# ──────────────────────────────────────────────────────────────────────────────
# Inline-comment–safe tiny Turtle reader (collects repeated predicates)
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
    wPrice: float
    wCO2: float

@dataclass
class Slot:
    iri: str
    hour: int
    price: float
    green: float
    priceN: float = 0.0
    greenN: float = 0.0
    score: float = 0.0
    trace: List[str] = None

@dataclass
class Allocation:
    slot: Slot
    kw: float
    grid_kwh: float
    batt_kwh: float
    cost: float

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    W = Weights(
        wPrice=float(S["ex:policy"]["ex:wPrice"]),
        wCO2=float(S["ex:policy"]["ex:wCO2"]),
    )
    site_cap = float(S.get("ex:site", {}).get("ex:siteCapKw", 99))
    charger_cap = float(S.get("ex:charger", {}).get("ex:maxKw", 7.0))
    eff = float(S.get("ex:charger", {}).get("ex:efficiency", 0.90))
    slot_hours = float(S.get("ex:policy", {}).get("ex:slotHours", 1.0))

    def normalize(vals: List[float]) -> Tuple[List[float], Dict[str, float]]:
        lo, hi = min(vals), max(vals)
        rng = hi - lo
        if rng < 1e-12:
            return [0.0 for _ in vals], {"min": lo, "max": hi, "rng": rng}
        return [(v - lo) / rng for v in vals], {"min": lo, "max": hi, "rng": rng}

    def driver(D: Dict[str, Dict[str, Any]]):
        # EV requirement
        batt_kwh = float(D["ex:ev"]["ex:batteryKWh"])
        soc_now = float(D["ex:ev"]["ex:socNow"])
        soc_target = float(D["ex:ev"]["ex:socTarget"])
        need_batt = max(0.0, (soc_target - soc_now) * batt_kwh)  # kWh into battery

        # Window + slots
        arrival = int(D["ex:window"]["ex:arrivalHour"])
        depart  = int(D["ex:window"]["ex:departHour"])
        window_slots = [str(s) for s in listify(D["ex:window"]["ex:hasSlot"])]

        # Build slot objects
        slots: List[Slot] = []
        for s in window_slots:
            props = D.get(s, {})
            slots.append(Slot(
                iri=s,
                hour=int(props.get("ex:hour", -1)),
                price=float(props.get("ex:price", 1.0)),
                green=float(props.get("ex:green", 0.0)),
            ))

        # Normalize price & green across available slots
        priceN, _ = normalize([s.price for s in slots])
        greenN, _ = normalize([s.green for s in slots])
        for i, s in enumerate(slots):
            s.priceN = priceN[i]
            s.greenN = greenN[i]
            invG = (1 - s.greenN)             # (1 greenN) math:difference invG
            pTerm = W.wPrice * s.priceN       # (wPrice priceN) math:product
            gTerm = W.wCO2   * invG           # (wCO2 invG)    math:product
            s.score = pTerm + gTerm           # (pTerm gTerm)  math:sum
            s.trace = [(
                f"Score N3: (1 {s.greenN:.3f}) math:difference {invG:.3f} ; "
                f"({W.wPrice:.2f} {s.priceN:.3f}) math:product {pTerm:.3f} ; "
                f"({W.wCO2:.2f} {invG:.3f}) math:product {gTerm:.3f} ; "
                f"sum→ {s.score:.3f}"
            )]

        # Sort slots by ascending score (cheaper & greener first)
        slots.sort(key=lambda s: s.score)

        # Greedy allocate until battery need is met; obey per-slot power cap
        kw_cap = min(site_cap, charger_cap)
        allocations: List[Allocation] = []
        remain_batt = need_batt
        total_grid_kwh = 0.0
        total_cost = 0.0
        green_energy = 0.0

        for s in slots:
            if remain_batt <= 1e-9:
                break
            # Max battery gain this slot at cap
            max_batt_gain = kw_cap * slot_hours * eff
            take_batt = min(max_batt_gain, remain_batt)
            grid_kwh = take_batt / eff
            kw = grid_kwh / slot_hours
            cost = grid_kwh * s.price

            allocations.append(Allocation(
                slot=s, kw=kw, grid_kwh=grid_kwh, batt_kwh=take_batt, cost=cost
            ))
            remain_batt -= take_batt
            total_grid_kwh += grid_kwh
            total_cost += cost
            green_energy += grid_kwh * s.green  # green share of grid energy

        avg_green = (green_energy / total_grid_kwh) if total_grid_kwh > 0 else 0.0

        return {
            "weights": W,
            "eff": eff,
            "kw_cap": kw_cap,
            "slot_hours": slot_hours,
            "need_batt": need_batt,
            "arrival": arrival,
            "depart": depart,
            "slots": slots,
            "allocs": allocations,
            "sum_grid": total_grid_kwh,
            "sum_cost": total_cost,
            "sum_batt": need_batt - remain_batt if 'remain_batt' in locals() else 0.0,
            "avg_green": avg_green,
        }

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
    kw_cap = result["kw_cap"]
    slot_hours = result["slot_hours"]

    allocs: List[Allocation] = result["allocs"]

    # ── ANSWER ──
    print("Answer:")
    if not allocs:
        print("- No charging scheduled.")
    else:
        for i, a in enumerate(allocs, start=1):
            print(f"- Slot h{a.slot.hour}: {a.kw:.2f} kW for {slot_hours:.1f} h "
                  f"→ grid {a.grid_kwh:.2f} kWh, cost €{a.cost:.2f} (green {a.slot.green:.0%})")
        print(f"  • Battery gain: {result['sum_batt']:.2f} kWh "
              f"(need {result['need_batt']:.2f} kWh)")
        print(f"  • Grid energy:  {result['sum_grid']:.2f} kWh")
        print(f"  • Total cost:   €{result['sum_cost']:.2f}")
        print(f"  • Avg green share: {result['avg_green']:.0%}")

    # ── REASON WHY ──
    print("\nReason why:")
    print(f"- Weights: wPrice={W.wPrice:.2f}, wCO2={W.wCO2:.2f}")
    print(f"- Caps: min(siteCap, chargerCap) = {kw_cap:.2f} kW ; efficiency={result['eff']:.2f}")
    for s in result["slots"]:
        print(f"- slot h{s.hour}:")
        for ln in s.trace:
            print(f"  • {ln}")

    # Echo rule/data fingerprints for auditability
    print("\nInputs (fingerprints):")
    print(f"- Static RDF bytes: {len(STATIC_TTL.encode())} ; Dynamic RDF bytes: {len(DYNAMIC_TTL.encode())}")
    print(f"- Rules N3 bytes: {len(RULES_N3.encode())} (math:* triples only)")

    # ── CHECK (harness) ──
    print("\nCheck (harness):")
    errors: List[str] = []

    # (C1) Energy requirement met within tolerance
    if result["sum_batt"] + 1e-6 < result["need_batt"]:
        errors.append(f"(C1) Battery energy shortfall: {result['sum_batt']:.3f} < {result['need_batt']:.3f}")

    # (C2) Per-slot power cap respected and non-negative; hours within window membership
    window_hours = {int(D["ex:window"]["ex:arrivalHour"]) + i for i in range(
        int(D["ex:window"]["ex:departHour"]) - int(D["ex:window"]["ex:arrivalHour"]))}
    # We used explicit membership in TTL, but also check numeric hours are in [arrival, depart)
    a_hr = int(D["ex:window"]["ex:arrivalHour"])
    d_hr = int(D["ex:window"]["ex:departHour"])
    for a in allocs:
        if a.kw - kw_cap > 1e-9:
            errors.append(f"(C2) Slot h{a.slot.hour} exceeds cap: {a.kw:.3f} kW > {kw_cap:.3f} kW")
        if a.kw < -1e-9:
            errors.append(f"(C2) Slot h{a.slot.hour} negative power")
        if not (a_hr <= a.slot.hour < d_hr):
            errors.append(f"(C2) Slot h{a.slot.hour} outside window [{a_hr},{d_hr})")

    # (C3) Recompute each slot score from normalized terms & weights
    for s in result["slots"]:
        invG = (1 - s.greenN)
        pTerm = W.wPrice * s.priceN
        gTerm = W.wCO2   * invG
        recomputed = pTerm + gTerm
        if abs(recomputed - s.score) > 1e-9:
            errors.append(f"(C3) Score mismatch for slot h{s.hour}: {s.score:.6f} vs {recomputed:.6f}")

    # (C4) Sorting by ascending score is preserved
    sorted_copy = sorted(result["slots"], key=lambda z: z.score)
    if [s.iri for s in sorted_copy] != [s.iri for s in result["slots"]]:
        errors.append("(C4) Slot sort order mismatch")

    # (C5) Increasing wPrice should not increase average € per kWh in the chosen schedule (heuristic check)
    def rerun_with_price_bonus(delta=0.20):
        S2 = {k: {kk: (vv[:] if isinstance(vv, list) else vv) for kk, vv in v.items()} for k, v in S.items()}
        S2["ex:policy"]["ex:wPrice"] = float(S2["ex:policy"]["ex:wPrice"]) + delta
        # Renormalize wCO2 to keep sum ~ 1.0
        S2["ex:policy"]["ex:wCO2"] = max(0.0, 1.0 - float(S2["ex:policy"]["ex:wPrice"]))
        r2 = specialize_driver(S2)(D)
        if r2["sum_grid"] > 0:
            return r2["sum_cost"] / r2["sum_grid"]
        return float("inf")

    base_avg_price = (result["sum_cost"] / result["sum_grid"]) if result["sum_grid"] > 0 else float("inf")
    new_avg_price = rerun_with_price_bonus(0.20)
    if new_avg_price > base_avg_price + 1e-9:
        errors.append("(C5) Raising wPrice increased average price per kWh")

    if errors:
        print("❌ FAIL")
        for e in errors:
            print(" -", e)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")

if __name__ == "__main__":
    main()

