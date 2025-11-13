#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (turbine limits, site caps, slot size) + RULES_N3 (math:* triple
# patterns only) are partially evaluated into a small Driver. At run time, the
# Driver ingests dynamic RDF (per-slot wind, price, curtailment, curfew flags),
# mirrors the N3 arithmetic (requested fraction, grid-cap fraction, operability
# via cut-in/cut-out), computes a physical capacity from a simple power curve,
# chooses an allowed setpoint fraction (min of constraints), and produces:
#   1) Answer (per-slot setpoint, kW/kWh/revenue, totals),
#   2) Reason why (trace lines that mirror math:* steps/comparisons),
#   3) Check (harness re-validating power curve, caps, curfew, and arithmetic),
#      with a detailed PASS summary.
#
# Contract with P3:
# - All rule arithmetic/relations appear as math:* built-ins only in N3.
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
@prefix ex:  <http://example.org/wind#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Turbine characteristics
ex:turbine ex:ratedPower_kW 3000 .
ex:turbine ex:cutIn_ms       3.5 .
ex:turbine ex:rated_ms      12.0 .
ex:turbine ex:cutOut_ms     25.0 .

# Site & policy
ex:site    ex:gridCap_kW   2500 .   # export cap below rated
ex:policy  ex:slotHours       1.0 .  # hourly slots
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/wind#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Ops flags
ex:ops ex:maintenance false .

# Window membership
ex:window ex:hasSlot ex:slot_h18 .
ex:window ex:hasSlot ex:slot_h19 .
ex:window ex:hasSlot ex:slot_h20 .
ex:window ex:hasSlot ex:slot_h21 .
ex:window ex:hasSlot ex:slot_h22 .
ex:window ex:hasSlot ex:slot_h23 .

# Per-slot signals (wind m/s, price €/kWh, curtailment fraction 0..1, curfew flag)
ex:slot_h18 ex:hour 18 . ex:slot_h18 ex:wind_ms  5.0 . ex:slot_h18 ex:price 0.20 . ex:slot_h18 ex:curtailFrac 0.0 . ex:slot_h18 ex:isCurfew false .
ex:slot_h19 ex:hour 19 . ex:slot_h19 ex:wind_ms  8.0 . ex:slot_h19 ex:price 0.22 . ex:slot_h19 ex:curtailFrac 0.2 . ex:slot_h19 ex:isCurfew false .
ex:slot_h20 ex:hour 20 . ex:slot_h20 ex:wind_ms 11.5 . ex:slot_h20 ex:price 0.25 . ex:slot_h20 ex:curtailFrac 0.0 . ex:slot_h20 ex:isCurfew false .
ex:slot_h21 ex:hour 21 . ex:slot_h21 ex:wind_ms 13.0 . ex:slot_h21 ex:price 0.24 . ex:slot_h21 ex:curtailFrac 0.3 . ex:slot_h21 ex:isCurfew false .
ex:slot_h22 ex:hour 22 . ex:slot_h22 ex:wind_ms  4.0 . ex:slot_h22 ex:price 0.21 . ex:slot_h22 ex:curtailFrac 0.0 . ex:slot_h22 ex:isCurfew true .
ex:slot_h23 ex:hour 23 . ex:slot_h23 ex:wind_ms 26.0 . ex:slot_h23 ex:price 0.23 . ex:slot_h23 ex:curtailFrac 0.0 . ex:slot_h23 ex:isCurfew true .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — only math:* built-ins for arithmetic/relations
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/wind#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# R1: Grid-cap fraction = gridCap_kW / ratedPower_kW (global)
{ ex:site ex:gridCap_kW ?gc . ex:turbine ex:ratedPower_kW ?rp .
  ( ?gc ?rp ) math:quotient ?gfrac . }
=>
{ ex:policy ex:gridCapFrac ?gfrac } .

# R2: Requested fraction per slot = 1 - curtailFrac
{ ?s ex:curtailFrac ?cf . ( 1 ?cf ) math:difference ?rf . } => { ?s ex:requestedFrac ?rf } .

# R3: Operability flag (cut-in ≤ v and v < cut-out)
{ ?s ex:wind_ms ?v . ex:turbine ex:cutIn_ms ?vin . ex:turbine ex:cutOut_ms ?vout .
  ?v math:notLessThan ?vin . ?v math:lessThan ?vout . }
=>
{ ?s ex:operable true } .

# R4: Curfew cap → allowedMaxFrac = 0 if curfew
{ ?s ex:isCurfew true . } => { ?s ex:allowedMaxFrac 0 } .
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
                add(s, p, o[1:o.find('"', 1)])
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
class SlotEval:
    iri: str
    hour: int
    wind: float
    price: float
    curtailFrac: float
    isCurfew: bool
    operable: bool
    gridCapFrac: float
    requestedFrac: float
    allowedMaxFrac: float
    physFrac: float
    chosenFrac: float
    power_kW: float
    energy_kWh: float
    revenue_eur: float
    trace: List[str]

def wind_power_fraction(v: float, vin: float, vr: float, vout: float) -> float:
    """
    Simple normalized power curve:
      0, if v < vin or v >= vout
      ((v^3 - vin^3) / (vr^3 - vin^3)), if vin ≤ v < vr
      1, if vr ≤ v < vout
    """
    if v < vin or v >= vout:
        return 0.0
    if v >= vr:
        return 1.0
    num = v**3 - vin**3
    den = vr**3 - vin**3
    if den <= 0:
        return 0.0
    return max(0.0, min(1.0, num / den))

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    ratedPower = float(S["ex:turbine"]["ex:ratedPower_kW"])
    vin  = float(S["ex:turbine"]["ex:cutIn_ms"])
    vr   = float(S["ex:turbine"]["ex:rated_ms"])
    vout = float(S["ex:turbine"]["ex:cutOut_ms"])
    slotHours = float(S["ex:policy"]["ex:slotHours"])
    gridCap_kW = float(S["ex:site"]["ex:gridCap_kW"])
    gridCapFrac = min(1.0, gridCap_kW / ratedPower)

    def driver(D: Dict[str, Dict[str, Any]]):
        maintenance = bool(D.get("ex:ops", {}).get("ex:maintenance", False))
        slots = [str(s) for s in listify(D["ex:window"]["ex:hasSlot"])]

        evals: List[SlotEval] = []
        for s in slots:
            P = D[s]
            hour = int(P.get("ex:hour", -1))
            wind = float(P.get("ex:wind_ms", 0.0))
            price = float(P.get("ex:price", 0.0))
            curtail = float(P.get("ex:curtailFrac", 0.0))
            isCurfew = bool(P.get("ex:isCurfew", False))

            # RULES_N3 mirror (math:*):
            requestedFrac = max(0.0, min(1.0, 1.0 - curtail))    # R2
            gcf = gridCapFrac                                   # R1
            operable = (wind >= vin and wind < vout and not maintenance)  # R3 (+ maintenance)
            allowedMaxFrac = 0.0 if isCurfew else 1.0           # R4

            # Physical fraction from wind power curve
            physFrac = wind_power_fraction(wind, vin, vr, vout) if operable else 0.0

            # Chosen setpoint fraction = min(physFrac, gridCapFrac, requestedFrac, allowedMaxFrac)
            chosenFrac = min(physFrac, gcf, requestedFrac, allowedMaxFrac)
            power_kW = chosenFrac * ratedPower
            energy_kWh = power_kW * slotHours
            revenue = energy_kWh * price

            trace = [
                f"R2: (1 {curtail:.2f}) math:difference requestedFrac={requestedFrac:.3f}",
                f"R1: ({gridCap_kW:.0f} {ratedPower:.0f}) math:quotient gridCapFrac={gcf:.3f}",
                f"R3: operable if {wind:.1f} math:notLessThan {vin:.1f} and {wind:.1f} math:lessThan {vout:.1f} ⇒ {str(operable).lower()}",
                f"Power curve: physFrac={physFrac:.3f} (vin={vin:.1f}, vr={vr:.1f}, vout={vout:.1f})",
                f"R4: curfew={str(isCurfew).lower()} ⇒ allowedMaxFrac={allowedMaxFrac:.1f}",
                f"Setpoint = min({physFrac:.3f}, {gcf:.3f}, {requestedFrac:.3f}, {allowedMaxFrac:.1f}) = {chosenFrac:.3f}",
            ]

            evals.append(SlotEval(
                iri=s, hour=hour, wind=wind, price=price,
                curtailFrac=curtail, isCurfew=isCurfew, operable=operable,
                gridCapFrac=gcf, requestedFrac=requestedFrac, allowedMaxFrac=allowedMaxFrac,
                physFrac=physFrac, chosenFrac=chosenFrac, power_kW=power_kW,
                energy_kWh=energy_kWh, revenue_eur=revenue, trace=trace
            ))

        # Keep chronological order by hour
        evals.sort(key=lambda e: e.hour)
        totals = {
            "energy_kWh": sum(e.energy_kWh for e in evals),
            "revenue_eur": sum(e.revenue_eur for e in evals),
            "avg_setpoint": (sum(e.chosenFrac for e in evals) / len(evals)) if evals else 0.0
        }
        return evals, totals, {
            "ratedPower": ratedPower, "vin": vin, "vr": vr, "vout": vout,
            "gridCapFrac": gridCapFrac, "slotHours": slotHours, "maintenance": maintenance
        }

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
        status = "operable" if e.operable else "stopped"
        print(f"- h{e.hour:02d} v={e.wind:.1f} m/s ({status}, curfew={str(e.isCurfew).lower()}) "
              f"→ setpoint {e.chosenFrac:.3f} • {e.power_kW:.0f} kW • {e.energy_kWh:.1f} kWh • €{e.revenue_eur:.2f}")
    print(f"  • Total energy: {totals['energy_kWh']:.1f} kWh")
    print(f"  • Total revenue: €{totals['revenue_eur']:.2f}")
    print(f"  • Avg setpoint: {totals['avg_setpoint']:.3f}")

    # ── REASON WHY ──
    print("\nReason why:")
    print(f"- Turbine: rated={ctx['ratedPower']:.0f} kW, vin={ctx['vin']:.1f}, vr={ctx['vr']:.1f}, vout={ctx['vout']:.1f}")
    print(f"- Site: gridCapFrac={ctx['gridCapFrac']:.3f}, slotHours={ctx['slotHours']:.1f}, maintenance={str(ctx['maintenance']).lower()}")
    for e in evals:
        print(f"- slot h{e.hour}:")
        for ln in e.trace:
            print(f"  • {ln}")

    # ── CHECK (harness) ──
    print("\nCheck (harness):")
    errors: List[str] = []

    rated = ctx["ratedPower"]; vin = ctx["vin"]; vr = ctx["vr"]; vout = ctx["vout"]
    gridCapFrac = ctx["gridCapFrac"]; slotHours = ctx["slotHours"]; maintenance = ctx["maintenance"]

    # Stats for richer PASS output
    nslots = len(evals)
    c1_max_dp = 0.0  # max abs delta setpoint
    c1_max_de = 0.0  # max abs delta energy
    c1_max_dr = 0.0  # max abs delta revenue
    curfew_count = sum(1 for e in evals if e.isCurfew)
    nonop_count = sum(1 for e in evals if not e.operable)
    cap_hits = 0

    # (C1) Recompute physical fraction and chosen setpoint; verify power/energy/revenue
    for e in evals:
        phys = 0.0 if maintenance else wind_power_fraction(e.wind, vin, vr, vout)
        oper = (e.wind >= vin and e.wind < vout and not maintenance)
        allowed = 0.0 if e.isCurfew else 1.0
        req = max(0.0, min(1.0, 1.0 - e.curtailFrac))
        chosen = min(phys if oper else 0.0, gridCapFrac, req, allowed)
        dp = abs(chosen - e.chosenFrac)
        power = chosen * rated
        energy = power * slotHours
        revenue = energy * e.price
        de = abs(energy - e.energy_kWh)
        dr = abs(revenue - e.revenue_eur)
        c1_max_dp = max(c1_max_dp, dp)
        c1_max_de = max(c1_max_de, de)
        c1_max_dr = max(c1_max_dr, dr)
        if dp > 1e-9:
            errors.append(f"(C1) Chosen setpoint mismatch at h{e.hour}: {e.chosenFrac:.6f} vs {chosen:.6f}")
        if de > 1e-6:
            errors.append(f"(C1) Energy mismatch at h{e.hour}: {e.energy_kWh:.6f} vs {energy:.6f}")
        if dr > 1e-6:
            errors.append(f"(C1) Revenue mismatch at h{e.hour}: {e.revenue_eur:.6f} vs {revenue:.6f}")

    # (C2) Caps and flags: curfew ⇒ zero; not operable ⇒ zero; grid cap respected
    for e in evals:
        if e.isCurfew and e.chosenFrac > 1e-9:
            errors.append(f"(C2) Curfew non-zero at h{e.hour}")
        if not e.operable and e.chosenFrac > 1e-9:
            errors.append(f"(C2) Non-operable but non-zero at h{e.hour}")
        if e.chosenFrac - gridCapFrac > 1e-9:
            errors.append(f"(C2) Grid cap fraction exceeded at h{e.hour}")
        if abs(e.chosenFrac - gridCapFrac) < 1e-9:
            cap_hits += 1

    # (C3) Monotonicity wrt curtailment: increasing curtailFrac decreases or equal energy
    def rerun_with_added_curtail(delta=0.2):
        D2 = {k: (v.copy() if isinstance(v, dict) else v) for k,v in parse_turtle_simple(DYNAMIC_TTL).items()}
        for s in listify(D2["ex:window"]["ex:hasSlot"]):
            node = D2[s]
            node["ex:curtailFrac"] = min(1.0, float(node.get("ex:curtailFrac", 0.0)) + delta)
        return specialize_driver(parse_turtle_simple(STATIC_TTL))(D2)
    evals2, totals2, _ = rerun_with_added_curtail(0.2)
    energy_drop = totals['energy_kWh'] - totals2['energy_kWh']
    if energy_drop < -1e-9:
        errors.append("(C3) Energy increased after *increasing* curtailment")

    # (C4) Determinism & order
    if [e.hour for e in evals] != sorted([e.hour for e in evals]):
        errors.append("(C4) Slots not in chronological order")

    if errors:
        print("❌ FAIL")
        for er in errors:
            print(" -", er)
        raise SystemExit(1)
    else:
        # Rich PASS summary
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Arithmetic re-check: {nslots} slots OK "
              f"(max Δsetpoint={c1_max_dp:.3e}, max Δenergy={c1_max_de:.3e} kWh, max Δrevenue={c1_max_dr:.3e} €)")
        print(f"  • [C2] Caps/flags OK: curfew slots={curfew_count}, non-operable slots={nonop_count}, "
              f"grid-cap hit slots={cap_hits}")
        print(f"  • [C3] Curtailment monotonicity OK: base {totals['energy_kWh']:.1f} → "
              f"+20% curtail {totals2['energy_kWh']:.1f} kWh (Δ={energy_drop:.1f} kWh ≤ 0)")
        print(f"  • [C4] Order OK: hours {[e.hour for e in evals]}")

if __name__ == "__main__":
    main()

