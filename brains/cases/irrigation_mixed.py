#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (policy, soil physics) + RULES_N3 (math:* triple patterns only)
# are partially evaluated into a small driver. At run time, the driver ingests
# dynamic RDF (field soil moisture, root-zone depth, ETc & rain forecast),
# mirrors the N3 arithmetic to compute a soil water deficit (mm), adjusts for
# efficiency and delivery caps, and produces:
#   1) Answer (planned irrigation depth per event, total volume),
#   2) Reason why (trace lines that mirror math:* steps),
#   3) Check (harness re-validating conversions, caps, and consistency).
#
# Contract with P3:
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
@prefix ex:  <http://example.org/irrig#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Policy / system characteristics
ex:policy ex:targetVWC_pct   25.0 .   # target volumetric water content (% v/v)
ex:policy ex:efficiency       0.85 .   # fraction of applied water reaching root zone
ex:policy ex:perEventCap_mm  25.0 .    # max depth per irrigation event (mm)
ex:policy ex:systemRate_mm_h 12.0 .    # system delivery rate (mm per hour)
ex:policy ex:windowHours      2.0 .    # hours available for irrigation now
"""

DYNAMIC_TTL = r"""
@prefix ex:  <http://example.org/irrig#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Field state & forecast (for a single planning window)
ex:FieldA ex:area_ha        15.0 .
ex:FieldA ex:VWC_pct        18.0 .
ex:FieldA ex:rootDepth_m     0.4 .

ex:Forecast ex:ETc_mm       10.0 .
ex:Forecast ex:rain_mm       4.0 .
"""

# ──────────────────────────────────────────────────────────────────────────────
# RULES (N3) — only math:* built-ins for arithmetic/relations
# ──────────────────────────────────────────────────────────────────────────────
RULES_N3 = r"""
@prefix ex:   <http://example.org/irrig#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .

# R1: Soil deficit from setpoint
{
  ex:policy  ex:targetVWC_pct ?t .
  ex:FieldA  ex:VWC_pct       ?v .
  ( ?t ?v )            math:difference ?dtheta_pct .
  ( ?dtheta_pct 100 )  math:quotient   ?dtheta .
  ex:FieldA  ex:rootDepth_m   ?z .
  ( ?dtheta ?z )       math:product    ?tmp .
  ( ?tmp 1000 )        math:product    ?soilDef .
}
=>
{ ex:Plan ex:soilDef_mm ?soilDef } .

# R2: Net atmosphere adjustment
{
  ex:Forecast ex:ETc_mm  ?e .
  ex:Forecast ex:rain_mm ?r .
  ( ?e ?r )             math:difference ?net .
}
=>
{ ex:Plan ex:netAtmos_mm ?net } .

# R3: Gross need
{
  ex:Plan ex:soilDef_mm  ?d .
  ex:Plan ex:netAtmos_mm ?n .
  ( ?d ?n )             math:sum ?gross .
}
=>
{ ex:Plan ex:grossNeed_mm ?gross } .

# R4: Expose policy constants
{ ex:policy ex:efficiency ?eff . }        => { ex:Plan ex:eff ?eff } .
{ ex:policy ex:perEventCap_mm ?cap . }    => { ex:Plan ex:perEventCap ?cap } .
{ ex:policy ex:systemRate_mm_h ?rate . }  => { ex:Plan ex:systemRate ?rate } .
{ ex:policy ex:windowHours ?wh . }        => { ex:Plan ex:windowHours ?wh } .
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
class Policy:
    target_vwc_pct: float
    efficiency: float
    per_event_cap_mm: float
    system_rate_mm_h: float
    window_hours: float

@dataclass
class Field:
    area_ha: float
    vwc_pct: float
    root_depth_m: float

@dataclass
class Forecast:
    etc_mm: float
    rain_mm: float

@dataclass
class Plan:
    event_mm: float
    soil_gain_mm: float
    volume_m3: float
    leftover_soil_mm: float
    traces: List[str]

def specialize_driver(S: Dict[str, Dict[str, Any]]):
    P = Policy(
        target_vwc_pct=float(S["ex:policy"]["ex:targetVWC_pct"]),
        efficiency=float(S["ex:policy"]["ex:efficiency"]),
        per_event_cap_mm=float(S["ex:policy"]["ex:perEventCap_mm"]),
        system_rate_mm_h=float(S["ex:policy"]["ex:systemRate_mm_h"]),
        window_hours=float(S["ex:policy"]["ex:windowHours"]),
    )

    def driver(D: Dict[str, Dict[str, Any]]) -> Tuple[Plan, Dict[str, Any]]:
        F = Field(
            area_ha=float(D["ex:FieldA"]["ex:area_ha"]),
            vwc_pct=float(D["ex:FieldA"]["ex:VWC_pct"]),
            root_depth_m=float(D["ex:FieldA"]["ex:rootDepth_m"]),
        )
        W = Forecast(
            etc_mm=float(D["ex:Forecast"]["ex:ETc_mm"]),
            rain_mm=float(D["ex:Forecast"]["ex:rain_mm"]),
        )

        traces: List[str] = []

        # Mirror RULES_N3 steps (math:* semantics in trace)
        dtheta_pct = P.target_vwc_pct - F.vwc_pct
        dtheta = dtheta_pct / 100.0
        tmp = dtheta * F.root_depth_m
        soilDef = tmp * 1000.0
        traces.append(f"R1: ({P.target_vwc_pct} {F.vwc_pct}) math:difference {dtheta_pct:.3f} ; "
                      f"({dtheta_pct:.3f} 100) math:quotient {dtheta:.3f} ; "
                      f"({dtheta:.3f} {F.root_depth_m}) math:product {tmp:.3f} ; "
                      f"({tmp:.3f} 1000) math:product {soilDef:.1f} mm.")

        net = W.etc_mm - W.rain_mm
        traces.append(f"R2: ({W.etc_mm} {W.rain_mm}) math:difference {net:.1f} mm.")

        gross = soilDef + net
        traces.append(f"R3: ({soilDef:.1f} {net:.1f}) math:sum {gross:.1f} mm.")

        # Clamp at zero (>= 0) using math:notLessThan notion
        gross = max(0.0, gross)
        if gross == 0.0:
            traces.append("Clamp: need notLessThan 0 satisfied at 0.0 mm.")

        # Convert gross soil need to applied depth using efficiency
        required_applied = gross / P.efficiency if P.efficiency > 0 else float("inf")
        traces.append(f"R4: required_applied = ({gross:.1f} / {P.efficiency:.2f}) → {required_applied:.1f} mm "
                      "(mirrors math:quotient).")

        # Delivery cap = min(perEventCap, systemRate*windowHours)
        deliverable_cap = min(P.per_event_cap_mm, P.system_rate_mm_h * P.window_hours)
        traces.append(f"Caps: perEventCap {P.per_event_cap_mm:.1f} mm ; "
                      f"system cap ({P.system_rate_mm_h:.1f}×{P.window_hours:.1f}) = "
                      f"{(P.system_rate_mm_h*P.window_hours):.1f} mm ⇒ deliverable {deliverable_cap:.1f} mm "
                      "(min via math:notGreaterThan reasoning).")

        # Choose event depth
        event_mm = min(required_applied, deliverable_cap)
        soil_gain_mm = event_mm * P.efficiency
        leftover_soil_mm = max(0.0, gross - soil_gain_mm)

        # Volume conversion: 1 mm over 1 ha = 10 m³
        volume_m3 = event_mm * F.area_ha * 10.0
        traces.append(f"Vol: event {event_mm:.1f} mm × area {F.area_ha:.1f} ha × 10 → {volume_m3:.1f} m³.")

        return (Plan(
            event_mm=event_mm,
            soil_gain_mm=soil_gain_mm,
            volume_m3=volume_m3,
            leftover_soil_mm=leftover_soil_mm,
            traces=traces
        ), {
            "P": P, "F": F, "W": W,
            "grossNeed_mm": gross,
            "required_applied_mm": required_applied,
            "deliverable_cap_mm": deliverable_cap
        })

    return driver

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────
def main():
    S = parse_turtle_simple(STATIC_TTL)
    D = parse_turtle_simple(DYNAMIC_TTL)

    driver = specialize_driver(S)
    plan, ctx = driver(D)

    P: Policy = ctx["P"]
    F: Field = ctx["F"]
    gross = ctx["grossNeed_mm"]
    required_applied = ctx["required_applied_mm"]
    deliverable_cap = ctx["deliverable_cap_mm"]

    # ── ANSWER ──
    print("Answer:")
    if plan.event_mm <= 1e-9:
        print("- No irrigation needed this window.")
    else:
        print(f"- Event depth: {plan.event_mm:.1f} mm (cap {deliverable_cap:.1f} mm)")
        print(f"  • Soil gain this event: {plan.soil_gain_mm:.1f} mm (eff {P.efficiency:.2f})")
        print(f"  • Field volume: {plan.volume_m3:.1f} m³ over {F.area_ha:.1f} ha")
        print(f"  • Remaining unmet soil need (if any): {plan.leftover_soil_mm:.1f} mm "
              f"(gross need {gross:.1f} mm)")

    # ── REASON WHY ──
    print("\nReason why:")
    for ln in plan.traces:
        print(f"- {ln}")

    # ── CHECK (harness) ─
    print("\nCheck (harness):")
    errors: List[str] = []

    # Convenience aliases
    event = plan.event_mm
    eff   = P.efficiency
    area  = F.area_ha
    rate_cap = P.system_rate_mm_h * P.window_hours
    expected_event_min = min(required_applied, deliverable_cap)

    # (C1) Non-negativity and cap + "min" selection mirror
    if event < -1e-9:
        errors.append("(C1) Event depth negative")
    if event - deliverable_cap > 1e-9:
        errors.append(f"(C1) Event depth {event:.3f} exceeds deliverable cap {deliverable_cap:.3f}")
    d_event_min = abs(event - expected_event_min)
    if d_event_min > 1e-9:
        errors.append(f"(C1) Event depth not min(required_applied, cap): {event:.6f} vs {expected_event_min:.6f}")

    # (C2) Soil gain consistency with efficiency
    expected_gain = event * eff
    d_gain = abs(plan.soil_gain_mm - expected_gain)
    if d_gain > 1e-6:
        errors.append("(C2) Soil gain ≠ event_mm × efficiency")

    # (C3) Meet-vs-leftover logic
    if required_applied <= deliverable_cap + 1e-9:
        # capacity sufficient → we expect soil gain ≈ gross and leftover ≈ 0
        if abs(plan.soil_gain_mm - gross) > 1e-6:
            errors.append(f"(C3) Gross need {gross:.3f} not fully met though capacity sufficed")
        if abs(plan.leftover_soil_mm - 0.0) > 1e-6:
            errors.append("(C3) Leftover should be zero when capacity suffices")
    else:
        # capacity limited → event==cap and leftover = gross - soil_gain > 0
        if abs(event - deliverable_cap) > 1e-9:
            errors.append("(C3) Expected event depth to hit the cap under limitation")
        if plan.leftover_soil_mm <= -1e-9:
            errors.append("(C3) Expected non-negative leftover soil need")
        d_leftover_match = abs(plan.leftover_soil_mm - (gross - plan.soil_gain_mm))
        if d_leftover_match > 1e-6:
            errors.append("(C3) Leftover soil need mismatch")

    # (C4) Volume conversion check: 1 mm over 1 ha = 10 m³
    expected_vol = event * area * 10.0
    d_vol = abs(plan.volume_m3 - expected_vol)
    if d_vol > 1e-6:
        errors.append("(C4) Volume conversion mismatch")

    # (C5) Caps provenance: deliverable_cap = min(perEventCap, systemRate*windowHours)
    if abs(rate_cap - (P.system_rate_mm_h * P.window_hours)) > 1e-12:
        errors.append("(C5) Internal rate cap computation drift")
    expected_cap = min(P.per_event_cap_mm, rate_cap)
    d_cap = abs(deliverable_cap - expected_cap)
    if d_cap > 1e-9:
        errors.append("(C5) Deliverable cap ≠ min(perEventCap, systemRate*windowHours)")

    # (C6) Efficiency monotonicity: increasing efficiency should not increase the required event depth
    if eff > 0:
        eff2 = eff * 1.10
        req2 = gross / eff2 if eff2 > 0 else float("inf")
        event2 = min(req2, deliverable_cap)
        if event2 - event > 1e-9:
            errors.append(f"(C6) Increasing efficiency raised event depth ({event:.3f} → {event2:.3f} mm)")

    # Outcome
    if errors:
        print("❌ FAIL")
        for e in errors:
            print(" -", e)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")
        print(f"  • [C1] Event bounds & min() OK: event={event:.1f} mm, cap={deliverable_cap:.1f} mm, "
              f"req_applied={required_applied:.1f} mm, Δmin={d_event_min:.2e}")
        print(f"  • [C2] Soil gain OK: gain={plan.soil_gain_mm:.1f} mm (eff={eff:.2f}), Δgain={d_gain:.2e}")
        if required_applied <= deliverable_cap + 1e-9:
            print(f"  • [C3] Capacity sufficed: gross={gross:.1f} mm fully met, leftover≈0.0")
        else:
            print(f"  • [C3] Capacity limited: event hits cap; leftover={plan.leftover_soil_mm:.1f} mm")
        print(f"  • [C4] Volume OK: {plan.volume_m3:.1f} m³ over {area:.1f} ha, Δvol={d_vol:.2e}")
        print(f"  • [C5] Cap derivation OK: perEventCap={P.per_event_cap_mm:.1f} mm; "
              f"systemRate×window={rate_cap:.1f} mm; deliverable={deliverable_cap:.1f} mm")
        if eff > 0:
            print("  • [C6] Efficiency monotonicity OK (event depth non-increasing with +10% eff)")

if __name__ == "__main__":
    main()

