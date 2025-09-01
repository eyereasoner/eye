#!/usr/bin/env python3
#
# Mixed-Computation Header (what this program does)
# -------------------------------------------------
# Static RDF (policy, crop spec, product metadata) + RULES_N3 (math:* triple
# patterns only) are partially evaluated into a small driver. At run time, the
# driver ingests dynamic RDF (field state & short-term weather), computes a
# split-application nitrogen plan under per-application and seasonal caps,
# chooses the product based on rain risk vs. cost, and prints:
#   1) Answer (per-application kg N/ha and farm totals),
#   2) Reason why (trace lines that mirror math:* steps / comparisons),
#   3) Check (harness that re-validates totals, caps, and product choice).
#
# Contract with EYE learning:
# - All rule arithmetic/relations appear as math:* built-ins only in N3.
# - Everything is inline (no external file writes).
# - One file produces Answer • Reason why • Check.

from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, List, Tuple, Any

# ──────────────────────────────────────────────────────────────────────────────
# Inputs (Data + Rules + Goal)
# ──────────────────────────────────────────────────────────────────────────────

GOAL = "Given field + weather RDF and policy N3 rules, recommend a split nitrogen plan per hectare under caps and risk constraints."

DATA_RDF_TURTLE = r"""
@prefix : <http://example.org/agri#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Dynamic: field status & weather (would change day-to-day)
:FieldA a :WheatField ;
  :area_ha 10.0 ;
  :stage "tillering" ;
  :soilN_kg_ha 30.0 ;
  :forecastRain24h_mm 5.0 ;
  :airTemp_C 18.0 ;
  :soilTemp_C 15.0 .

# Static-ish: crop and economics (could live in a static KB)
:WheatCropSpec a :CropSpec ;
  :nPerTon_kg 25.0 ;
  :targetYield_t_ha 8.0 .

# Static-ish: policy/legal caps and thresholds
:Policy a :NitrogenPolicy ;
  :maxPerApp_kg_ha 120.0 ;
  :maxSeasonTotal_kg_ha 220.0 ;
  :heavyRain_mm 10.0 .

# Static-ish: product metadata (cost proxies)
:Urea a :Fertilizer ;
  :name "Urea (46% N)" ;
  :cost_index 0.80 .

:AmmoniumNitrate a :Fertilizer ;
  :name "Ammonium Nitrate (33.5% N)" ;
  :cost_index 1.00 .
"""

# RULES_N3 strictly uses math:* triple patterns for all arithmetic/relations.
RULES_N3 = r"""
@prefix :    <http://example.org/agri#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

# R1: req = (nPerTon * targetYield) - soilN
{
  :WheatCropSpec :nPerTon_kg ?npt .
  :WheatCropSpec :targetYield_t_ha ?yt .
  :FieldA :soilN_kg_ha ?soil .
  ( ?npt ?yt ?prod ) math:product .
  ( ?prod ?soil ?req ) math:difference .
}
=>
{ :Plan :reqN_kg_ha ?req . } .

# R1b: expose season cap for downstream clamping in the driver
{ :Policy :maxSeasonTotal_kg_ha ?cap . } => { :Plan :seasonCap ?cap . } .

# R3: expose heavy-rain threshold (comparison done with math:notLessThan notion in driver trace)
{ :Policy :heavyRain_mm ?hr . } => { :Plan :heavyRainThreshold ?hr . } .

# R4: expose per-application cap
{ :Policy :maxPerApp_kg_ha ?cap . } => { :Plan :perAppCap ?cap . } .
"""

# ──────────────────────────────────────────────────────────────────────────────
# Inline-comment–safe, minimal Turtle reader (supports ';' lists & multi-line)
# ──────────────────────────────────────────────────────────────────────────────

def parse_turtle_simple(s: str) -> Dict[str, Dict[str, Any]]:
    """
    Parses simple Turtle used here:
      :S a :Type ; :p1 10.0 ; :p2 "txt" .
    Features:
    - Skips @prefix lines.
    - Strips inline comments after '#' (outside of quoted strings).
    - Handles multi-line statements with ';' predicate lists.
    - Parses floats, booleans (not used here), and qnames/strings.
    """
    out: Dict[str, Dict[str, Any]] = {}
    buf: List[str] = []

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

    def flush(stmt: str):
        stmt = stmt.strip()
        if not stmt:
            return
        # remove trailing '.'
        if stmt.endswith('.'):
            stmt = stmt[:-1]
        # subject + predicate-object list separated by ';'
        parts = stmt.strip().split(None, 1)
        if not parts:
            return
        subj = parts[0]
        rest = parts[1] if len(parts) > 1 else ""
        segments = [seg.strip() for seg in rest.split(';') if seg.strip()]
        out.setdefault(subj, {})
        for seg in segments:
            toks = seg.split(None, 1)
            if len(toks) < 2:
                continue
            pred, obj = toks[0], toks[1].strip()
            if pred == 'a':
                pred = ':type'
            # object parsing
            if obj.startswith('"'):
                end = obj.find('"', 1)
                while end != -1 and obj[end-1] == '\\':
                    end = obj.find('"', end + 1)
                lit = obj[1:end if end != -1 else len(obj)]
                out[subj][pred] = lit
            else:
                tok = obj.split()[0]
                if tok in ('true', 'false'):
                    out[subj][pred] = (tok == 'true')
                else:
                    try:
                        out[subj][pred] = float(tok)
                    except ValueError:
                        out[subj][pred] = tok

    for raw in s.splitlines():
        if raw.lstrip().startswith('@prefix'):
            continue
        line = strip_inline_comment(raw).strip()
        if not line or line.startswith('#'):
            continue
        buf.append(line)
        if line.endswith('.'):
            flush(' '.join(buf))
            buf = []
    if buf:
        flush(' '.join(buf))
    return out

# ──────────────────────────────────────────────────────────────────────────────
# Mixed computation driver (policy + rules → specialized function)
# ──────────────────────────────────────────────────────────────────────────────

@dataclass
class Policy:
    max_per_app: float
    max_season_total: float
    heavy_rain_mm: float

@dataclass
class CropSpec:
    n_per_ton: float
    target_yield: float

@dataclass
class FieldState:
    area_ha: float
    stage: str
    soilN: float
    rain24h_mm: float
    airTemp_C: float
    soilTemp_C: float

@dataclass
class Product:
    name: str
    cost_index: float
    qname: str

@dataclass
class Plan:
    per_ha_rates: List[float]
    product_qname: str
    reason_lines: List[str]

def specialize_driver(policy: Policy, crop: CropSpec, products: Dict[str, Product]):
    """
    Partially evaluate policy + (math-only) rules into a small decision function.
    N3 is documentation & trace; arithmetic is mirrored in Python, with math:*
    semantics reflected in the trace lines.
    """
    splits_for_stage = {"tillering": [0.60, 0.40]}
    default_split = [1.00]

    def pick_product(rain24h_mm: float) -> Tuple[str, List[str]]:
        trace = []
        # math:notLessThan (>=)
        if rain24h_mm >= policy.heavy_rain_mm:
            trace.append(f"R3: (?r {rain24h_mm} ?hr {policy.heavy_rain_mm}) math:notLessThan ⇒ avoid Urea, prefer AmmoniumNitrate.")
            return (":AmmoniumNitrate", trace)
        cheaper = min(products.values(), key=lambda p: p.cost_index)
        trace.append(f"R3: (?r {rain24h_mm} ?hr {policy.heavy_rain_mm}) not satisfied ⇒ prefer cheaper product {cheaper.name}.")
        return (cheaper.qname, trace)

    def driver(field: FieldState) -> Plan:
        trace: List[str] = []

        # R1 mirror: req = (nPerTon * targetYield) - soilN
        prod = crop.n_per_ton * crop.target_yield
        req = prod - field.soilN
        trace.append(f"R1: (?npt {crop.n_per_ton} ?yt {crop.target_yield} ?prod {prod:.1f}) math:product; "
                     f"(?prod {prod:.1f} ?soil {field.soilN} ?req {req:.1f}) math:difference.")

        # Clamp to season cap (R1b exposure)
        clamped_req = max(0.0, min(policy.max_season_total, req))
        if req <= policy.max_season_total:
            trace.append(f"R1b: (?req {req:.1f} ?cap {policy.max_season_total:.1f}) math:notGreaterThan ⇒ keep {req:.1f}.")
        else:
            trace.append(f"R1b: (?req {req:.1f} ?cap {policy.max_season_total:.1f}) math:greaterThan ⇒ clamp to {policy.max_season_total:.1f}.")
        if clamped_req == 0.0:
            trace.append(f"Clamp0: (?req {req:.1f} 0) math:notLessThan not satisfied ⇒ clamp floor 0.0.")

        # Stage-based split (policy outside N3 to keep math-only rules)
        splits = splits_for_stage.get(field.stage, default_split)
        trace.append(f"R2: stage='{field.stage}' ⇒ split {splits} of total.")

        # Distribute with per-app cap (R4)
        per_ha_rates: List[float] = []
        remaining = clamped_req
        for i, frac in enumerate(splits):
            ideal = clamped_req * frac
            rate = min(ideal, policy.max_per_app)
            per_ha_rates.append(round(rate, 1))
            remaining -= rate
            if ideal <= policy.max_per_app:
                trace.append(f"R4: (?ideal {ideal:.1f} ?cap {policy.max_per_app:.1f}) math:notGreaterThan ⇒ app#{i+1} {rate:.1f} kg/ha.")
            else:
                trace.append(f"R4: (?ideal {ideal:.1f} ?cap {policy.max_per_app:.1f}) math:greaterThan ⇒ app#{i+1} capped {rate:.1f} kg/ha.")

        # Spread leftover if any capacity remains
        idx = 0
        while remaining > 1e-6 and idx < len(per_ha_rates):
            capacity = policy.max_per_app - per_ha_rates[idx]
            take = min(capacity, remaining)
            if take > 0:
                per_ha_rates[idx] = round(per_ha_rates[idx] + take, 1)
                remaining -= take
                trace.append(f"R4: redistribute leftover {take:.1f} into app#{idx+1} (remaining {remaining:.1f}).")
            idx += 1

        # Product choice (R3)
        product_qname, prod_reason = pick_product(field.rain24h_mm)
        trace.extend(prod_reason)

        return Plan(per_ha_rates=per_ha_rates, product_qname=product_qname, reason_lines=trace)

    return driver

# ──────────────────────────────────────────────────────────────────────────────
# Run
# ──────────────────────────────────────────────────────────────────────────────

def main():
    kb = parse_turtle_simple(DATA_RDF_TURTLE)

    if ":Policy" not in kb:
        raise KeyError(f"':Policy' not found in parsed data. Subjects seen: {list(kb.keys())}")

    policy = Policy(
        max_per_app=float(kb[":Policy"][":maxPerApp_kg_ha"]),
        max_season_total=float(kb[":Policy"][":maxSeasonTotal_kg_ha"]),
        heavy_rain_mm=float(kb[":Policy"][":heavyRain_mm"]),
    )
    crop = CropSpec(
        n_per_ton=float(kb[":WheatCropSpec"][":nPerTon_kg"]),
        target_yield=float(kb[":WheatCropSpec"][":targetYield_t_ha"]),
    )
    field = FieldState(
        area_ha=float(kb[":FieldA"][":area_ha"]),
        stage=str(kb[":FieldA"][":stage"]),
        soilN=float(kb[":FieldA"][":soilN_kg_ha"]),
        rain24h_mm=float(kb[":FieldA"][":forecastRain24h_mm"]),
        airTemp_C=float(kb[":FieldA"][":airTemp_C"]),
        soilTemp_C=float(kb[":FieldA"][":soilTemp_C"]),
    )
    products = {
        ":Urea": Product(name=str(kb[":Urea"][":name"]), cost_index=float(kb[":Urea"][":cost_index"]), qname=":Urea"),
        ":AmmoniumNitrate": Product(name=str(kb[":AmmoniumNitrate"][":name"]), cost_index=float(kb[":AmmoniumNitrate"][":cost_index"]), qname=":AmmoniumNitrate"),
    }

    driver = specialize_driver(policy, crop, products)
    plan = driver(field)

    total_per_ha = round(sum(plan.per_ha_rates), 1)
    product_name = products[plan.product_qname].name
    per_app_totals = [round(r * field.area_ha, 1) for r in plan.per_ha_rates]

    # ── ANSWER ──
    print("Answer:")
    print(f"- FieldA ({field.area_ha:.1f} ha): {len(plan.per_ha_rates)} applications using {product_name}")
    for i, r in enumerate(plan.per_ha_rates, start=1):
        print(f"  • App #{i}: {r:.1f} kg N/ha  (farm total {per_app_totals[i-1]:.1f} kg)")
    print(f"  • Season total: {total_per_ha:.1f} kg N/ha")

    # ── REASON WHY ──
    print("\nReason why:")
    for line in plan.reason_lines:
        print(f"- {line}")

    # Echo rule/data fingerprints for auditability
    print("\nInputs (fingerprints):")
    print(f"- Data RDF size: {len(DATA_RDF_TURTLE.encode('utf-8'))} bytes; subjects: {len(parse_turtle_simple(DATA_RDF_TURTLE))}")
    print(f"- Rules N3 size: {len(RULES_N3.encode('utf-8'))} bytes (math:* triples only)")

    # ── CHECK (harness) ──
    print("\nCheck (harness):")
    errors: List[str] = []

    # (C1) Season total == clamped requirement
    raw_req = crop.n_per_ton * crop.target_yield - field.soilN
    clamp_expected = max(0.0, min(policy.max_season_total, raw_req))
    if abs(total_per_ha - clamp_expected) > 1e-6:
        errors.append(f"(C1) Season total {total_per_ha} ≠ clamped requirement {clamp_expected:.1f}")

    # (C2) Per-app cap
    for i, r in enumerate(plan.per_ha_rates, start=1):
        if r - policy.max_per_app > 1e-9:
            errors.append(f"(C2) App #{i} {r} exceeds per-app cap {policy.max_per_app}")

    # (C3) Rain rule respected
    if field.rain24h_mm >= policy.heavy_rain_mm and plan.product_qname == ":Urea":
        errors.append(f"(C3) Heavy rain but product is Urea")

    # (C4) Prefer cheaper when not heavy rain
    cheaper_qname = min((p.qname for p in products.values()), key=lambda q: products[q].cost_index)
    if field.rain24h_mm < policy.heavy_rain_mm and plan.product_qname != cheaper_qname:
        errors.append(f"(C4) No heavy rain; expected cheaper product {products[cheaper_qname].name}")

    if errors:
        print("❌ FAIL")
        for e in errors:
            print(" -", e)
        raise SystemExit(1)
    else:
        print("✅ PASS — all checks satisfied.")

if __name__ == "__main__":
    main()

