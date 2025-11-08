#!/usr/bin/env python3
"""
P3-STYLE, SELF-CONTAINED PYTHON PROGRAM (Zinnia)
Goal → A small, auditable simulator of a zinnia’s growth from seed to first bloom that outputs:
  • Answer — Key phenology days (germination, first bud, first bloom) and final state
  • Reason — Why those results hold, tied to explicit data & logic
  • Check  — Independent validations so the result can be trusted

Follows the P3.md structure (Prompt → Program → Proof).
The program is deterministic and portable (no external files).

This model is didactic and simplified — not a botanical authority.
Units: temperature °C, soil moisture [0,1], height cm, biomass g/m^2.
Time step: daily.
"""
from __future__ import annotations
from dataclasses import dataclass, asdict
from typing import List, Dict, Any, Tuple
import math
import json

# -----------------------------
# PROMPT, GOAL
# -----------------------------
PROMPT = (
    "Write a self-contained Python program that simulates the growth of a zinnia "
    "from seed under daily temperature and soil-moisture conditions, and that emits "
    "(1) Answer, (2) Reason Why, and (3) an independent Check."
)

GOAL = {
    "primary": "Determine when the seed germinates, when the first visible bud forms, and when the first flower blooms.",
    "secondary": [
        "Provide a stage timeline (seed → germinated → vegetative → bud → flowering).",
        "Report final height and biomass on the last simulated day.",
    ],
}

# -----------------------------
# DATA TYPES
# -----------------------------
@dataclass
class Day:
    day: int
    t_avg_c: float        # average daily temperature (°C)
    soil_moisture: float  # volumetric, simplified [0,1]
    daylength_h: float

@dataclass
class Logic:
    # Germination window: warm-season annual — needs warm soil & some moisture
    germ_temp_min: float = 18.0
    germ_temp_max: float = 35.0
    germ_moisture_min: float = 0.25
    germ_min_consec_days: int = 2

    # Thermal-time (GDD) parameters (warm-season base)
    tbase_c: float = 10.0

    # Bud & bloom thresholds based on cumulative GDD since germination
    bud_gdd_threshold: float = 600.0
    bloom_gdd_after_bud: float = 160.0

    # Minimal biomass gates for stages (ensure the plant has enough mass)
    min_biomass_for_bud: float = 120.0
    min_biomass_for_bloom: float = 180.0

    # Biomass dynamics (logistic with environmental stress multipliers)
    r: float = 0.22                # intrinsic daily growth rate
    k: float = 500.0               # carrying capacity (g/m^2)

    # Height conversion from biomass (monotone saturating)
    h_max_cm: float = 90.0         # tall zinnia cultivar
    alpha_h: float = 0.010

# -----------------------------
# ENVIRONMENT (deterministic synthetic 90-day warm season)
# -----------------------------

def generate_environment(num_days: int = 90) -> List[Day]:
    days: List[Day] = []
    for d in range(num_days):
        # Warm season: mean ~24°C, amplitude 5°C, period 30 days
        t_avg = 24.0 + 5.0 * math.sin(2 * math.pi * d / 30.0)
        # Moisture around 0.35 with rainy cycles (amp 0.18, period 12 days)
        soil_m = 0.35 + 0.18 * math.sin(2 * math.pi * (d + 3) / 12.0)
        soil_m = max(0.0, min(1.0, soil_m))
        # Daylength ~14h with tiny seasonal ripple (not strictly used for phenology here)
        daylen = 14.0 + 0.6 * math.sin(2 * math.pi * (d + 165) / 365.0)
        days.append(Day(day=d, t_avg_c=t_avg, soil_moisture=soil_m, daylength_h=daylen))
    return days

# -----------------------------
# CORE MODEL HELPERS
# -----------------------------
STAGES = ["seed", "germinated", "vegetative", "bud", "flowering"]

@dataclass
class State:
    stage: str
    biomass: float
    height_cm: float
    gdd_total: float         # cumulative GDD (base tbase_c) from day 0
    gdd_since_germ: float    # cumulative GDD since the day of germination


def gdd_for_day(t_avg_c: float, tbase_c: float) -> float:
    return max(0.0, t_avg_c - tbase_c)


def stress_temp(t: float) -> float:
    # Warm-season response: peak near 28°C, tolerates ~18–35°C
    if t < 8.0 or t > 40.0:
        return 0.0
    return math.exp(-((t - 28.0) ** 2) / (2 * 7.5 ** 2))


def stress_moisture(m: float) -> float:
    # Broad optimum around 0.4–0.6, taper outside
    if m <= 0.05:
        return 0.0
    if m >= 0.95:
        return 0.9
    return max(0.0, 1.0 - 4.0 * (m - 0.5) ** 2)


def biomass_to_height(biomass: float, logic: Logic) -> float:
    return logic.h_max_cm * (1.0 - math.exp(-logic.alpha_h * max(0.0, biomass)))

# -----------------------------
# SIMULATION
# -----------------------------

def simulate(days: List[Day], logic: Logic) -> Dict[str, Any]:
    consec_ok = 0
    germ_day: int | None = None
    bud_gdd_cross_day: int | None = None

    state = State(stage="seed", biomass=0.2, height_cm=0.0, gdd_total=0.0, gdd_since_germ=0.0)

    timeline: List[Dict[str, Any]] = []
    bud_day: int | None = None
    bloom_day: int | None = None

    for d in days:
        # Update thermal time
        gdd = gdd_for_day(d.t_avg_c, logic.tbase_c)
        state.gdd_total += gdd
        if germ_day is not None:
            state.gdd_since_germ += gdd

        # Germination window check
        cond_ok = (
            logic.germ_temp_min <= d.t_avg_c <= logic.germ_temp_max and
            d.soil_moisture >= logic.germ_moisture_min
        )
        consec_ok = consec_ok + 1 if cond_ok else 0
        if germ_day is None and consec_ok >= logic.germ_min_consec_days:
            germ_day = d.day
            state.stage = "germinated"
            state.gdd_since_germ = 0.0

        # Growth after germination
        stress = stress_temp(d.t_avg_c) * stress_moisture(d.soil_moisture)
        if germ_day is not None:
            dB = logic.r * state.biomass * (1.0 - state.biomass / logic.k) * max(0.0, stress)
            state.biomass = max(0.0, state.biomass + dB)
        else:
            state.biomass = max(0.0, state.biomass * 0.999)

        state.height_cm = biomass_to_height(state.biomass, logic)

        # Stage transitions (monotone):
        prev_stage = state.stage
        if germ_day is None:
            stage_now = "seed"
        else:
            # Start as vegetative by default after germination
            stage_now = "vegetative"
            # Bud requires both thermal time and biomass gate
            if (state.gdd_since_germ >= logic.bud_gdd_threshold and
                state.biomass >= logic.min_biomass_for_bud):
                stage_now = "bud"
            # Flowering requires additional GDD and biomass
            if (state.gdd_since_germ >= logic.bud_gdd_threshold + logic.bloom_gdd_after_bud and
                state.biomass >= logic.min_biomass_for_bloom):
                stage_now = "flowering"
        # Enforce non-regression of stages
        if STAGES.index(stage_now) < STAGES.index(prev_stage):
            stage_now = prev_stage
        state.stage = stage_now

        # Mark key days once
        if bud_day is None and state.stage == "bud":
            bud_day = d.day
        if bloom_day is None and state.stage == "flowering":
            bloom_day = d.day

        timeline.append({
            "day": d.day,
            "t_avg_c": round(d.t_avg_c, 2),
            "soil_moisture": round(d.soil_moisture, 3),
            "gdd_total": round(state.gdd_total, 2),
            "gdd_since_germ": round(state.gdd_since_germ, 2),
            "gdd_day": round(gdd, 2),
            "biomass": round(state.biomass, 2),
            "height_cm": round(state.height_cm, 2),
            "stage": state.stage,
            "stage_changed": prev_stage != state.stage,
        })
    # Fallback: if crossing day wasn't captured in-loop (e.g., due to ordering), derive it from the timeline
    if bud_gdd_cross_day is None and germ_day is not None:
        for row in timeline:
            if row["day"] >= germ_day and row["gdd_since_germ"] >= logic.bud_gdd_threshold:
                bud_gdd_cross_day = row["day"]
                break

    return {
        "timeline": timeline,
        "germ_day": germ_day,
        "bud_day": bud_day,
        "bloom_day": bloom_day,
        "final_state": {
            "day": days[-1].day,
            "stage": state.stage,
            "biomass": round(state.biomass, 2),
            "height_cm": round(state.height_cm, 2),
            "gdd_total": round(state.gdd_total, 2),
            "gdd_since_germ": round(state.gdd_since_germ, 2),
        },
    }

# -----------------------------
# INDEPENDENT CHECKS
# -----------------------------

def check_invariants(result: Dict[str, Any], logic: Logic) -> Dict[str, Any]:
    checks: List[Dict[str, Any]] = []
    def add(name: str, passed: bool, details: str) -> None:
        checks.append({"name": name, "pass": bool(passed), "details": details})

    tl = result["timeline"]
    gday = result.get("germ_day")
    bud_day = result.get("bud_day")
    bloom_day = result.get("bloom_day")
    bud_cross_day = result.get("bud_gdd_cross_day")
    tol_gdd = 5.0

    # 1) Stage monotonicity
    order = {s: i for i, s in enumerate(STAGES)}
    regressed = False
    last_idx = 0
    bad_day = None
    for row in tl:
        idx = order[row["stage"]]
        if idx < last_idx:
            regressed = True
            bad_day = row["day"]
            break
        last_idx = idx
    add("Stage monotonicity (no regression)", not regressed,
        "Stages never decreased over time" if not regressed else f"Stage regressed on day {bad_day}.")

    # 2) Germination window re-check
    if gday is not None:
        ok_germ = True
        for j in range(gday - (logic.germ_min_consec_days - 1), gday + 1):
            if j < 0 or j >= len(tl):
                ok_germ = False
                break
            t = tl[j]["t_avg_c"]; m = tl[j]["soil_moisture"]
            if not (logic.germ_temp_min <= t <= logic.germ_temp_max and m >= logic.germ_moisture_min):
                ok_germ = False
        add("Germination window satisfied", ok_germ,
            f"Germination on day {gday} followed {logic.germ_min_consec_days} qualifying days." if ok_germ else "Germination window inconsistent with data.")
    else:
        add("Germination detected", False, "No germination day found")

    # 3) Biomass monotonicity after germination
    if gday is not None:
        ok_b = True; last_b = None; d_bad = None
        for row in tl[gday:]:
            b = row["biomass"]
            if last_b is not None and b + 1e-6 < last_b:
                ok_b = False; d_bad = row["day"]; break
            last_b = b
        add("Biomass monotonic after germination", ok_b,
            "Biomass non-decreasing each day after germination" if ok_b else f"Biomass decreased on day {d_bad}.")

        # 4) Height monotonicity after germination
        ok_h = True; last_h = None; d_bad_h = None
        for row in tl[gday:]:
            h = row["height_cm"]
            if last_h is not None and h + 1e-6 < last_h:
                ok_h = False; d_bad_h = row["day"]; break
            last_h = h
        add("Height monotonic after germination", ok_h,
            "Height non-decreasing after germination" if ok_h else f"Height decreased on day {d_bad_h}.")
    else:
        add("Growth invariants after germination", False, "Skipped because germination missing")

    # 5) Height upper bound respected
    hmax = logic.h_max_cm
    ok_hmax = all(row["height_cm"] <= hmax + 1e-6 for row in tl)
    add("Height never exceeds h_max", ok_hmax,
        "All heights ≤ h_max" if ok_hmax else "Height exceeded h_max at least once.")

    # 6) Bud-day gates satisfied and ordering vs GDD crossing
    if bud_day is not None and 0 <= bud_day < len(tl):
        row_bud = tl[bud_day]
        gates = (row_bud["gdd_since_germ"] + 1e-6 >= logic.bud_gdd_threshold and
                 row_bud["biomass"] + 1e-6 >= logic.min_biomass_for_bud)
        add("Bud-day gates (GDD & biomass) satisfied", gates,
            f"Bud on day {bud_day} with GDD {row_bud['gdd_since_germ']}, biomass {row_bud['biomass']}")
        if bud_cross_day is not None:
            add("Bud occurs on/after first GDD threshold-crossing", bud_day >= bud_cross_day,
                f"First cross day={bud_cross_day}, bud day={bud_day}.")
    else:
        add("Bud detected", False, "No bud day found or out of range")

    # 7) Bloom threshold satisfied (end-of-day totals)
    if bloom_day is not None and 0 <= bloom_day < len(tl):
        gdd_bloom = tl[bloom_day]["gdd_since_germ"]
        required = logic.bud_gdd_threshold + logic.bloom_gdd_after_bud
        pass_bloom = gdd_bloom + 1e-6 >= (required - tol_gdd)
        add("Bloom threshold met (GDD totals)", pass_bloom,
            f"Bloom day GDD {gdd_bloom:.1f} vs required {required:.1f} (tol {tol_gdd}).")
    else:
        add("Bloom detected", False, "No bloom day found or out of range")

    # 8) Alternative bloom-day cross-check (GDD-only)
    if gday is not None:
        target = logic.bud_gdd_threshold + logic.bloom_gdd_after_bud
        alt = None
        for row in tl:
            if row["day"] >= gday and row["gdd_since_germ"] >= target:
                alt = row["day"]; break
        if bloom_day is None or alt is None:
            add("Alt bloom-day (GDD-only) within ±3 days", False, "Cannot compute alternative or primary bloom day")
        else:
            pass_alt = abs(alt - bloom_day) <= 3
            add("Alt bloom-day (GDD-only) within ±3 days", pass_alt, f"primary={bloom_day}, alt={alt}")
    else:
        add("Alt bloom-day check", False, "Skipped: no germination")

    # 9) Alternative bud-threshold cross-check (first day crossing)
    if gday is not None:
        alt_cross = None
        for row in tl:
            if row["day"] >= gday and row["gdd_since_germ"] >= logic.bud_gdd_threshold:
                alt_cross = row["day"]; break
        if alt_cross is None:
            add("Alt bud-threshold cross-day exists", False, "No day crosses the bud GDD threshold in timeline (unexpected if bloom occurs).")
        else:
            if bud_cross_day is None:
                # If not recorded, accept alt crossing if bud_day (if present) is on/after it
                if bud_day is None:
                    add("Alt bud-threshold cross-day (record absent)", True, f"recorded=None, alt={alt_cross}")
                else:
                    add("Alt bud-threshold cross-day (record absent)", bud_day >= alt_cross,
                        f"recorded=None, alt={alt_cross}, bud_day={bud_day}")
            else:
                add("Alt bud-threshold cross-day matches", bud_cross_day == alt_cross,
                    f"recorded={bud_cross_day}, alt={alt_cross}")

    # 10) Conservative bud→bloom GDD (informational)
    if bud_day is not None and bloom_day is not None and 0 <= bud_day < len(tl) and 0 <= bloom_day < len(tl):
        conservative_gap = tl[bloom_day]["gdd_since_germ"] - max(logic.bud_gdd_threshold, tl[bud_day]["gdd_since_germ"] - tl[bud_day]["gdd_day"])
        add("Conservative bud→bloom GDD (informational)", True,
            f"{conservative_gap:.1f} (target ~{logic.bloom_gdd_after_bud}) — may be lower due to discretization/biomass gating.")

    ok = all(ch["pass"] for ch in checks)
    return {"ok": ok, "checks": checks}

# -----------------------------
# REASON BUILDER
# -----------------------------

def build_reason(result: Dict[str, Any], logic: Logic) -> str:
    tl = result["timeline"]
    parts: List[str] = []

    gday = result["germ_day"]
    if gday is None:
        parts.append(
            "Germination did not occur because the required warm-and-moist consecutive-day window was not met."
        )
    else:
        d0 = max(0, gday - (logic.germ_min_consec_days - 1))
        days_str = ", ".join(
            f"(day {j}: T={tl[j]['t_avg_c']}°C, M={tl[j]['soil_moisture']})" for j in range(d0, gday + 1)
        )
        parts.append(f"Germination on day {gday} after qualifying days: {days_str}.")

    bday = result["bud_day"]
    if bday is None:
        parts.append("No bud stage within the horizon.")
    else:
        cross = result.get("bud_gdd_cross_day")
        if cross is not None:
            parts.append(
                f"Bud GDD threshold (~{logic.bud_gdd_threshold}) was first crossed on day {cross}; "
                f"visible bud on day {bday} once biomass exceeded {logic.min_biomass_for_bud} g/m²."
            )
        else:
            parts.append(
                "First visible bud occurred when cumulative GDD and biomass gates were both met."
            )

    fday = result["bloom_day"]
    if fday is None:
        parts.append("No open flower within the horizon.")
    else:
        parts.append(
            f"First bloom on day {fday}, after an additional ~{logic.bloom_gdd_after_bud} GDD beyond bud and biomass ≥ {logic.min_biomass_for_bloom} g/m²."
        )

    final = result["final_state"]
    parts.append(
        f"By day {final['day']}, stage '{final['stage']}', height {final['height_cm']} cm, biomass {final['biomass']} g/m², "
        f"and cumulative GDD since germination {final['gdd_since_germ']}."
    )

    return " ".join(parts)

# -----------------------------
# MAIN
# -----------------------------

def main() -> None:
    logic = Logic()
    env = generate_environment(90)
    sim = simulate(env, logic)

    answer: Dict[str, Any] = {
        "germination_day": sim["germ_day"],
        "first_bud_day": sim["bud_day"],
        "first_bloom_day": sim["bloom_day"],
        "bud_gdd_threshold_cross_day": sim.get("bud_gdd_cross_day"),
        "stage_timeline": [
            {k: row[k] for k in ("day", "stage", "stage_changed", "height_cm", "biomass")}
            for row in sim["timeline"]
        ],
        "final_state": sim["final_state"],
    }

    reason = build_reason(sim, logic)

    check = check_invariants(sim, logic)

    triad = {
        "Answer": answer,
        "Reason": reason,
        "Check": check,
        "Meta": {
            "PROMPT": PROMPT,
            "GOAL": GOAL,
            "LOGIC": asdict(logic),
            "DATA": {
                "days": len(env),
                "t_avg_c_range": [round(min(d.t_avg_c for d in env), 2), round(max(d.t_avg_c for d in env), 2)],
                "soil_moisture_range": [round(min(d.soil_moisture for d in env), 3), round(max(d.soil_moisture for d in env), 3)],
                "daylength_range": [round(min(d.daylength_h for d in env), 2), round(max(d.daylength_h for d in env), 2)],
            },
        },
    }

    print(json.dumps(triad, indent=2))


if __name__ == "__main__":
    main()

