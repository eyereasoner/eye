#!/usr/bin/env python3
"""
P3-STYLE, SELF-CONTAINED PYTHON PROGRAM
Goal → A small, auditable simulator of a grass seed’s growth that outputs:
  • Answer — What happened (key dates, final state)
  • Reason — Why those results hold, tied to explicit data & logic
  • Check  — Independent validations so the result can be trusted

The program is deliberately deterministic and portable (no external files needed).
It embeds:
  • PROMPT: human-readable description of the task
  • GOAL: precise question(s) the program must answer
  • DATA: synthetic but realistic daily environment signals for 60 days
  • LOGIC: explicit rules for germination and biomass/height dynamics
Then it produces the triad (Answer, Reason, Check) as JSON on stdout.

Notes
-----
• This is a didactic model, not a botanical authority.
• Units: temperature °C, soil moisture in [0,1], height cm, biomass g/m^2.
• Time step: 1 day.
"""
from __future__ import annotations
from dataclasses import dataclass, asdict
from typing import List, Dict, Any, Tuple
import math
import json

# -----------------------------
# PROMPT, GOAL, DATA, LOGIC
# -----------------------------
PROMPT = (
    "Write a self-contained Python program that simulates the growth of a grass seed "
    "under daily temperature and soil-moisture conditions, and that emits three things: "
    "(1) Answer, (2) Reason Why, and (3) an independent Check."
)

GOAL = {
    "primary": "Determine when (on which day) the seed germinates and when the plant first reaches mowable height (>= 7.0 cm).",
    "secondary": [
        "Report the stage timeline (seed → germinated → seedling → tillering → vegetative).",
        "Report final height and biomass on the last simulated day.",
    ],
}

@dataclass
class Day:
    day: int
    t_avg_c: float        # average daily temperature (°C)
    soil_moisture: float  # volumetric, simplified [0,1]
    daylength_h: float    # hours of daylight

@dataclass
class Logic:
    # Germination requires at least these conditions for N consecutive days
    germ_temp_min: float = 10.0    # °C
    germ_temp_max: float = 30.0    # °C
    germ_moisture_min: float = 0.30
    germ_min_consec_days: int = 3

    # Thermal-time (GDD) parameters (base temp for cool-season grass)
    tbase_c: float = 5.0

    # Biomass dynamics (simple logistic with stress multipliers)
    r: float = 0.20               # intrinsic growth rate per day
    k: float = 300.0              # carrying capacity (g/m^2)

    # Height conversion from biomass (empirical, monotonic saturating)
    # height_cm = h_max * (1 - exp(-alpha * biomass))
    h_max_cm: float = 25.0
    alpha_h: float = 0.02

    # Stage thresholds by biomass (g/m^2)
    stage_seedling: float = 10.0
    stage_tillering: float = 50.0
    stage_vegetative: float = 150.0

    # Mowable threshold
    mowable_height_cm: float = 7.0

# -----------------------------
# DATA GENERATION (deterministic)
# -----------------------------

def generate_environment(num_days: int = 60) -> List[Day]:
    """Deterministic synthetic series with gentle oscillations.
    Designed to be reasonable for spring-like conditions."""
    days: List[Day] = []
    for d in range(num_days):
        # Temperature oscillates around 17°C with amplitude 6°C, period 30 days
        t_avg = 17.0 + 6.0 * math.sin(2 * math.pi * d / 30.0)
        # Soil moisture oscillates around 0.45 with amplitude 0.12, period 15 days (phase shift)
        soil_m = 0.45 + 0.12 * math.sin(2 * math.pi * (d + 5) / 15.0)
        # Daylength around 13h with small seasonal trend
        daylen = 13.0 + 1.2 * math.sin(2 * math.pi * (d + 90) / 365.0)
        # Clamp moisture to [0,1]
        soil_m = max(0.0, min(1.0, soil_m))
        days.append(Day(day=d, t_avg_c=t_avg, soil_moisture=soil_m, daylength_h=daylen))
    return days

# -----------------------------
# CORE MODEL
# -----------------------------

@dataclass
class State:
    stage: str            # one of: seed, germinated, seedling, tillering, vegetative
    biomass: float        # g/m^2
    height_cm: float
    cum_gdd: float        # °C·days above base

STAGE_ORDER = ["seed", "germinated", "seedling", "tillering", "vegetative"]


def gdd_for_day(t_avg_c: float, tbase_c: float) -> float:
    return max(0.0, t_avg_c - tbase_c)


def stress_temp(t: float, logic: Logic) -> float:
    # Piecewise thermal response: ideal ~18–24°C, taper outside 10–30°C
    if t < 5.0 or t > 35.0:
        return 0.0
    # smooth bell-ish curve around 21°C
    return math.exp(-((t - 21.0) ** 2) / (2 * 7.0 ** 2))


def stress_moisture(m: float) -> float:
    # 0.3–0.7 is good, taper outside
    if m <= 0.05:
        return 0.0
    if m >= 0.95:
        return 0.9
    # quadratic bowl centered at 0.5 (normalized)
    return max(0.0, 1.0 - 4.0 * (m - 0.5) ** 2)


def biomass_to_height(biomass: float, logic: Logic) -> float:
    return logic.h_max_cm * (1.0 - math.exp(-logic.alpha_h * max(0.0, biomass)))


def next_stage(current_stage: str, biomass: float, germinated: bool, logic: Logic) -> str:
    if current_stage == "seed":
        return "germinated" if germinated else "seed"
    if current_stage in ("germinated", "seed"):
        if biomass >= logic.stage_seedling:
            return "seedling"
        return current_stage
    if current_stage == "seedling":
        if biomass >= logic.stage_tillering:
            return "tillering"
        return current_stage
    if current_stage == "tillering":
        if biomass >= logic.stage_vegetative:
            return "vegetative"
        return current_stage
    return current_stage


def simulate(days: List[Day], logic: Logic) -> Dict[str, Any]:
    # Track consecutive days satisfying germination conditions
    consec_ok = 0
    germ_day: int | None = None

    state = State(stage="seed", biomass=0.1, height_cm=0.0, cum_gdd=0.0)

    timeline: List[Dict[str, Any]] = []
    mow_day: int | None = None

    for d in days:
        # Update GDD
        gdd = gdd_for_day(d.t_avg_c, logic.tbase_c)
        state.cum_gdd += gdd

        # Check germination conditions window
        cond_ok = (
            logic.germ_temp_min <= d.t_avg_c <= logic.germ_temp_max and
            d.soil_moisture >= logic.germ_moisture_min
        )
        consec_ok = consec_ok + 1 if cond_ok else 0
        if germ_day is None and consec_ok >= logic.germ_min_consec_days:
            germ_day = d.day

        # Growth only after germination
        stress = stress_temp(d.t_avg_c, logic) * stress_moisture(d.soil_moisture)
        if germ_day is not None:
            # logistic Euler step with stress
            dB = logic.r * state.biomass * (1.0 - state.biomass / logic.k) * max(0.0, stress)
            state.biomass = max(0.0, state.biomass + dB)
        else:
            state.biomass = max(0.0, state.biomass * 0.999)  # negligible respiration

        state.height_cm = biomass_to_height(state.biomass, logic)
        stage_before = state.stage
        state.stage = next_stage(state.stage, state.biomass, germ_day is not None, logic)

        if mow_day is None and state.height_cm >= logic.mowable_height_cm:
            mow_day = d.day

        timeline.append({
            "day": d.day,
            "t_avg_c": round(d.t_avg_c, 2),
            "soil_moisture": round(d.soil_moisture, 3),
            "gdd_cum": round(state.cum_gdd, 2),
            "biomass": round(state.biomass, 2),
            "height_cm": round(state.height_cm, 2),
            "stage": state.stage,
            "stage_changed": stage_before != state.stage,
        })

    return {
        "timeline": timeline,
        "germ_day": germ_day,
        "mow_day": mow_day,
        "final_state": {
            "day": days[-1].day,
            "stage": state.stage,
            "biomass": round(state.biomass, 2),
            "height_cm": round(state.height_cm, 2),
            "cum_gdd": round(state.cum_gdd, 2),
        },
    }

# -----------------------------
# INDEPENDENT CHECKS
# -----------------------------

def check_invariants(result: Dict[str, Any], logic: Logic) -> Tuple[bool, List[str]]:
    ok = True
    notes: List[str] = []

    tl = result["timeline"]
    # 1) Biomass monotonic non-decreasing after germination (within numeric tolerance)
    if result["germ_day"] is not None:
        last = None
        for row in tl[result["germ_day"]:]:
            b = row["biomass"]
            if last is not None and b + 1e-6 < last:
                ok = False
                notes.append(f"Biomass decreased after germination: {b} < {last} on day {row['day']}")
                break
            last = b
    else:
        notes.append("No germination detected; monotonicity after germination not applicable.")

    # 2) Stage ordering never regresses
    order_index = {s: i for i, s in enumerate(STAGE_ORDER)}
    last_idx = 0
    for row in tl:
        idx = order_index[row["stage"]]
        if idx < last_idx:
            ok = False
            notes.append(f"Stage regressed on day {row['day']}: {row['stage']}")
            break
        last_idx = idx

    # 3) If a germ day is reported, verify the 3-day window condition actually holds in the raw data
    if result["germ_day"] is not None:
        gday = result["germ_day"]
        # recompute from timeline fields (which include temperature & moisture)
        # ensure days gday-2, gday-1, gday satisfy germination conditions
        window_ok = True
        for j in range(gday - 2, gday + 1):
            if j < 0 or j >= len(tl):
                window_ok = False
                break
            t = tl[j]["t_avg_c"]
            m = tl[j]["soil_moisture"]
            if not (logic.germ_temp_min <= t <= logic.germ_temp_max and m >= logic.germ_moisture_min):
                window_ok = False
        if not window_ok:
            ok = False
            notes.append("Reported germination day is inconsistent with its 3-day condition window.")

    # 4) Alternative height model cross-check using cumulative GDD
    # h_alt = h_max * (1 - exp(-beta * GDD)) with beta chosen so that at ~500 GDD height ~ 7 cm
    # Solve beta from: 7 = h_max * (1 - exp(-beta * 500))
    target_mow_gdd = 500.0
    beta = -math.log(1.0 - (logic.mowable_height_cm / logic.h_max_cm)) / target_mow_gdd
    mow_alt: int | None = None
    for row in tl:
        h_alt = logic.h_max_cm * (1.0 - math.exp(-beta * row["gdd_cum"]))
        if h_alt >= logic.mowable_height_cm:
            mow_alt = row["day"]
            break
    # The independently estimated mow day should be within ±2 days of the primary model
    if result["mow_day"] is None or mow_alt is None:
        notes.append("Unable to cross-verify mowable day (one of the estimates is missing).")
    else:
        if abs(mow_alt - result["mow_day"]) > 2:
            ok = False
            notes.append(
                f"Mowable day mismatch: primary={result['mow_day']} vs alt={mow_alt} (>2 days)."
            )
        else:
            notes.append(
                f"Mowable day cross-check agrees within tolerance: primary={result['mow_day']} alt={mow_alt}."
            )

    return ok, notes

# -----------------------------
# REASON CONSTRUCTION (EXPLANATION)
# -----------------------------

def build_reason(result: Dict[str, Any], logic: Logic) -> str:
    tl = result["timeline"]
    parts: List[str] = []

    gday = result["germ_day"]
    if gday is None:
        parts.append(
            "Germination did not occur because the required 3 consecutive days with "
            f"temperature in [{logic.germ_temp_min},{logic.germ_temp_max}]°C and soil moisture ≥ {logic.germ_moisture_min} were never met."
        )
    else:
        days_str = ", ".join(
            f"(day {j}: T={tl[j]['t_avg_c']}°C, M={tl[j]['soil_moisture']})" for j in range(gday-2, gday+1)
        )
        parts.append(
            f"Germination on day {gday} followed three qualifying days: {days_str}."
        )

    mday = result["mow_day"]
    if mday is None:
        parts.append(
            f"The plant never reached mowable height ({logic.mowable_height_cm} cm) within the simulation horizon."
        )
    else:
        parts.append(
            f"Mowable height {logic.mowable_height_cm} cm was first reached on day {mday}."
        )

    final = result["final_state"]
    parts.append(
        f"By day {final['day']}, the plant is in stage '{final['stage']}' with biomass {final['biomass']} g/m² "
        f"and height {final['height_cm']} cm."
    )

    return " " .join(parts)

# -----------------------------
# MAIN
# -----------------------------

def main() -> None:
    logic = Logic()
    env = generate_environment(60)
    sim = simulate(env, logic)

    # Answer
    answer: Dict[str, Any] = {
        "germination_day": sim["germ_day"],
        "mowable_day": sim["mow_day"],
        "stage_timeline": [
            {k: row[k] for k in ("day", "stage", "stage_changed", "height_cm", "biomass")}
            for row in sim["timeline"]
        ],
        "final_state": sim["final_state"],
    }

    # Reason Why
    reason: str = build_reason(sim, logic)

    # Independent Check
    ok, notes = check_invariants(sim, logic)
    check = {
        "ok": ok,
        "notes": notes,
    }

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

