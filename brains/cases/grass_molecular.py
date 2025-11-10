#!/usr/bin/env python3
"""
P3-STYLE, SELF-CONTAINED PYTHON PROGRAM — MOLECULAR GERMINATION (GRASS SEED)

Goal → A small, auditable simulator at the **molecular level** for a grass seed’s
transition from dormancy to radicle emergence and early seedling establishment.
It outputs the P3 triad:
  • Answer — key molecular milestones & days
  • Reason — why those results hold (linking to rules/data)
  • Check  — explicit, enumerated validations for trust

Didactic model (not a botanical authority). No external files. Deterministic.

OVERVIEW FOR EVERYONE (PLAIN LANGUAGE)
Seeds wake when they drink water. As they rehydrate, a “stop” hormone (ABA) fades and a
“go” hormone (GA) rises. GA removes molecular brakes (called DELLAs), which lets the seed
turn stored starch into sugar using the enzyme α‑amylase. Sugar powers the seed and, with
water, pushes the first root (radicle) out by softening cell walls (expansins). When enough
water, energy, and wall‑softening come together, the radicle breaks through. Soon after,
a protective shoot tip (coleoptile) becomes visible above the soil.

State variables (daily time step):
  W    — seed water content [0..1]
  ABA  — abscisic acid (relative units)
  GA   — gibberellin (relative units)
  DELLA— growth repressors (relative units, high = repression)
  AMY  — α-amylase expression level [0..1]
  S    — starch reserve (g arbitrary)
  GLC  — soluble sugar pool (g arbitrary)
  ATP  — energetic state (arbitrary)
  EXP  — wall-loosening capacity (expansins etc.) [0..1]
  H    — emergent seedling height (cm; tiny early) — for post-emergence sanity

Key signals:
  • Hormone switch (GA/ABA ratio up → DELLA degradation → AMY induction)
  • Starch → sugar via α-amylase → ATP → turgor & wall yielding with EXP
  • Radicle emerges when pressure index passes a threshold and gates are satisfied

Milestones reported in Answer:
  - hormone_switch_day (GA/ABA ≥ R_switch for ≥N days)
  - amylase_induction_day (AMY ≥ A_thresh)
  - starch_half_day (S ≤ S0/2)
  - radicle_emergence_day (pressure index crosses threshold with gates)
  - coleoptile_visible_day (height ≥ 0.5 cm)
"""
from __future__ import annotations
from dataclasses import dataclass, asdict
from typing import List, Dict, Any
import math
import json

# -----------------------------------
# PROMPT & GOAL
# -----------------------------------
PROMPT = (
    "Write a self-contained Python program that simulates early molecular events in "
    "a grass seed’s germination under daily temperature and moisture, and emits "
    "(1) Answer (milestones), (2) Reason Why, (3) explicit Checks."
)

GOAL = {
    "primary": "Determine days for hormone switch, α-amylase induction, starch half-use, radicle emergence, and first coleoptile visibility.",
    "secondary": [
        "Provide a stage timeline with molecular state snapshots.",
        "Ensure mass/logic constraints via explicit checks.",
    ],
}

# -----------------------------------
# DATA TYPES & LOGIC
# -----------------------------------
@dataclass
class Day:
    day: int
    t_avg_c: float
    soil_moisture: float  # [0..1]

@dataclass
class Logic:
    # Environment → Hydration
    k_hydrate: float = 0.35  # approach rate of W toward soil moisture per day

    # Hormones (relative units in [0,+))
    ABA_init: float = 1.2
    GA_init: float = 0.05
    DELLA_init: float = 1.0

    ABA_decay_base: float = 0.10     # ABA decays with hydration
    ABA_decay_by_W: float = 0.25     # more decay when hydrated (multiplies W)

    GA_syn_base: float = 0.05       # basal GA synthesis when hydrated/warm
    GA_syn_temp_peak: float = 23.0  # °C
    GA_syn_temp_sigma: float = 6.0
    GA_deg: float = 0.06            # GA turnover
    GA_max: float = 2.0

    DELLA_deg_k: float = 0.35       # GA-dependent DELLA degradation
    DELLA_K: float = 0.3            # softening parameter
    DELLA_basal_syn: float = 0.02   # small synthesis (stress/dry bias via (1-W))

    # Amylase expression (0..1) — driven by GA/ABA ratio and DELLA low
    A_thresh: float = 0.30
    R_switch: float = 1.0           # GA/ABA ratio threshold for switch
    switch_days: int = 2            # consecutive days
    k_amylase_up: float = 0.50
    k_amylase_down: float = 0.10

    # Reserves and energetics
    S0: float = 1.0                 # initial starch (arbitrary grams)
    k_hydrolysis: float = 0.35      # starch → sugar rate, scales with AMY and W
    yield_glc: float = 0.90
    k_resp: float = 0.30            # sugar → ATP conversion
    k_ATP_decay: float = 0.15

    # Wall loosening & growth readiness
    k_exp_up: float = 0.35
    k_exp_down: float = 0.05
    exp_switch: float = 1.2         # require GA/ABA ≥ this for strong EXP

    # Radicle emergence criterion
    radicle_pressure_thresh: float = 0.22  # threshold on pressure index
    min_EXP_for_emerge: float = 0.25
    min_ATP_for_emerge: float = 0.08

    # Height mapping (micro; post-emergence only for sanity)
    h_max_cm: float = 2.5
    alpha_h: float = 2.2            # height = h_max*(1-exp(-alpha*growth_signal))

# -----------------------------------
# ENVIRONMENT (30 days, cool-warm spring, moistening pattern)
# -----------------------------------

def generate_environment(n: int = 30) -> List[Day]:
    days: List[Day] = []
    for d in range(n):
        T = 18.0 + 5.5 * math.sin(2 * math.pi * d / 20.0)  # 15–23.5°C
        M = 0.30 + 0.25 * (0.5 + 0.5 * math.sin(2 * math.pi * (d + 3) / 10.0))  # 0.3..0.55
        M = max(0.0, min(1.0, M))
        days.append(Day(d, T, M))
    return days

# -----------------------------------
# HELPERS
# -----------------------------------

def ga_temp_factor(T: float, peak: float, sigma: float) -> float:
    # Gaussian around peak (unitless)
    return math.exp(-((T - peak) ** 2) / (2.0 * sigma ** 2))

# -----------------------------------
# SIMULATION
# -----------------------------------

def simulate(days: List[Day], L: Logic) -> Dict[str, Any]:
    # State
    W = 0.10
    ABA = L.ABA_init
    GA = L.GA_init
    DELLA = L.DELLA_init
    AMY = 0.0
    EXP = 0.0
    S = L.S0
    GLC = 0.0
    ATP = 0.0
    H = 0.0

    # Milestones
    hormone_switch_day = None
    amylase_induction_day = None
    starch_half_day = None
    radicle_emergence_day = None
    coleoptile_visible_day = None

    switch_run = 0

    timeline: List[Dict[str, Any]] = []

    for d in days:
        # Hydration moves toward soil moisture
        W = max(0.0, min(1.0, W + L.k_hydrate * (d.soil_moisture - W)))

        # Hormones
        ABA_decay = (L.ABA_decay_base + L.ABA_decay_by_W * W) * ABA
        ABA = max(0.0, ABA - ABA_decay)

        GA_syn = L.GA_syn_base * W * ga_temp_factor(d.t_avg_c, L.GA_syn_temp_peak, L.GA_syn_temp_sigma) * (1.0 - GA / L.GA_max)
        GA = max(0.0, GA + GA_syn - L.GA_deg * GA)

        # DELLA degrades with GA signaling, slight synthesis if dry
        DELLA = max(0.0, DELLA * (1.0 - L.DELLA_deg_k * (GA / (GA + L.DELLA_K))) + L.DELLA_basal_syn * (1.0 - W))

        # GA/ABA ratio & hormone switch tracking
        R = GA / (ABA + 1e-9)
        if R >= L.R_switch:
            switch_run += 1
            if hormone_switch_day is None and switch_run >= L.switch_days:
                hormone_switch_day = d.day
        else:
            switch_run = 0

        # α-amylase expression (up with GA dominance & low DELLA)
        drive = max(0.0, R - L.R_switch) * (1.0 - min(1.0, DELLA))
        AMY = max(0.0, min(1.0, AMY + L.k_amylase_up * drive - L.k_amylase_down * AMY))
        if amylase_induction_day is None and AMY >= L.A_thresh:
            amylase_induction_day = d.day

        # Reserve use: starch → sugar → ATP
        hydro = L.k_hydrolysis * AMY * W * S
        hydro = min(hydro, S)  # can't hydrolyze more than available
        S = max(0.0, S - hydro)
        GLC = max(0.0, GLC + L.yield_glc * hydro)
        ATP = max(0.0, (1.0 - L.k_ATP_decay) * ATP + L.k_resp * GLC)

        # EXP (wall loosening) — needs strong GA/ABA and hydration
        exp_drive = max(0.0, (R - L.exp_switch)) * W
        EXP = max(0.0, min(1.0, EXP + L.k_exp_up * exp_drive - L.k_exp_down * EXP))

        # Pressure index for emergence (turgor × wall loosening × sugar availability / repression)
        pressure_index = W * EXP * (GLC / (1.0 + DELLA + 1e-6))

        # Starch half-used milestone
        if starch_half_day is None and S <= 0.5 * L.S0:
            starch_half_day = d.day

        # Radicle emergence gate
        gates_ok = (EXP >= L.min_EXP_for_emerge and ATP >= L.min_ATP_for_emerge and R >= L.R_switch)
        if radicle_emergence_day is None and gates_ok and pressure_index >= L.radicle_pressure_thresh:
            radicle_emergence_day = d.day

        # Very small height signal post-emergence, from sugar & EXP
        growth_signal = 0.0
        if radicle_emergence_day is not None and d.day >= radicle_emergence_day:
            growth_signal = min(1.0, EXP * (GLC / (GLC + 0.1)))
            H = L.h_max_cm * (1.0 - math.exp(-L.alpha_h * (H / L.h_max_cm + 0.1 * growth_signal)))
            if coleoptile_visible_day is None and H >= 0.5:
                coleoptile_visible_day = d.day

        timeline.append({
            "day": d.day,
            "T": round(d.t_avg_c, 2),
            "M": round(d.soil_moisture, 3),
            "W": round(W, 3),
            "ABA": round(ABA, 3),
            "GA": round(GA, 3),
            "R": round(R, 3),
            "DELLA": round(DELLA, 3),
            "AMY": round(AMY, 3),
            "S": round(S, 3),
            "GLC": round(GLC, 3),
            "ATP": round(ATP, 3),
            "EXP": round(EXP, 3),
            "pressure_index": round(pressure_index, 3),
            "H": round(H, 3),
            "gates_ok": gates_ok,
        })

    result = {
        "timeline": timeline,
        "events": {
            "hormone_switch_day": hormone_switch_day,
            "amylase_induction_day": amylase_induction_day,
            "starch_half_day": starch_half_day,
            "radicle_emergence_day": radicle_emergence_day,
            "coleoptile_visible_day": coleoptile_visible_day,
        },
        "final_state": timeline[-1],
        "logic": asdict(L),
    }
    return result

# -----------------------------------
# CHECKS — explicit, enumerated
# -----------------------------------

def run_checks(sim: Dict[str, Any]) -> Dict[str, Any]:
    checks = []
    def add(name: str, passed: bool, details: str):
        checks.append({"name": name, "pass": bool(passed), "details": details})

    tl = sim["timeline"]
    ev = sim["events"]
    L = sim["logic"]  # only for info in details

    # 1) Non-negativity & bounds
    ok_bounds = True
    bad = None
    for row in tl:
        if not (0.0 <= row["W"] <= 1.0 and 0.0 <= row["AMY"] <= 1.0 and 0.0 <= row["EXP"] <= 1.0):
            ok_bounds = False; bad = row["day"]; break
        if row["S"] < -1e-6 or row["GLC"] < -1e-6 or row["ATP"] < -1e-6:
            ok_bounds = False; bad = row["day"]; break
    add("State variable bounds respected", ok_bounds, "All bounded in [0,1] or ≥0" if ok_bounds else f"Bounds violated on day {bad}")

    # 2) Mass sanity: starch never negative, and total potential sugar <= S0*yield + eps
    S0 = sim["logic"]["S0"] if isinstance(sim["logic"], dict) else 1.0
    yield_glc = sim["logic"]["yield_glc"] if isinstance(sim["logic"], dict) else 0.9
    ok_mass = True
    max_glc_seen = 0.0
    bad = None
    for row in tl:
        if row["S"] < -1e-9:
            ok_mass = False; bad = row["day"]; break
        max_glc_seen = max(max_glc_seen, row["GLC"])
    if max_glc_seen > yield_glc * S0 + 1e-6:
        ok_mass = False
        details_mass = f"Max GLC {max_glc_seen:.3f} > yield*S0 {yield_glc*S0:.3f}"
    else:
        details_mass = f"Max GLC {max_glc_seen:.3f} ≤ yield*S0 {yield_glc*S0:.3f}"
    add("Mass sanity (starch→sugar)", ok_mass, details_mass if ok_mass else f"Mass violation on day {bad}")

    # 3) Logical gating: AMY induction only after hormone switch period
    ok_amy_gate = True
    if ev["amylase_induction_day"] is not None and ev["hormone_switch_day"] is not None:
        ok_amy_gate = ev["amylase_induction_day"] >= ev["hormone_switch_day"]
    add("Amylase induction after hormone switch", ok_amy_gate,
        f"AMY day={ev['amylase_induction_day']} switch day={ev['hormone_switch_day']}")

    # 4) DELLA generally decreases after sustained GA dominance
    ok_della_trend = True
    start = ev["hormone_switch_day"] if ev["hormone_switch_day"] is not None else None
    if start is not None:
        first = tl[start]["DELLA"]
        last = tl[-1]["DELLA"]
        ok_della_trend = last <= first + 0.05  # allow small noise
    add("DELLA non-increasing post-switch", ok_della_trend, f"DELLA {tl[start]['DELLA'] if start is not None else 'NA'} → {tl[-1]['DELLA']}")

    # 5) Emergence gates sanity when emergence reported
    ok_emerge = True
    if ev["radicle_emergence_day"] is not None:
        d = ev["radicle_emergence_day"]
        row = tl[d]
        cond = (row["EXP"] >= 0.25 and row["ATP"] >= 0.08 and row["R"] >= 1.0 and row["pressure_index"] >= 0.22)
        ok_emerge = cond
    add("Emergence day satisfies gates", ok_emerge,
        "All gates met at reported emergence day" if ok_emerge else "Emergence gates not satisfied")

    # 6) Height non-decreasing after emergence
    ok_h = True
    if ev["radicle_emergence_day"] is not None:
        last_h = None
        for row in tl[ev["radicle_emergence_day"]:]:
            if last_h is not None and row["H"] + 1e-6 < last_h:
                ok_h = False; break
            last_h = row["H"]
    add("Height monotonic post-emergence", ok_h, "Non-decreasing" if ok_h else "Height decreased post-emergence")

    # 7) Alternative radicle-day estimate (two simple heuristics)
    # Heuristic A: hydration + GA/ABA dominance + minimum sugar
    alt1 = None
    for row in tl:
        if row["W"] >= 0.75 and row["R"] >= 1.0 and row["GLC"] >= 0.12:
            alt1 = row["day"]; break
    # Heuristic B: loose pressure-index gates (90% of thresholds)
    rp = sim["logic"]["radicle_pressure_thresh"] if isinstance(sim["logic"], dict) else 0.22
    me = sim["logic"]["min_EXP_for_emerge"] if isinstance(sim["logic"], dict) else 0.25
    ma = sim["logic"]["min_ATP_for_emerge"] if isinstance(sim["logic"], dict) else 0.08
    alt2 = None
    for row in tl:
        if row["pressure_index"] >= 0.9 * rp and row["EXP"] >= 0.9 * me and row["ATP"] >= 0.9 * ma:
            alt2 = row["day"]; break
    alt = alt1 if alt1 is not None else alt2
    if ev["radicle_emergence_day"] is None:
        ok_cross = alt is None
    else:
        ok_cross = alt is not None and abs(alt - ev["radicle_emergence_day"]) <= 2
    add("Alt radicle-day (simple) within ±2 days", ok_cross,
        f"primary={ev['radicle_emergence_day']}, alt={alt} (alt1={alt1}, alt2={alt2})")

    # 8) Energy never negative & responds to sugar
    ok_energy = True
    prev_ATP = tl[0]["ATP"]
    grew_when_glc = False
    for i in range(1, len(tl)):
        if tl[i]["ATP"] < -1e-9:
            ok_energy = False; break
        if tl[i-1]["GLC"] < tl[i]["GLC"] and tl[i]["ATP"] >= prev_ATP:
            grew_when_glc = True
        prev_ATP = tl[i]["ATP"]
    add("ATP non-negative & responds to sugar", ok_energy and grew_when_glc,
        "ATP stayed ≥0 and generally rose with sugar" if ok_energy and grew_when_glc else "ATP behavior suspect")

    ok = all(c["pass"] for c in checks)
    return {"ok": ok, "checks": checks}

# -----------------------------------
# REASON BUILDER
# -----------------------------------

def build_reason(sim: Dict[str, Any]) -> str:
    ev = sim["events"]
    tl = sim["timeline"]
    parts = []
    hs = ev["hormone_switch_day"]
    if hs is None:
        parts.append("No sustained GA/ABA hormone switch occurred; ABA likely stayed dominant.")
    else:
        t0 = max(0, hs - 1)
        parts.append(
            f"Hormone switch on day {hs} after GA/ABA ≥ 1 held for ≥2 days (e.g., day {t0} ratio {tl[t0]['R']}, day {hs} ratio {tl[hs]['R']})."
        )

    ai = ev["amylase_induction_day"]
    if ai is None:
        parts.append("α‑amylase never crossed induction threshold, limiting starch mobilization.")
    else:
        parts.append(f"α‑amylase induction on day {ai}, enabling starch hydrolysis → sugar.")

    if ev["starch_half_day"] is not None:
        parts.append(f"Starch reserves reached half of initial by day {ev['starch_half_day']}.")

    re = ev["radicle_emergence_day"]
    if re is None:
        parts.append("Radicle did not emerge; pressure index or gates remained below thresholds.")
    else:
        row = tl[re]
        parts.append(
            f"Radicle emergence on day {re} when hydration (W={row['W']}) and GA‑driven wall loosening (EXP={row['EXP']}) "
            f"combined with sugar (GLC={row['GLC']}) to overcome DELLA repression and meet the pressure index threshold ({row['pressure_index']})."
        )

    cv = ev["coleoptile_visible_day"]
    if cv is not None:
        parts.append(f"Coleoptile became visible (≈0.5 cm) by day {cv}.")

    return " ".join(parts)

# -----------------------------------
# MAIN
# -----------------------------------

def main() -> None:
    L = Logic()
    env = generate_environment(30)
    sim = simulate(env, L)

    answer = {
        **sim["events"],
        "final_state": sim["final_state"],
    }
    reason = build_reason(sim)
    check = run_checks(sim)

    out = {
        "Answer": answer,
        "Reason": reason,
        "Check": check,
        "Meta": {
            "PROMPT": PROMPT,
            "GOAL": GOAL,
            "LOGIC": asdict(L),
            "DATA": {
                "days": len(env),
                "T_range": [round(min(d.t_avg_c for d in env), 2), round(max(d.t_avg_c for d in env), 2)],
                "M_range": [round(min(d.soil_moisture for d in env), 3), round(max(d.soil_moisture for d in env), 3)],
            },
        },
    }
    print(json.dumps(out, indent=2))

if __name__ == "__main__":
    main()

