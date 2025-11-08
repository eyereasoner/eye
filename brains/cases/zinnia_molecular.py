#!/usr/bin/env python3
"""
P3-STYLE, SELF-CONTAINED PYTHON PROGRAM — MOLECULAR GROWTH & BLOOM (ZINNIA)

OVERVIEW FOR EVERYONE (PLAIN LANGUAGE)
Zinnias are summer annuals that love warmth. Inside the seedling, a tug-of-war between
hormones sets the pace: ABA is the "stop" signal when things are dry; GA (gibberellin) and
IAA (auxin) are "go" signals for growth. As the plant drinks and warms, ABA fades while GA
and IAA rise. GA removes molecular brakes (called DELLAs). With the brakes off, enzymes
(such as α‑amylase) turn stored starch into sugars that power the plant. Other proteins
(expansins) loosen cell walls so tissues can enlarge. As energy and growth hormones build,
the shoot tip commits to making a flower (a developmental switch), a bud becomes visible,
pigments accumulate, and petals open — the first bloom.

This program simulates those events at a simplified molecular level, day by day, and then
proves its own result with explicit checks.

It outputs the P3 triad:
  • Answer — key molecular/phenology milestones & final state
  • Reason — plain-language explanation tied to the rules
  • Check  — explicit, enumerated validations (pass/fail with details)

WHAT YOU GET
- A deterministic simulator with a clear Prompt → Program → Proof structure.
- An easy-to-read JSON output with named milestones and a checklist of validations.
- Comments throughout the code explaining what each step does and why.

HOW TO RUN
1) Save as `zinnia_molecular.py`
2) Run: `python3 zinnia_molecular.py`
3) Read the JSON for Answer / Reason / Check.

TWEAK IT
- Change season length or warmth in `generate_environment()`.
- Adjust biological gates/thresholds in the `Logic` dataclass.
- Tighten/loosen validations (tolerances) in `run_checks()`.

Didactic model (not a botanical authority). No external files. Deterministic.
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
    "Write a self-contained Python program that simulates molecular events in a zinnia "
    "from early growth to first bloom under daily temperature and moisture, and emits "
    "(1) Answer (milestones), (2) Reason Why, (3) explicit Checks."
)

GOAL = {
    "primary": "Determine days for hormone switch, α-amylase induction, floral commitment, bud visibility, pigment onset, and first bloom.",
    "secondary": [
        "Provide a timeline with molecular snapshots.",
        "Ensure logic and mass constraints via explicit checks.",
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
    daylength_h: float

@dataclass
class Logic:
    # Environment → Hydration
    k_hydrate: float = 0.35

    # Hormones (relative units)
    ABA_init: float = 1.0
    GA_init: float = 0.08
    IAA_init: float = 0.15
    DELLA_init: float = 1.0

    ABA_decay_base: float = 0.10
    ABA_decay_by_W: float = 0.22

    GA_syn_base: float = 0.06
    GA_syn_temp_peak: float = 27.0
    GA_syn_temp_sigma: float = 7.0
    GA_deg: float = 0.06
    GA_max: float = 2.0

    IAA_syn_base: float = 0.05
    IAA_syn_temp_peak: float = 26.0
    IAA_syn_temp_sigma: float = 8.0
    IAA_deg: float = 0.05
    IAA_max: float = 1.8

    DELLA_deg_k: float = 0.32
    DELLA_K: float = 0.30
    DELLA_basal_syn: float = 0.02

    # Amylase expression (0..1)
    A_thresh: float = 0.28
    R_switch: float = 1.0            # GA/ABA ≥ 1 for switch
    switch_days: int = 2
    k_amylase_up: float = 0.50
    k_amylase_down: float = 0.10

    # Reserves & energetics
    S0: float = 1.0
    k_hydrolysis: float = 0.32
    yield_glc: float = 0.90
    k_resp: float = 0.28
    k_ATP_decay: float = 0.14

    # Wall loosening & growth readiness
    k_exp_up: float = 0.34
    k_exp_down: float = 0.06
    exp_switch_ratio: float = 1.1    # GA/ABA threshold for strong EXP

    # Floral commitment (FS, 0..1)
    k_fs_up: float = 0.25
    k_fs_down: float = 0.04
    fs_ratio_thresh: float = 1.2     # GA/ABA dominance target
    fs_aux_thresh: float = 0.45      # IAA level target
    fs_daylength_thresh: float = 13.2
    fs_commit_thresh: float = 0.35
    fs_commit_days: int = 3

    # Pigment accumulation (PIG, 0..1)
    k_pig_up: float = 0.20
    k_pig_down: float = 0.03
    pig_thresh: float = 0.30

    # Bud visibility & bloom thresholds
    bud_pressure_thresh: float = 0.20
    min_EXP_for_bud: float = 0.22
    min_ATP_for_bud: float = 0.08

    bloom_open_thresh: float = 0.18  # opening index threshold

    # Height mapping (cm, monotone)
    h_max_cm: float = 90.0
    alpha_h: float = 0.010

# -----------------------------------
# ENVIRONMENT (warm season, 100 days)
# -----------------------------------

def generate_environment(n: int = 100) -> List[Day]:
    days: List[Day] = []
    for d in range(n):
        T = 24.0 + 5.0 * math.sin(2 * math.pi * d / 30.0)
        M = 0.35 + 0.18 * math.sin(2 * math.pi * (d + 3) / 12.0)
        M = max(0.0, min(1.0, M))
        DL = 14.0 + 0.6 * math.sin(2 * math.pi * (d + 165) / 365.0)
        days.append(Day(d, T, M, DL))
    return days

# -----------------------------------
# HELPERS
# -----------------------------------

def gauss(x: float, mu: float, sigma: float) -> float:
    return math.exp(-((x - mu) ** 2) / (2.0 * sigma ** 2))

# -----------------------------------
# SIMULATION
# -----------------------------------
# STATE VARIABLES (daily)
#  W     — tissue water content [0..1]
#  ABA   — abscisic acid (stop signal)
#  GA    — gibberellin (growth-promoting)
#  IAA   — auxin (supports growth/opening)
#  DELLA — growth repressors (higher = more braking)
#  AMY   — α‑amylase (starch→sugar enzyme)
#  EXP   — wall-loosening capacity [0..1] (expansins proxy)
#  S     — starch reserves (arbitrary g)
#  GLC   — soluble sugars (arbitrary g)
#  ATP   — energy proxy
#  FS    — floral switch readiness [0..1]
#  PIG   — pigment accumulation [0..1]
#  H     — plant height (cm; simple sanity mapping)
# DERIVED INDICES
#  bud_pressure — readiness to expand tissues into a visible bud
#  OI (opening index) — petal-opening readiness; MU integrates OI over time for maturation

def simulate(days: List[Day], L: Logic) -> Dict[str, Any]:
    # State
    W = 0.12
    ABA = L.ABA_init
    GA = L.GA_init
    IAA = L.IAA_init
    DELLA = L.DELLA_init
    AMY = 0.0
    EXP = 0.0
    S = L.S0
    GLC = 0.0
    ATP = 0.0
    FS = 0.0
    PIG = 0.0
    H = 0.0

    # Milestones
    hormone_switch_day = None
    amylase_induction_day = None
    starch_half_day = None
    floral_commit_day = None
    bud_visible_day = None
    pigment_onset_day = None
    first_bloom_day = None

    switch_run = 0
    fs_run = 0

    # For cross-checks
    MU = 0.0  # maturation units (accumulate opening drive)

    timeline: List[Dict[str, Any]] = []

    for d in days:
        # Hydration
        W = max(0.0, min(1.0, W + L.k_hydrate * (d.soil_moisture - W)))

        # Hormones
        ABA_decay = (L.ABA_decay_base + L.ABA_decay_by_W * W) * ABA
        ABA = max(0.0, ABA - ABA_decay)

        GA_syn = L.GA_syn_base * W * gauss(d.t_avg_c, L.GA_syn_temp_peak, L.GA_syn_temp_sigma) * (1.0 - GA / L.GA_max)
        GA = max(0.0, GA + GA_syn - L.GA_deg * GA)

        IAA_syn = L.IAA_syn_base * W * gauss(d.t_avg_c, L.IAA_syn_temp_peak, L.IAA_syn_temp_sigma) * (1.0 - IAA / L.IAA_max)
        IAA = max(0.0, IAA + IAA_syn - L.IAA_deg * IAA)

        DELLA = max(0.0, DELLA * (1.0 - L.DELLA_deg_k * (GA / (GA + L.DELLA_K))) + L.DELLA_basal_syn * (1.0 - W))

        # Ratios & switches
        Rga = GA / (ABA + 1e-9)
        if Rga >= L.R_switch:
            switch_run += 1
            if hormone_switch_day is None and switch_run >= L.switch_days:
                hormone_switch_day = d.day
        else:
            switch_run = 0

        # Amylase
        drive_amy = max(0.0, Rga - L.R_switch) * (1.0 - min(1.0, DELLA))
        AMY = max(0.0, min(1.0, AMY + L.k_amylase_up * drive_amy - L.k_amylase_down * AMY))
        if amylase_induction_day is None and AMY >= L.A_thresh:
            amylase_induction_day = d.day

        # Reserves & energy
        hydro = L.k_hydrolysis * AMY * W * S
        hydro = min(hydro, S)
        S = max(0.0, S - hydro)
        GLC = max(0.0, GLC + L.yield_glc * hydro)
        ATP = max(0.0, (1.0 - L.k_ATP_decay) * ATP + L.k_resp * GLC)

        # Wall loosening
        exp_drive = max(0.0, (Rga - L.exp_switch_ratio)) * W
        EXP = max(0.0, min(1.0, EXP + L.k_exp_up * exp_drive - L.k_exp_down * EXP))

        # Floral commitment (FS)
        fs_drive = 0.0
        if Rga >= L.fs_ratio_thresh and IAA >= L.fs_aux_thresh and d.daylength_h >= L.fs_daylength_thresh:
            fs_drive = (Rga - L.fs_ratio_thresh + 1e-6) * (IAA - L.fs_aux_thresh + 1e-6) * (d.daylength_h - L.fs_daylength_thresh + 1e-6)
        FS = max(0.0, min(1.0, FS + L.k_fs_up * fs_drive - L.k_fs_down * FS))

        if FS >= L.fs_commit_thresh:
            fs_run += 1
            if floral_commit_day is None and fs_run >= L.fs_commit_days:
                floral_commit_day = d.day
        else:
            fs_run = 0

        # Bud pressure index
        bud_pressure = W * EXP * (ATP / (1.0 + DELLA + 1e-6)) * max(0.2, FS)
        if bud_visible_day is None and floral_commit_day is not None:
            if bud_pressure >= L.bud_pressure_thresh and EXP >= L.min_EXP_for_bud and ATP >= L.min_ATP_for_bud:
                bud_visible_day = d.day

        # Pigment accumulation (proxy for petal coloration readiness)
        pig_drive = max(0.0, FS) * (GLC / (GLC + 0.2))
        PIG = max(0.0, min(1.0, PIG + L.k_pig_up * pig_drive - L.k_pig_down * PIG))
        if pigment_onset_day is None and PIG >= L.pig_thresh and bud_visible_day is not None:
            pigment_onset_day = d.day

        # Opening index (OI) for bloom
        aux_fac = IAA / (IAA + 0.5)
        atp_fac = ATP / (ATP + 0.3)
        pig_fac = PIG / (PIG + 0.3)
        OI = EXP * aux_fac * atp_fac * pig_fac
        MU += OI  # accumulate maturation units

        if first_bloom_day is None and bud_visible_day is not None and pigment_onset_day is not None and OI >= L.bloom_open_thresh:
            first_bloom_day = d.day

        # Height (post-bud growth)
        if bud_visible_day is not None and d.day >= bud_visible_day:
            growth_signal = min(1.0, EXP * (GLC / (GLC + 0.2)) * (IAA / (IAA + 0.5)))
            H = L.h_max_cm * (1.0 - math.exp(-L.alpha_h * (H / L.h_max_cm + 0.08 * growth_signal)))

        timeline.append({
            "day": d.day,
            "T": round(d.t_avg_c, 2),
            "M": round(d.soil_moisture, 3),
            "DL": round(d.daylength_h, 2),
            "W": round(W, 3),
            "ABA": round(ABA, 3),
            "GA": round(GA, 3),
            "IAA": round(IAA, 3),
            "Rga": round(Rga, 3),
            "DELLA": round(DELLA, 3),
            "AMY": round(AMY, 3),
            "S": round(S, 3),
            "GLC": round(GLC, 3),
            "ATP": round(ATP, 3),
            "EXP": round(EXP, 3),
            "FS": round(FS, 3),
            "PIG": round(PIG, 3),
            "bud_pressure": round(bud_pressure, 3),
            "OI": round(OI, 3),
            "MU": round(MU, 3),
            "H": round(H, 2),
        })

    result = {
        "timeline": timeline,
        "events": {
            "hormone_switch_day": hormone_switch_day,
            "amylase_induction_day": amylase_induction_day,
            "starch_half_day": starch_half_day,
            "floral_commit_day": floral_commit_day,
            "bud_visible_day": bud_visible_day,
            "pigment_onset_day": pigment_onset_day,
            "first_bloom_day": first_bloom_day,
        },
        "final_state": timeline[-1],
        "logic": asdict(L),
    }
    return result

# -----------------------------------
# CHECKS — explicit, enumerated
# -----------------------------------

# CHECKLIST OVERVIEW (what you get)
#  1) State bounds (all key variables in valid ranges)
#  2) Mass sanity (starch→sugar upper bound)
#  3) Amylase after GA/ABA switch
#  4) DELLA broadly non-increasing after switch
#  5) Event order non-decreasing (switch → AMY → floral commit → bud → pigment → bloom)
#  6) Bud-day gates satisfied
#  7) Bloom-day opening index meets threshold
#  8) Height monotonic post-bud & never exceeds h_max
#  9) Alternative bloom-day (heuristics) within ±3 days of primary
# 10) Pigment onset on/after bud visibility

def run_checks(sim: Dict[str, Any]) -> Dict[str, Any]:
    checks = []
    def add(name: str, passed: bool, details: str):
        checks.append({"name": name, "pass": bool(passed), "details": details})

    tl = sim["timeline"]
    ev = sim["events"]
    L = sim["logic"]

    # 1) Bounds & non-negativity
    ok_bounds = True; bad = None
    for row in tl:
        if not (0.0 <= row["W"] <= 1.0 and 0.0 <= row["AMY"] <= 1.0 and 0.0 <= row["EXP"] <= 1.0 and 0.0 <= row["FS"] <= 1.0 and 0.0 <= row["PIG"] <= 1.0):
            ok_bounds = False; bad = row["day"]; break
        if row["S"] < -1e-6 or row["GLC"] < -1e-6 or row["ATP"] < -1e-6:
            ok_bounds = False; bad = row["day"]; break
    add("State variable bounds respected", ok_bounds, "All bounded in [0,1] or ≥0" if ok_bounds else f"Bounds violated on day {bad}")

    # 2) Mass sanity (starch→sugar upper bound)
    S0 = L["S0"] if isinstance(L, dict) else 1.0
    yield_glc = L["yield_glc"] if isinstance(L, dict) else 0.9
    max_glc_seen = max(row["GLC"] for row in tl)
    ok_mass = max_glc_seen <= yield_glc * S0 + 1e-6
    add("Mass sanity (starch→sugar)", ok_mass, f"Max GLC {max_glc_seen:.3f} ≤ yield*S0 {yield_glc*S0:.3f}" if ok_mass else f"Max GLC {max_glc_seen:.3f} > yield*S0 {yield_glc*S0:.3f}")

    # 3) Amylase after hormone switch
    ok_amy_gate = True
    if ev["amylase_induction_day"] is not None and ev["hormone_switch_day"] is not None:
        ok_amy_gate = ev["amylase_induction_day"] >= ev["hormone_switch_day"]
    add("Amylase induction after GA/ABA switch", ok_amy_gate, f"AMY={ev['amylase_induction_day']}, switch={ev['hormone_switch_day']}")

    # 4) DELLA trend (post-switch non-increasing broadly)
    ok_della = True
    start = ev["hormone_switch_day"] if ev["hormone_switch_day"] is not None else None
    if start is not None:
        ok_della = tl[-1]["DELLA"] <= tl[start]["DELLA"] + 0.05
    add("DELLA non-increasing post-switch", ok_della, f"DELLA start={tl[start]['DELLA'] if start is not None else 'NA'} → end={tl[-1]['DELLA']}")

    # 5) Ordering of events
    ok_order = True; detail_order = []
    seq = [
        ("hormone_switch_day", ev["hormone_switch_day"]),
        ("amylase_induction_day", ev["amylase_induction_day"]),
        ("floral_commit_day", ev["floral_commit_day"]),
        ("bud_visible_day", ev["bud_visible_day"]),
        ("pigment_onset_day", ev["pigment_onset_day"]),
        ("first_bloom_day", ev["first_bloom_day"]),
    ]
    prev = -1
    for name, val in seq:
        if val is None: continue
        if val < prev: ok_order = False
        prev = val
        detail_order.append(f"{name}={val}")
    add("Event order non-decreasing", ok_order, ", ".join(detail_order) if detail_order else "no events")

    # 6) Bud-day gates & bloom threshold satisfied
    ok_bud = True
    if ev["bud_visible_day"] is not None:
        d = ev["bud_visible_day"]; row = tl[d]
        ok_bud = (row["bud_pressure"] >= L["bud_pressure_thresh"] - 1e-6 and row["EXP"] >= L["min_EXP_for_bud"] - 1e-6 and row["ATP"] >= L["min_ATP_for_bud"] - 1e-6)
    add("Bud-day gates satisfied", ok_bud, f"bud_pressure={tl[ev['bud_visible_day']]['bud_pressure'] if ev['bud_visible_day'] is not None else 'NA'}")

    ok_bloom = True
    if ev["first_bloom_day"] is not None:
        d = ev["first_bloom_day"]; row = tl[d]
        ok_bloom = row["OI"] + 1e-6 >= L["bloom_open_thresh"]
    add("Bloom-day opening index meets threshold", ok_bloom, f"OI={tl[ev['first_bloom_day']]['OI'] if ev['first_bloom_day'] is not None else 'NA'} vs thresh {L['bloom_open_thresh']}")

    # 7) Height checks after bud
    ok_hmono = True; ok_hmax = True
    if ev["bud_visible_day"] is not None:
        last = None
        for row in tl[ev["bud_visible_day"]:]:
            if last is not None and row["H"] + 1e-6 < last:
                ok_hmono = False; break
            last = row["H"]
        ok_hmax = max(r["H"] for r in tl) <= L["h_max_cm"] + 1e-6
    add("Height monotonic post-bud", ok_hmono, "Non-decreasing" if ok_hmono else "Decreased at some point")
    add("Height never exceeds h_max", ok_hmax, f"h_max_cm={L['h_max_cm']}")

    # 8) Energy responds to sugar
    ok_energy = True; grew = False; prev_ATP = tl[0]["ATP"]
    for i in range(1, len(tl)):
        if tl[i]["ATP"] < -1e-9: ok_energy = False
        if tl[i]["GLC"] > tl[i-1]["GLC"] and tl[i]["ATP"] >= prev_ATP: grew = True
        prev_ATP = tl[i]["ATP"]
    add("ATP non-negative & generally rises with sugar", ok_energy and grew, "Behavior consistent" if (ok_energy and grew) else "ATP behavior suspect")

    # 9) Alternative bloom-day (heuristics) within ±3 days
    alt1 = None
    for row in tl:
        if row["FS"] >= L["fs_commit_thresh"] and row["PIG"] >= 0.8 * L["pig_thresh"] and row["OI"] >= 0.9 * L["bloom_open_thresh"]:
            alt1 = row["day"]; break
    # MU-based alternative (use maturation units)
    target_MU = max(8.0, 0.8 * sum(r["OI"] for r in tl))  # loose but present
    csum = 0.0; alt2 = None
    for row in tl:
        csum += row["OI"]
        if csum >= target_MU:
            alt2 = row["day"]; break
    alt = alt1 if alt1 is not None else alt2
    if ev["first_bloom_day"] is None:
        ok_alt = alt is None
    else:
        ok_alt = alt is not None and abs(alt - ev["first_bloom_day"]) <= 3
    add("Alt bloom-day (heuristics) within ±3 days", ok_alt, f"primary={ev['first_bloom_day']}, alt={alt} (alt1={alt1}, alt2={alt2})")

    # 10) Pigment onset occurs on/after bud visibility
    ok_pig = True
    if ev["pigment_onset_day"] is not None and ev["bud_visible_day"] is not None:
        ok_pig = ev["pigment_onset_day"] >= ev["bud_visible_day"]
    add("Pigment onset on/after bud visibility", ok_pig, f"pigment={ev['pigment_onset_day']}, bud={ev['bud_visible_day']}")

    ok = all(c["pass"] for c in checks)
    return {"ok": ok, "checks": checks}

# -----------------------------------
# REASON BUILDER
# -----------------------------------

def build_reason(sim: Dict[str, Any]) -> str:
    ev = sim["events"]; tl = sim["timeline"]
    parts = []
    hs = ev["hormone_switch_day"]
    if hs is None:
        parts.append("No sustained GA/ABA hormone switch occurred; ABA likely stayed dominant.")
    else:
        t0 = max(0, hs - 1)
        parts.append(
            f"Hormone switch on day {hs} after GA/ABA ≥ 1 held for ≥2 days (e.g., day {t0} ratio {tl[t0]['Rga']}, day {hs} ratio {tl[hs]['Rga']})."
        )

    ai = ev["amylase_induction_day"]
    if ai is None:
        parts.append("α‑amylase never crossed induction threshold, slowing sugar release from starch.")
    else:
        parts.append(f"α‑amylase induction on day {ai} enabled starch hydrolysis → sugars → ATP.")

    fc = ev["floral_commit_day"]
    if fc is not None:
        parts.append(f"Floral commitment by day {fc} as GA/ABA dominance, auxin, and long daylength aligned to drive the floral switch (FS).")

    bv = ev["bud_visible_day"]
    if bv is not None:
        row = tl[bv]
        parts.append(
            f"Bud became visible on day {bv} when hydration, wall loosening (EXP={row['EXP']}), and energy (ATP={row['ATP']}) produced enough pressure to expand tissues."
        )

    pg = ev["pigment_onset_day"]
    if pg is not None:
        parts.append(f"Pigment accumulation started by day {pg}, indicating petal maturation.")

    fb = ev["first_bloom_day"]
    if fb is not None:
        row = tl[fb]
        parts.append(
            f"First bloom on day {fb} when the opening index (OI={row['OI']}) crossed the threshold, combining EXP, auxin, energy, and pigment readiness."
        )

    return " ".join(parts) if parts else "No key events occurred under the simulated conditions."

# -----------------------------------
# MAIN
# -----------------------------------

def main() -> None:
    L = Logic()
    env = generate_environment(100)
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
                "DL_range": [round(min(d.daylength_h for d in env), 2), round(max(d.daylength_h for d in env), 2)],
            },
        },
    }
    print(json.dumps(out, indent=2))

if __name__ == "__main__":
    main()

