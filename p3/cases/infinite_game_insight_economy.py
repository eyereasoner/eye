#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 CASE: “Inside the Insight Economy” — Triad-Only, (In)finite-Minded Labels
============================================================================

WHAT THIS FILE DOES
-------------------
This program turns the ideas behind “the insight economy” into a small testable model.
Instead of trading raw data, good actors trade *insights*: short answers that are
computed where the data lives. That tends to be more respectful (less copying),
more lawful (minimize personal data), and more useful (value is in the answer,
not in hoarding the data).

Explanation:
- Good actors don’t hoard raw data. They compute a small, specific *insight*
  where the data lives, share just that answer, and align tech/legal/business.
- We score 5 elements from 0..1 and classify each scenario as:
    • infinite-minded  — long-game, respectful, sustainable patterns
    • finite-minded    — short-term “data hoarding” patterns

Five elements:
1) Derivation (tech)         — compute insights *near the data* (zero-copy/federated)
2) Minimization (legal)      — reduce personal data, limit purpose, add safeguards
3) Activation (business)     — value from *using the insight*, not owning the data
4) Equilibrium (balance)     — tech/legal/business are balanced, not lopsided
5) Copy-Resistance (economy) — the insight *loses value when copied*; contracts/expiry help

Classification rule (simple, explicit):
- “infinite-minded” iff:
    avg ≥ 0.60, at least 3 pillars ≥ 0.60, and penalty < 0.25
  (each distinct red-flag bucket adds 0.15 to penalty)

This file prints ONLY three sections:
  • Answer      — easy-to-read scoreboard + JSON map
  • Reason Why  — concise narratives
  • Check       — a full PASS/FAIL harness (lists all checks)

Exit code: 0 if all checks pass, 1 otherwise.
"""

from dataclasses import dataclass
from typing import List, Dict, Tuple
import statistics as stats
import json, sys, math

# -----------------------------
# Data: five illustrative scenarios
# -----------------------------

@dataclass
class Decision:
    year: int
    description: str
    tags: List[str]

@dataclass
class Scenario:
    name: str
    context: str
    decisions: List[Decision]
    traits: Dict[str, float]  # {"tech_maturity":..,"legal_maturity":..,"biz_maturity":..,"copy_resistance":..}

# The first three are designed to come out as *infinite-minded* with the rules below.
DATA: List[Scenario] = [
    Scenario(
        name="Delfour ↔ Community Pharmacy (insight request)",
        context="Supermarket asks pharmacy for a fit-for-purpose insight about likely sugar-sensitive purchases; compute near data.",
        traits={"tech_maturity": 0.80, "legal_maturity": 0.78, "biz_maturity": 0.76, "copy_resistance": 0.85},
        decisions=[
            Decision(2025, "Derive prediction at pharmacy; send only a yes/no + confidence", ["on_site_derivation", "zero_copy", "data_minimized"]),
            Decision(2025, "Contract governs the insight (not data), revocable and auditable", ["insight_contract", "expiry", "auditability", "purpose_limited"]),
            Decision(2025, "Per-insight pricing with shared upside if conversion occurs", ["pay_per_insight", "mutual_value"]),
        ],
    ),
    Scenario(
        name="Mobility Pass ↔ City Parking (dynamic pricing via insights)",
        context="Driver keeps telemetry; parking operator requests a congestion/availability *insight* to set dynamic rates—no raw movement data leaves.",
        traits={"tech_maturity": 0.77, "legal_maturity": 0.80, "biz_maturity": 0.74, "copy_resistance": 0.82},
        decisions=[
            Decision(2024, "Federated compute of congestion index from user devices", ["federated_compute", "on_site_derivation", "zero_copy"]),
            Decision(2024, "Purpose-limited, legitimate-interest assessment for traffic management", ["purpose_limited", "legitimate_interest", "data_minimized"]),
            Decision(2025, "Pay-per-insight with rate limits & short-lived tokens", ["pay_per_insight", "rate_limited", "expiry"]),
        ],
    ),
    Scenario(
        name="Clinical Research Board ↔ EHR Network (trial feasibility)",
        context="Hospitals compute cohort counts locally; the broker receives only k-anonymized counts and eligibility flags.",
        traits={"tech_maturity": 0.81, "legal_maturity": 0.83, "biz_maturity": 0.70, "copy_resistance": 0.80},
        decisions=[
            Decision(2023, "k-anon cohort counts; no row-level exports", ["on_site_derivation", "zero_copy", "data_minimized"]),
            Decision(2023, "Ethics & legal review confirm minimization suffices", ["purpose_limited", "legitimate_interest"]),
            Decision(2024, "Per-site incentives tied to *use* of the insight, not data volume", ["mutual_value", "pay_per_insight"]),
        ],
    ),
    # Intended to be finite-minded
    Scenario(
        name="AdTech Broker (warehouse the web)",
        context="Collects raw browsing and app telemetry into a central lake; derives value mainly from copying & reselling profiles.",
        traits={"tech_maturity": 0.62, "legal_maturity": 0.25, "biz_maturity": 0.65, "copy_resistance": 0.20},
        decisions=[
            Decision(2022, "Raw event export from SDKs into central warehouse", ["raw_export", "warehouse_merge"]),
            Decision(2023, "Broad consents; profiling without minimization", ["broad_consent", "profiling_no_min"]),
            Decision(2025, "Claims 'ownership' of user data, resale to partners", ["data_ownership_claims"]),
        ],
    ),
    Scenario(
        name="Insurer Bulk EHR Import (risk scoring)",
        context="Seeks bulk EHR copies for risk models; aims to keep a permanent lake.",
        traits={"tech_maturity": 0.55, "legal_maturity": 0.35, "biz_maturity": 0.60, "copy_resistance": 0.30},
        decisions=[
            Decision(2024, "Requests CSV exports of medical records", ["raw_export"]),
            Decision(2024, "Broad, evergreen consents", ["broad_consent"]),
            Decision(2025, "Central model training without minimization", ["profiling_no_min", "warehouse_merge"]),
        ],
    ),
]

# -----------------------------
# Scoring helpers (clear + readable)
# -----------------------------

def clamp01(x: float) -> float:
    """Keep any computed score neatly between 0 and 1."""
    return max(0.0, min(1.0, x))

def score_derivation(s: Scenario) -> Tuple[float, List[str]]:
    """Technology: compute where the data lives (zero-copy/federated/on-site)."""
    reasons = []
    base = 0.6 * s.traits.get("tech_maturity", 0.5)
    boosts = sum(1 for d in s.decisions if "on_site_derivation" in d.tags or "federated_compute" in d.tags or "zero_copy" in d.tags)
    score = base + min(0.35, boosts * 0.18)
    if boosts:
        reasons.append("Derives insights near data (zero-copy/federated).")
    return clamp01(score), reasons

def score_minimization(s: Scenario) -> Tuple[float, List[str], List[str]]:
    """Legal: minimize personal data, limit purpose, add safeguards."""
    reasons, flags = [], []
    base = 0.6 * s.traits.get("legal_maturity", 0.5)
    good = sum(1 for d in s.decisions if "data_minimized" in d.tags or "purpose_limited" in d.tags or "legitimate_interest" in d.tags or "expiry" in d.tags or "auditability" in d.tags)
    score = base + min(0.35, good * 0.17)
    if good:
        reasons.append("Minimizes data with purpose limits & safeguards.")
    bad = sum(1 for d in s.decisions if "broad_consent" in d.tags or "profiling_no_min" in d.tags)
    if bad:
        score -= min(0.35, bad * 0.20)
        flags.append("Broad/profiling without minimization.")
    return clamp01(score), reasons, flags

def score_activation(s: Scenario) -> Tuple[float, List[str], List[str]]:
    """Business: value comes from *using* the insight, not owning raw data."""
    reasons, flags = [], []
    base = 0.6 * s.traits.get("biz_maturity", 0.5)
    pro = sum(1 for d in s.decisions if "insight_contract" in d.tags or "pay_per_insight" in d.tags or "mutual_value" in d.tags or "rate_limited" in d.tags)
    score = base + min(0.35, pro * 0.16)
    if pro:
        reasons.append("Value via insight contracts / pay-per-insight / mutual gain.")
    bad = sum(1 for d in s.decisions if "warehouse_merge" in d.tags or "data_ownership_claims" in d.tags)
    if bad:
        score -= min(0.35, bad * 0.18)
        flags.append("Data-hoarding business model.")
    return clamp01(score), reasons, flags

def score_equilibrium(der: float, minz: float, act: float) -> Tuple[float, List[str]]:
    """
    Balance across tech/legal/business: if one lags far behind, the stool tips over.
    We reward balance by penalizing variance; perfect balance ≈ 1.0, extreme imbalance ≈ 0.2.
    """
    vals = [der, minz, act]
    mu = sum(vals) / 3
    var = sum((v - mu) ** 2 for v in vals) / 3
    # Worst variance on [0,1] among triplets is (1,0,0) variants:
    max_var = ((1 - 1/3)**2 + (0 - 1/3)**2 + (0 - 1/3)**2) / 3
    balance = 1.0 - (var / max_var)
    score = clamp01(0.2 + 0.8 * balance)
    reasons = ["Balanced Trinity across tech/legal/business." if balance >= 0.7 else "Trinity slightly imbalanced."]
    return score, reasons

def score_copy_resistance(s: Scenario) -> Tuple[float, List[str], List[str]]:
    """
    Good insights are “wasting assets” when copied—copying them broadly kills their point.
    Contracts, expiry, and rate limits help; raw exports harm.
    """
    reasons, flags = [], []
    base = 0.6 * s.traits.get("copy_resistance", 0.5)
    supports = sum(1 for d in s.decisions if "rate_limited" in d.tags or "expiry" in d.tags or "insight_contract" in d.tags)
    score = base + min(0.35, supports * 0.16)
    if supports:
        reasons.append("Copy-resistant via contract/expiry/rate limits.")
    if any("raw_export" in d.tags for d in s.decisions):
        score *= 0.6
        flags.append("Raw data exports undermine copy-resistance.")
    return clamp01(score), reasons, flags

# -----------------------------
# Aggregate + classification
# -----------------------------

THRESH_AVG = 0.60
THRESH_STRONG = 0.60

def evaluate(s: Scenario) -> Dict:
    der, der_r = score_derivation(s)
    minz, minz_r, minz_f = score_minimization(s)
    act, act_r, act_f = score_activation(s)
    eq, eq_r = score_equilibrium(der, minz, act)
    cr, cr_r, cr_f = score_copy_resistance(s)

    pillars = {
        "Derivation": der,
        "Minimization": minz,
        "Activation": act,
        "Equilibrium": eq,
        "Copy-Resistance": cr,
    }
    reasons_map = {
        "Derivation": der_r,
        "Minimization": minz_r,
        "Activation": act_r,
        "Equilibrium": eq_r,
        "Copy-Resistance": cr_r,
    }
    flags = list(dict.fromkeys(minz_f + act_f + cr_f))

    avg = stats.mean(pillars.values())
    strong = sum(1 for v in pillars.values() if v >= THRESH_STRONG)

    # red-flag pressure (each bucket adds 0.15)
    penalty = 0.0
    if "Broad/profiling without minimization." in flags:
        penalty += 0.15
    if "Data-hoarding business model." in flags:
        penalty += 0.15
    if "Raw data exports undermine copy-resistance." in flags:
        penalty += 0.15

    infinite = (avg >= THRESH_AVG and strong >= 3 and penalty < 0.25)
    classification = "infinite-minded" if infinite else "finite-minded"

    def pct(x): return f"{round(100*x)}%"
    reason = f"Avg {pct(avg)} with {strong}/5 elements ≥{int(THRESH_STRONG*100)}%. Classification: {classification.upper()}."
    brief = []
    for k, lst in reasons_map.items():
        if lst:
            brief.append(f"{k}: {', '.join(lst)}")
    if flags:
        brief.append("Red flags: " + "; ".join(flags))
    if brief:
        reason += " " + " ".join(brief)

    return {
        "name": s.name,
        "classification": classification,
        "pillars": pillars,
        "average": avg,
        "strong": strong,
        "reason_text": reason,
        "flags": flags,
        "penalty": penalty,
        "context": s.context,
        "reasons_map": reasons_map,
    }

# -----------------------------
# Check catalog (names + descriptions)
# -----------------------------

INVARIANT_INFO = {
    "I1": "average equals mean of pillars",
    "I2": "each pillar within [0,1]",
    "I3": "strong_count matches pillars ≥ 0.60",
    "I4": "if infinite-minded then (avg/strong/penalty) preconditions hold",
    "I5": "if penalty ≥ 0.25 then must be finite-minded",
    "I6": "narrative includes canonical label",
    "I7": "reasons_map keys match pillar keys",
    "I8": "average between min and max pillar values",
    "I9": "deterministic: re-evaluation matches",
    "I10": "zero flags + thresholds ⇒ infinite-minded",
    "I11": "all pillars ≥ 0.70 ⇒ infinite-minded",
    "I12": "raw_export present ⇒ narrative includes raw-export flag",
    "I13": "thresholds met & penalty < 0.25 ⇒ infinite-minded (contrapositive)",
    "I14": "penalty equals 0.15 × distinct flag buckets",
    "I15": "very imbalanced Trinity ⇒ low Equilibrium",
    "I16": "near-equal Trinity ⇒ high Equilibrium",
}

UNIT_INFO = {
    "U1": "All-High-Insight ⇒ infinite-minded",
    "U2": "All-Low-DataLake ⇒ finite-minded",
    "U3": "Near-But-Avg-Below (avg below) ⇒ finite-minded",
    "U4": "Avg-High-But-Flagged (penalty ≥0.25) ⇒ finite-minded",
    "U5": "Imbalanced-Trinity doesn’t sneak through on average alone",
    "U6": "Finite wording alone doesn’t block if Trinity holds",
    "U7": "Monotonicity: adding supportive tag doesn’t reduce Derivation",
    "U8": "One flag (0.15) with high pillars can still be infinite-minded",
    "U9": "Two flags (0.30) force finite-minded",
    "U10": "Removing raw_export raises Copy-Resistance",
}

# -----------------------------
# Invariants (return detailed failures per code)
# -----------------------------

def run_invariants(results: List[Dict]) -> Dict[str, List[str]]:
    issues: Dict[str, List[str]] = {k: [] for k in INVARIANT_INFO}
    name_to_obj = {s.name: s for s in DATA}

    # I1..I8
    for r in results:
        # I1
        expected = sum(r["pillars"].values()) / 5
        if abs(r["average"] - expected) > 1e-9:
            issues["I1"].append(f"{r['name']}: average mismatch ({r['average']:.3f} vs {expected:.3f})")

        # I2
        for k, v in r["pillars"].items():
            if not (0.0 - 1e-9 <= v <= 1.0 + 1e-9):
                issues["I2"].append(f"{r['name']}: pillar '{k}'={v:.3f} out of [0,1]")

        # I3
        strong_count = sum(1 for v in r["pillars"].values() if v >= THRESH_STRONG)
        if strong_count != r["strong"]:
            issues["I3"].append(f"{r['name']}: strong_count {r['strong']} vs {strong_count}")

        # I4
        if r["classification"] == "infinite-minded":
            if not (r["average"] >= THRESH_AVG and r["strong"] >= 3 and r["penalty"] < 0.25):
                issues["I4"].append(f"{r['name']}: infinite-minded but rule not satisfied")

        # I5
        if r["penalty"] >= 0.25 and r["classification"] != "finite-minded":
            issues["I5"].append(f"{r['name']}: penalty {r['penalty']:.2f} but not finite-minded")

        # I6
        needed = "Classification: INFINITE-MINDED." if r["classification"] == "infinite-minded" else "Classification: FINITE-MINDED."
        if needed not in r["reason_text"]:
            issues["I6"].append(f"{r['name']}: missing canonical label in narrative")

        # I7
        if set(r["reasons_map"].keys()) != set(r["pillars"].keys()):
            issues["I7"].append(f"{r['name']}: reasons keys mismatch pillars")

        # I8
        vals = list(r["pillars"].values())
        if not (min(vals) - 1e-9 <= r["average"] <= max(vals) + 1e-9):
            issues["I8"].append(f"{r['name']}: average not between min/max pillars")

    # I9 determinism (re-evaluate from original object)
    for r in results:
        src = name_to_obj.get(r["name"])
        if not src:
            issues["I9"].append(f"{r['name']}: missing source for determinism")
            continue
        r2 = evaluate(src)
        if (abs(r2["average"] - r["average"]) > 1e-9 or
            r2["classification"] != r["classification"] or
            r2["strong"] != r["strong"]):
            issues["I9"].append(f"{r['name']}: non-deterministic evaluation")

    # I10 zero-flags + thresholds ⇒ infinite-minded
    for r in results:
        if not r["flags"] and r["average"] >= THRESH_AVG and r["strong"] >= 3:
            if r["classification"] != "infinite-minded":
                issues["I10"].append(f"{r['name']}: thresholds met w/o flags but not infinite-minded")

    # I11 all pillars strong ⇒ infinite-minded
    for r in results:
        if all(v >= 0.70 for v in r["pillars"].values()):
            if r["classification"] != "infinite-minded":
                issues["I11"].append(f"{r['name']}: all pillars strong but not infinite-minded")

    # I12 raw_export ⇒ narrative includes CR flag
    for r in results:
        src = name_to_obj.get(r["name"])
        if not src: 
            continue
        has_raw = any("raw_export" in d.tags for d in src.decisions)
        if has_raw and "Raw data exports undermine copy-resistance." not in r["reason_text"]:
            issues["I12"].append(f"{r['name']}: raw_export present but narrative lacks explicit flag")

    # I13 thresholds met + penalty<0.25 ⇒ infinite-minded
    for r in results:
        if r["average"] >= THRESH_AVG and r["strong"] >= 3 and r["penalty"] < 0.25:
            if r["classification"] != "infinite-minded":
                issues["I13"].append(f"{r['name']}: thresholds met but classification not infinite-minded")

    # I14 penalty = 0.15 × distinct flag buckets
    for r in results:
        expected_pen = 0.0
        if "Broad/profiling without minimization." in r["flags"]:
            expected_pen += 0.15
        if "Data-hoarding business model." in r["flags"]:
            expected_pen += 0.15
        if "Raw data exports undermine copy-resistance." in r["flags"]:
            expected_pen += 0.15
        if abs(expected_pen - r["penalty"]) > 1e-9:
            issues["I14"].append(f"{r['name']}: expected {expected_pen:.2f}, got {r['penalty']:.2f}")

    # I15 very imbalanced Trinity ⇒ low Equilibrium
    for r in results:
        d, m, a = r["pillars"]["Derivation"], r["pillars"]["Minimization"], r["pillars"]["Activation"]
        mu = (d + m + a) / 3
        std = math.sqrt(((mu - d)**2 + (mu - m)**2 + (mu - a)**2) / 3)
        if std >= 0.45 and not (r["pillars"]["Equilibrium"] <= 0.40 + 1e-9):
            issues["I15"].append(f"{r['name']}: high imbalance (std={std:.2f}) but Equilibrium={r['pillars']['Equilibrium']:.2f} not low")

    # I16 near-equal Trinity ⇒ high Equilibrium
    for r in results:
        d, m, a = r["pillars"]["Derivation"], r["pillars"]["Minimization"], r["pillars"]["Activation"]
        if max(d, m, a) - min(d, m, a) <= 0.05 and r["pillars"]["Equilibrium"] < 0.85 - 1e-9:
            issues["I16"].append(f"{r['name']}: near-equal Trinity but Equilibrium={r['pillars']['Equilibrium']:.2f} not high")

    return issues

# -----------------------------
# Unit tests (return dict code -> message or empty list)
# -----------------------------

def run_unit_tests() -> Dict[str, List[str]]:
    out: Dict[str, List[str]] = {k: [] for k in UNIT_INFO}

    # U1
    high = Scenario(
        name="All-High-Insight",
        context="Synthetic: strong trinity + copy-resistance.",
        traits={"tech_maturity": 0.9, "legal_maturity": 0.9, "biz_maturity": 0.9, "copy_resistance": 0.9},
        decisions=[Decision(2025, "All the good patterns", ["on_site_derivation","zero_copy","data_minimized","purpose_limited","insight_contract","pay_per_insight","rate_limited","expiry"])]
    )
    r = evaluate(high)
    if r["classification"] != "infinite-minded":
        out["U1"].append("Expected infinite-minded")

    # U2
    low = Scenario(
        name="All-Low-DataLake",
        context="Synthetic: raw exports & profiling.",
        traits={"tech_maturity": 0.2, "legal_maturity": 0.2, "biz_maturity": 0.2, "copy_resistance": 0.2},
        decisions=[Decision(2025, "Everything wrong", ["raw_export","warehouse_merge","broad_consent","profiling_no_min","data_ownership_claims"])]
    )
    r2 = evaluate(low)
    if r2["classification"] != "finite-minded":
        out["U2"].append("Expected finite-minded")

    # U3
    near = Scenario(
        name="Near-But-Avg-Below",
        context="Synthetic: misses by average.",
        traits={"tech_maturity": 0.55, "legal_maturity": 0.55, "biz_maturity": 0.55, "copy_resistance": 0.55},
        decisions=[Decision(2025, "Mostly good", ["on_site_derivation","data_minimized","insight_contract"])]
    )
    r3 = evaluate(near)
    if r3["average"] >= THRESH_AVG or r3["classification"] != "finite-minded":
        out["U3"].append("Should be finite-minded with avg below threshold")

    # U4
    flagged = Scenario(
        name="Avg-High-But-Flagged",
        context="Synthetic: good scores but blatant hoarding flags.",
        traits={"tech_maturity": 0.75, "legal_maturity": 0.75, "biz_maturity": 0.75, "copy_resistance": 0.75},
        decisions=[Decision(2025, "Good+Bad", ["on_site_derivation","data_minimized","insight_contract","raw_export","warehouse_merge"])]
    )
    r4 = evaluate(flagged)
    if not (r4["average"] >= THRESH_AVG and r4["penalty"] >= 0.25 and r4["classification"] == "finite-minded"):
        out["U4"].append("Penalty ≥0.25 should force finite-minded")

    # U5
    imbalanced = Scenario(
        name="Imbalanced-Trinity",
        context="Synthetic: tech & legal high, business weak.",
        traits={"tech_maturity": 0.9, "legal_maturity": 0.9, "biz_maturity": 0.35, "copy_resistance": 0.7},
        decisions=[Decision(2025, "Good tech/legal, weak activation", ["on_site_derivation","data_minimized"])]
    )
    r5 = evaluate(imbalanced)
    if r5["average"] >= THRESH_AVG and r5["strong"] >= 3 and r5["classification"] == "infinite-minded":
        out["U5"].append("Imbalance should prevent easy pass")

    # U6
    phrasing = Scenario(
        name="Finite-Sounding-But-OK",
        context="Synthetic: wording mentions quarter, but patterns are sound.",
        traits={"tech_maturity": 0.72, "legal_maturity": 0.72, "biz_maturity": 0.72, "copy_resistance": 0.72},
        decisions=[Decision(2025, "All good patterns", ["on_site_derivation","data_minimized","insight_contract","rate_limited","expiry"])]
    )
    r6 = evaluate(phrasing)
    if r6["classification"] != "infinite-minded":
        out["U6"].append("Finite wording alone should not block")

    # U7
    base = Scenario(
        name="Mono-Base",
        context="Derivation baseline",
        traits={"tech_maturity": 0.6, "legal_maturity": 0.6, "biz_maturity": 0.6, "copy_resistance": 0.6},
        decisions=[Decision(2025, "Zero-copy not yet", [])]
    )
    more = Scenario(
        name="Mono-More",
        context="Add on_site_derivation",
        traits=base.traits,
        decisions=[Decision(2025, "Now zero-copy on-site", ["on_site_derivation"])]
    )
    r7a, r7b = evaluate(base), evaluate(more)
    if r7b["pillars"]["Derivation"] < r7a["pillars"]["Derivation"] - 1e-9:
        out["U7"].append("Supportive tag should not reduce Derivation")

    # U8
    oneflag = Scenario(
        name="One-Flag-High",
        context="High scores + one raw_export",
        traits={"tech_maturity": 0.9, "legal_maturity": 0.9, "biz_maturity": 0.9, "copy_resistance": 0.9},
        decisions=[Decision(2025, "Great but one raw export", ["on_site_derivation","data_minimized","insight_contract","rate_limited","expiry","raw_export"])]
    )
    r8 = evaluate(oneflag)
    if not (abs(r8["penalty"] - 0.15) < 1e-9 and r8["classification"] == "infinite-minded"):
        out["U8"].append("One flag shouldn’t flip classification if avg/strong hold")

    # U9
    twoflags = Scenario(
        name="Two-Flags-High",
        context="High pillars, two hoarding flags",
        traits={"tech_maturity": 0.9, "legal_maturity": 0.9, "biz_maturity": 0.9, "copy_resistance": 0.9},
        decisions=[Decision(2025, "Good patterns plus two hoarding flags", ["on_site_derivation","data_minimized","insight_contract","rate_limited","expiry","raw_export","warehouse_merge"])]
    )
    r9 = evaluate(twoflags)
    if not (abs(r9["penalty"] - 0.30) < 1e-9 and r9["classification"] == "finite-minded"):
        out["U9"].append("Two flags should force finite-minded")

    # U10
    with_export = Scenario(
        name="CR-With-Export",
        context="Raw export present",
        traits={"tech_maturity": 0.7, "legal_maturity": 0.7, "biz_maturity": 0.7, "copy_resistance": 0.7},
        decisions=[Decision(2025, "Mixed", ["on_site_derivation","insight_contract","raw_export"])]
    )
    no_export = Scenario(
        name="CR-No-Export",
        context="Raw export removed",
        traits=with_export.traits,
        decisions=[Decision(2025, "Mixed", ["on_site_derivation","insight_contract"])]
    )
    r10a, r10b = evaluate(with_export), evaluate(no_export)
    if not (r10b["pillars"]["Copy-Resistance"] > r10a["pillars"]["Copy-Resistance"] - 1e-9):
        out["U10"].append("Removing raw_export should raise Copy-Resistance")

    return out

# -----------------------------
# Pretty printers
# -----------------------------

def yesno(b: bool) -> str:
    return "PASS" if b else "FAIL"

def print_answer(results: List[Dict]):
    # Human-friendly scoreboard
    print("Answer")
    print("------")
    print("SCOREBOARD — (higher avg & strong pillars = more infinite-minded)")
    print("——————————————————————————————————————————————————————————————")
    print(f"{'Org/Scenario':38} {'Avg':>5} {'Strong≥60%':>12} {'Penalty':>8}  {'Class':>15}")
    print("-"*85)
    for r in results:
        avg = f"{r['average']*100:4.0f}%"
        pen = f"{r['penalty']:.2f}"
        cls = r['classification']
        print(f"{r['name'][:36]:38} {avg:>5} {str(r['strong'])+'/5':>12} {pen:>8}  {cls:>15}")
    # Also provide the original map as JSON (useful for machines / diffs)
    print("\nJSON")
    print("---")
    print(json.dumps({r["name"]: r["classification"] for r in results}, indent=2, ensure_ascii=False))

def print_reasons(results: List[Dict]):
    print("\nReason Why")
    print("----------")
    for r in results:
        print(f"- {r['name']}:")
        print(f"  {r['reason_text']}")

def print_checks(inv_fail: Dict[str, List[str]], unit_fail: Dict[str, List[str]]):
    total_inv = len(INVARIANT_INFO)
    total_unit = len(UNIT_INFO)
    inv_pass = sum(1 for k in INVARIANT_INFO if not inv_fail.get(k))
    unit_pass = sum(1 for k in UNIT_INFO if not unit_fail.get(k))

    print("\nCheck")
    print("-----")
    print(f"Checks run: {total_inv} invariants + {total_unit} unit tests = {total_inv + total_unit} total")
    print("\nInvariants")
    print("----------")
    for code in sorted(INVARIANT_INFO.keys(), key=lambda x: int(x[1:])):
        passed = not inv_fail.get(code)
        print(f"[{yesno(passed)}] {code} — {INVARIANT_INFO[code]}")
        if not passed:
            for msg in inv_fail[code]:
                print(f"    • {msg}")

    print("\nUnit Tests")
    print("----------")
    for code in sorted(UNIT_INFO.keys(), key=lambda x: int(x[1:])):
        passed = not unit_fail.get(code)
        print(f"[{yesno(passed)}] {code} — {UNIT_INFO[code]}")
        if not passed:
            for msg in unit_fail[code]:
                print(f"    • {msg}")

    all_ok = (inv_pass == total_inv) and (unit_pass == total_unit)
    print("\nSummary")
    print("-------")
    print(f"Invariants: {inv_pass}/{total_inv} PASS")
    print(f"Unit tests: {unit_pass}/{total_unit} PASS")
    print(f"OVERALL: {'PASS' if all_ok else 'FAIL'}")

# -----------------------------
# Runner
# -----------------------------

def main():
    results = [evaluate(s) for s in DATA]

    inv_fail = run_invariants(results)   # dict: code -> list of failures
    unit_fail = run_unit_tests()         # dict: code -> list of failures

    print_answer(results)
    print_reasons(results)
    print_checks(inv_fail, unit_fail)

    all_ok = all(len(v) == 0 for v in inv_fail.values()) and all(len(v) == 0 for v in unit_fail.values())
    sys.exit(0 if all_ok else 1)

if __name__ == "__main__":
    main()

