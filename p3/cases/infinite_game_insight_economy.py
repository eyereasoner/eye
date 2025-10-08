#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 CASE: “Inside the Insight Economy” — Triad-Only, (In)finite-Minded Labels
============================================================================

WHAT THIS FILE DOES (plain English)
-----------------------------------
This program turns the ideas behind “the insight economy” into a small testable model.
Instead of trading raw data, good actors trade *insights*: short answers that are
computed where the data lives. That tends to be more respectful (less copying),
more lawful (minimize personal data), and more useful (value is in the answer,
not in hoarding the data).

We evaluate scenarios on five elements (0..1 each):
  1) Derivation (technology)   – Do you compute the insight *near the data*?
  2) Minimization (legal)      – Do you reduce personal data and limit purpose?
  3) Activation (business)     – Is value created from the *insight* (not raw data)?
  4) Equilibrium (balance)     – Are tech/legal/business balanced, not lopsided?
  5) Copy-Resistance (economics) – Does the insight *lose value if copied*?
                                  (i.e., it’s useful in use, not as a transferable asset)

OUTPUT FORMAT (P3 triad)
------------------------
The program prints exactly three sections:
  • Answer     — {scenario: "infinite-minded" | "finite-minded"}
  • Reason Why — short, human-readable narratives per scenario
  • Check      — internal proof: invariants + unit tests (18 checks total)

WHY “(IN)FINITE-MINDED” HERE?
-----------------------------
Borrowing Sinek’s language:
  • “Infinite-minded” scenarios *play the long game*: they generate value while
    protecting people and relationships (compute near data, minimize, align incentives).
  • “Finite-minded” scenarios try to *win now* by copying/hoarding data and stretching
    purpose, which erodes trust and long-term adaptability.

HOW SCORING WORKS (high-level)
------------------------------
Each element gets a score from traits (0..1 baselines) and tagged decisions:
  • helpful tags (e.g., “on_site_derivation”, “zero_copy”, “purpose_limited”)
  • harmful tags (e.g., “raw_export”, “warehouse_merge”, “broad_consent”)

Classification rule (clear and simple):
  → “infinite-minded” iff
       average of the five elements ≥ 0.60,
       at least 3 elements ≥ 0.60,
       and red-flag pressure < 0.25 (each red-flag bucket adds 0.15).
  Otherwise: “finite-minded”.

USAGE
-----
$ python3 insight_economy_p3_triad.py
Exit code: 0 if all checks pass; 1 otherwise.

NOTE
----
This file prints *only* the triad sections. The explanations you need live in these comments.
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
# Scoring helpers (readable, not math-heavy)
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
# Proof: invariants (12) & unit tests (6) = 18 checks
# -----------------------------

def invariant_checks(results: List[Dict]) -> List[str]:
    issues = []
    for r in results:
        # I1: average equals mean(pillars)
        expected = sum(r["pillars"].values()) / 5
        if abs(r["average"] - expected) > 1e-9:
            issues.append(f"{r['name']}: I1 average mismatch")

        # I2: pillar bounds
        for k, v in r["pillars"].items():
            if not (0.0 - 1e-9 <= v <= 1.0 + 1e-9):
                issues.append(f"{r['name']}: I2 pillar '{k}' out of [0,1]")

        # I3: strong_count matches definition
        strong_count = sum(1 for v in r["pillars"].values() if v >= THRESH_STRONG)
        if strong_count != r["strong"]:
            issues.append(f"{r['name']}: I3 strong_count mismatch")

        # I4: if infinite-minded, rule preconditions must hold
        if r["classification"] == "infinite-minded":
            if not (r["average"] >= THRESH_AVG and r["strong"] >= 3 and r["penalty"] < 0.25):
                issues.append(f"{r['name']}: I4 infinite-minded rule violated")

        # I5: if penalty high, must be finite-minded
        if r["penalty"] >= 0.25 and r["classification"] != "finite-minded":
            issues.append(f"{r['name']}: I5 penalty high but not finite-minded")

        # I6: canonical wording present in narrative
        if r["classification"] == "infinite-minded":
            if "Classification: INFINITE-MINDED." not in r["reason_text"]:
                issues.append(f"{r['name']}: I6 missing canonical infinite label")
        else:
            if "Classification: FINITE-MINDED." not in r["reason_text"]:
                issues.append(f"{r['name']}: I6 missing canonical finite label")

        # I7: reasons_map keys match pillars
        if set(r["reasons_map"].keys()) != set(r["pillars"].keys()):
            issues.append(f"{r['name']}: I7 reasons keys mismatch pillars")

        # I8: average between min and max pillars
        vals = list(r["pillars"].values())
        if not (min(vals) - 1e-9 <= r["average"] <= max(vals) + 1e-9):
            issues.append(f"{r['name']}: I8 average not between min/max pillars")

    # I9: determinism — re-evaluate from original DATA objects by name
    name_to_obj = {s.name: s for s in DATA}
    for r in results:
        src = name_to_obj.get(r["name"])
        if not src:
            issues.append(f"{r['name']}: I9 missing source for determinism")
            continue
        r2 = evaluate(src)
        if (abs(r2["average"] - r["average"]) > 1e-9 or
            r2["classification"] != r["classification"] or
            r2["strong"] != r["strong"]):
            issues.append(f"{r['name']}: I9 non-deterministic evaluation")

    # I10: zero flags + thresholds → must be infinite-minded
    for r in results:
        if not r["flags"] and r["average"] >= THRESH_AVG and r["strong"] >= 3:
            if r["classification"] != "infinite-minded":
                issues.append(f"{r['name']}: I10 zero-flags & thresholds should be infinite-minded")

    # I11: all pillars ≥ 0.70 → must be infinite-minded
    for r in results:
        if all(v >= 0.70 for v in r["pillars"].values()):
            if r["classification"] != "infinite-minded":
                issues.append(f"{r['name']}: I11 all-strong pillars should be infinite-minded")

    # I12: if any decision exports raw data, Copy-Resistance should be narratively flagged
    for r in results:
        if any(tag in ["raw_export"] for p in DATA if p.name == r["name"] for d in p.decisions for tag in d.tags):
            if "Raw data exports undermine copy-resistance." not in r["reason_text"]:
                issues.append(f"{r['name']}: I12 raw-export present but narrative lacks flag")

    return issues

def unit_tests() -> List[str]:
    """
    Unit tests U1..U6 focusing on threshold edges and flags.
    """
    problems = []

    # U1: High across the board → infinite-minded
    high = Scenario(
        name="All-High-Insight",
        context="Synthetic: strong trinity + copy-resistance.",
        traits={"tech_maturity": 0.9, "legal_maturity": 0.9, "biz_maturity": 0.9, "copy_resistance": 0.9},
        decisions=[Decision(2025, "All the good patterns", ["on_site_derivation","zero_copy","data_minimized","purpose_limited","insight_contract","pay_per_insight","rate_limited","expiry"])]
    )
    r = evaluate(high)
    if r["classification"] != "infinite-minded":
        problems.append("U1: All-High-Insight should be infinite-minded")

    # U2: Low + raw exports/profiling → finite-minded
    low = Scenario(
        name="All-Low-DataLake",
        context="Synthetic: raw exports & profiling.",
        traits={"tech_maturity": 0.2, "legal_maturity": 0.2, "biz_maturity": 0.2, "copy_resistance": 0.2},
        decisions=[Decision(2025, "Everything wrong", ["raw_export","warehouse_merge","broad_consent","profiling_no_min","data_ownership_claims"])]
    )
    r2 = evaluate(low)
    if r2["classification"] != "finite-minded":
        problems.append("U2: All-Low-DataLake should be finite-minded")

    # U3: Avg just below threshold, strong pillars ≥3, no flags → still finite (avg binds)
    near = Scenario(
        name="Near-But-Avg-Below",
        context="Synthetic: misses by average.",
        traits={"tech_maturity": 0.60, "legal_maturity": 0.60, "biz_maturity": 0.60, "copy_resistance": 0.58},
        decisions=[Decision(2025, "Mostly good", ["on_site_derivation","data_minimized","insight_contract"])]
    )
    r3 = evaluate(near)
    if r3["average"] >= THRESH_AVG:
        problems.append("U3: Setup error (avg not below threshold)")
    if r3["classification"] != "finite-minded":
        problems.append("U3: Near-But-Avg-Below should be finite-minded")

    # U4: Avg above threshold but flags raise penalty ≥ 0.25 → finite
    flagged = Scenario(
        name="Avg-High-But-Flagged",
        context="Synthetic: good scores but blatant hoarding flags.",
        traits={"tech_maturity": 0.75, "legal_maturity": 0.75, "biz_maturity": 0.75, "copy_resistance": 0.75},
        decisions=[Decision(2025, "Good+Bad", ["on_site_derivation","data_minimized","insight_contract","raw_export","warehouse_merge"])]
    )
    r4 = evaluate(flagged)
    if not (r4["average"] >= THRESH_AVG and r4["penalty"] >= 0.25 and r4["classification"] == "finite-minded"):
        problems.append("U4: Avg-High-But-Flagged should be finite-minded due to penalty")

    # U5: Equilibrium matters — two high pillars, one very low → equilibrium drags classification
    imbalanced = Scenario(
        name="Imbalanced-Trinity",
        context="Synthetic: tech & legal high, business weak.",
        traits={"tech_maturity": 0.9, "legal_maturity": 0.9, "biz_maturity": 0.35, "copy_resistance": 0.7},
        decisions=[Decision(2025, "Good tech/legal, weak activation", ["on_site_derivation","data_minimized"])]
    )
    r5 = evaluate(imbalanced)
    if r5["average"] >= THRESH_AVG and r5["strong"] >= 3 and r5["classification"] == "infinite-minded":
        problems.append("U5: Imbalanced trinity should struggle due to low equilibrium")

    # U6: Finite-sounding wording but strong trinity, no flags → still infinite
    phrasing = Scenario(
        name="Finite-Sounding-But-OK",
        context="Synthetic: wording mentions quarter, but patterns are sound.",
        traits={"tech_maturity": 0.7, "legal_maturity": 0.7, "biz_maturity": 0.7, "copy_resistance": 0.7},
        decisions=[Decision(2025, "All good patterns", ["on_site_derivation","data_minimized","insight_contract","rate_limited","expiry"])]
    )
    r6 = evaluate(phrasing)
    if r6["classification"] != "infinite-minded":
        problems.append("U6: Finite wording alone should not block if trinity holds")

    return problems

# -----------------------------
# Runner: prints ONLY the triad
# -----------------------------

def main():
    results = [evaluate(s) for s in DATA]

    issues = invariant_checks(results)  # I1..I12
    tests = unit_tests()               # U1..U6
    ok = (not issues) and (not tests)

    triad = {
        "Answer": {r["name"]: r["classification"] for r in results},
        "Reason Why": {r["name"]: r["reason_text"] for r in results},
        "Check": {"ok": ok, "invariant_issues": issues, "unit_test_issues": tests}
    }

    print("Answer")
    print("------")
    print(json.dumps(triad["Answer"], indent=2, ensure_ascii=False))
    print("\nReason Why")
    print("----------")
    print(json.dumps(triad["Reason Why"], indent=2, ensure_ascii=False))
    print("\nCheck")
    print("-----")
    print(json.dumps(triad["Check"], indent=2, ensure_ascii=False))

    sys.exit(0 if ok else 1)

if __name__ == "__main__":
    main()

