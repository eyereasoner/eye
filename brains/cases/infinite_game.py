#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 CASE: Simon Sinek's "The Infinite Game" — Triad-Only Harness
================================================================

WHAT THIS FILE IS
-----------------
A self-contained, deterministic Python program that implements the P3 pattern:
  • Prompt   → Classify organizations as infinite-minded vs finite-minded
               using Sinek’s five elements.
  • Program  → Transparent scoring rules (0..1 per pillar), plus a small
               rule-set to combine pillars into a final classification.
  • Proof    → A minimal harness that prints ONLY:
                 - "Answer"     : {org: classification}
                 - "Reason Why" : short human-friendly narratives
                 - "Check"      : invariants + unit tests (pass/fail)

WHY "TRIAD-ONLY" OUTPUT?
------------------------
To make the result easy to consume and validate in pipelines/CI or notebooks,
we print three clear sections with JSON content and no extra noise.

MAPPING FROM SINEK TO CODE
--------------------------
We score each organization across the five elements:
  1) Just Cause
  2) Trusting Teams
  3) Worthy Rivals
  4) Existential Flexibility
  5) Courage to Lead

Signals come from two places:
  • traits (baseline 0..1): trust, rival_learning, pivot_readiness, courage
  • decisions (tagged): e.g., "long_term", "invest_in_people",
    "learn_from_rival", "existential_flex", "courage_under_pressure",
    or red-flag tags like "fear_based", "cut_invest_people", "win_oriented", "copycat"

SCORING OVERVIEW (TRANSPARENT & MODERATE)
-----------------------------------------
Each pillar score ∈ [0,1] is a mix of:
  - a weighted baseline from traits
  - increments for supportive decision tags
  - decrements for red-flag tags (where applicable)

Example (Worthy Rivals):
  score = 0.5 * rival_learning
        + min(0.35, 0.22 * count(learn_from_rival/partner_with_rival))
        - min(0.35, 0.17 * count(win_oriented/scoreboard/copycat))

CLASSIFICATION RULE (TUNED BUT PRINCIPLED)
------------------------------------------
An org is "infinite-minded" if ALL of the following are true:
  • average of five pillars ≥ 0.60
  • at least 3 pillars are ≥ 0.60
  • red-flag pressure is LOW (we compute a small penalty; must stay < 0.25)

Why these thresholds?
  - Sinek emphasizes direction and resilience over perfection. Requiring
    ≥3/5 strong pillars + decent average rewards genuine infinite orientation
    while allowing some unevenness across pillars.
  - We still guard against "good-on-average but toxic-in-practice" cases with
    a red-flag check so fear/vanity-driven behaviors don’t slip through.

WHAT COUNTS AS A RED FLAG?
--------------------------
  • "Fear-based or extractive decisions." (e.g., cutting people to hit a quarter)
  • "Obsessed with beating rivals/optics." (e.g., win_oriented, scoreboard, copycat)
Each red-flag bucket contributes 0.15 penalty. We require total < 0.25.

DATA MODEL (SMALL & EDITABLE)
-----------------------------
- traits: dict of baseline scores 0..1
- decisions: year + description + tags[]
  Add/remove orgs, update tags, or tweak trait baselines to reflect reality.

ADAPTATION GUIDE (HOW TO TUNE SAFELY)
-------------------------------------
1) If your org is robustly infinite-minded but shows "finite" here, first
   check whether your decisions include supportive tags (e.g., "learn_from_rival")
   and whether fear/win-obsession tags are overused.
2) If you want stricter results, raise the average threshold to 0.65 and/or
   require ≥4 pillars ≥ 0.60 (see `infinite_minded` rule).
3) If your sector is pivot-heavy (startups), increase the weight of
   Existential Flexibility by bumping its baseline multiplier from 0.45→0.5.
4) Keep the red-flag guardrails. They’re the key to keeping this honest.
"""

from dataclasses import dataclass
from typing import List, Dict, Tuple
import statistics as stats
import json
import sys

# -----------------------------
# Data (includes clear infinite-minded examples)
# -----------------------------

@dataclass
class Decision:
    year: int
    description: str
    tags: List[str]  # e.g., ["invest_in_people", "long_term", "learn_from_rival"]

@dataclass
class Org:
    name: str
    just_cause: str
    decisions: List[Decision]
    traits: Dict[str, float]  # {"trust":0..1,"rival_learning":0..1,"pivot_readiness":0..1,"courage":0..1}

DATA: List[Org] = [
    Org(
        name="Evergreen Foods Cooperative",
        just_cause="Make healthy food universally accessible while sustaining local farmers for generations.",
        traits={"trust": 0.88, "rival_learning": 0.75, "pivot_readiness": 0.82, "courage": 0.86},
        decisions=[
            Decision(2018, "Created profit-sharing for growers; transparent pricing", ["customer_care", "long_term", "invest_in_people"]),
            Decision(2020, "Partnered with a former rival to expand food deserts program", ["learn_from_rival", "partner_with_rival"]),
            Decision(2022, "Paused a lucrative product over traceability concerns", ["courage_under_pressure", "short_term_penalty"]),
            Decision(2024, "Piloted zero-waste packaging model", ["existential_flex", "long_term"]),
        ],
    ),
    Org(
        name="Helios Energy Trust",
        just_cause="Accelerate the transition to clean, reliable energy in a way ordinary households can afford.",
        traits={"trust": 0.80, "rival_learning": 0.80, "pivot_readiness": 0.90, "courage": 0.78},
        decisions=[
            Decision(2019, "Open-sourced inverter diagnostics; invited competitor audits", ["learn_from_rival", "customer_care"]),
            Decision(2021, "Shifted to community microgrids despite near-term loss", ["existential_flex", "short_term_penalty"]),
            Decision(2023, "Kept safety buffers during price spikes; rejected exploitative surcharges", ["courage_under_pressure", "long_term"]),
        ],
    ),
    Org(
        name="Phoenix Labs",
        just_cause="Make learning tools that help anyone master STEM.",
        traits={"trust": 0.70, "rival_learning": 0.60, "pivot_readiness": 0.90, "courage": 0.60},
        decisions=[
            Decision(2019, "Partnered with former rival to integrate accessibility tech", ["learn_from_rival", "customer_care"]),
            Decision(2021, "Pivoted from B2C to B2B to sustain mission", ["existential_flex", "short_term_penalty"]),
            Decision(2022, "Protected research time during downturn", ["invest_in_people", "long_term"]),
            Decision(2025, "Refused data sale conflicting with student privacy", ["courage_under_pressure"]),
        ],
    ),
    # Finite-minded examples
    Org(
        name="QuarterlyMax Inc.",
        just_cause="Maximize shareholder returns this quarter.",
        traits={"trust": 0.45, "rival_learning": 0.20, "pivot_readiness": 0.30, "courage": 0.35},
        decisions=[
            Decision(2020, "Cut R&D and training to hit Q4 target", ["quarterly_target", "cut_invest_people"]),
            Decision(2021, "Aggressive '#1 vs competitor' campaign", ["win_oriented", "scoreboard"]),
            Decision(2022, "Laid off teams despite record profits", ["fear_based", "short_term_gain"]),
            Decision(2024, "Copied rival feature just for ads", ["copycat", "win_oriented"]),
        ],
    ),
    Org(
        name="FlashTrend Media",
        just_cause="Be the most viewed channel this month.",
        traits={"trust": 0.40, "rival_learning": 0.35, "pivot_readiness": 0.35, "courage": 0.40},
        decisions=[
            Decision(2023, "Clickbait pivots; staff churn tolerated", ["fear_based", "scoreboard"]),
            Decision(2024, "Shadow-banned dissenting creators on rumor", ["fear_based"]),
            Decision(2025, "Boasted beating rival daily; copied formats", ["win_oriented", "copycat"]),
        ],
    ),
]

# -----------------------------
# Scoring helpers
# -----------------------------

def clamp01(x: float) -> float:
    """Keep a float in [0,1] range — safety against accidental overshoot."""
    return max(0.0, min(1.0, x))

def score_just_cause(org: Org) -> Tuple[float, List[str]]:
    reasons = []
    score = 0.0
    if len(org.just_cause.strip()) >= 30:
        score += 0.35; reasons.append("Clear, service-oriented cause.")
    reinforcers = sum(1 for d in org.decisions if "cause_over_profit" in d.tags or "long_term" in d.tags or "customer_care" in d.tags)
    score += min(0.45, reinforcers * 0.17)
    if reinforcers:
        reasons.append("Decisions reinforce the cause over short-term gains.")
    if "this quarter" in org.just_cause.lower() or "this month" in org.just_cause.lower():
        score *= 0.4; reasons.append("Cause framed around near-term winning.")
    return clamp01(score), reasons

def score_trusting_teams(org: Org) -> Tuple[float, List[str], List[str]]:
    reasons, flags = [], []
    base = org.traits.get("trust", 0.5)
    score = 0.45 * base
    invest = sum(1 for d in org.decisions if "invest_in_people" in d.tags or "customer_care" in d.tags)
    score += min(0.4, invest * 0.18)
    if invest:
        reasons.append("Invests in people and customers.")
    fear = sum(1 for d in org.decisions if "fear_based" in d.tags or "cut_invest_people" in d.tags)
    if fear:
        score -= min(0.4, fear * 0.22)
        flags.append("Fear-based or extractive decisions.")
    return clamp01(score), reasons, flags

def score_worthy_rival(org: Org) -> Tuple[float, List[str], List[str]]:
    reasons, flags = [], []
    base = org.traits.get("rival_learning", 0.5)
    score = 0.5 * base
    learn = sum(1 for d in org.decisions if "learn_from_rival" in d.tags or "partner_with_rival" in d.tags)
    score += min(0.35, learn * 0.22)
    if learn:
        reasons.append("Learns from or partners with rivals.")
    win = sum(1 for d in org.decisions if "win_oriented" in d.tags or "scoreboard" in d.tags or "copycat" in d.tags)
    if win:
        score -= min(0.35, win * 0.17)
        flags.append("Obsessed with beating rivals/optics.")
    return clamp01(score), reasons, flags

def score_existential_flex(org: Org) -> Tuple[float, List[str]]:
    reasons = []
    base = org.traits.get("pivot_readiness", 0.5)
    score = 0.45 * base
    pivots = sum(1 for d in org.decisions if "existential_flex" in d.tags or "short_term_penalty" in d.tags)
    score += min(0.45, pivots * 0.22)
    if pivots:
        reasons.append("Willing to pivot/accept short-term pain for mission.")
    return clamp01(score), reasons

def score_courage(org: Org) -> Tuple[float, List[str]]:
    reasons = []
    base = org.traits.get("courage", 0.5)
    score = 0.5 * base
    stands = sum(1 for d in org.decisions if "courage_under_pressure" in d.tags)
    score += min(0.35, stands * 0.25)
    if stands:
        reasons.append("Upholds values under pressure.")
    return clamp01(score), reasons

# -----------------------------
# Aggregate + classification
# -----------------------------

def evaluate_org(org: Org) -> Dict:
    """
    Compute pillar scores, build a concise human narrative, and decide
    infinite-minded vs finite-minded using the tuned rule.
    """
    jc, jc_r = score_just_cause(org)
    tt, tt_r, tt_f = score_trusting_teams(org)
    wr, wr_r, wr_f = score_worthy_rival(org)
    ef, ef_r = score_existential_flex(org)
    cl, cl_r = score_courage(org)

    pillars = {
        "Just Cause": jc,
        "Trusting Teams": tt,
        "Worthy Rivals": wr,
        "Existential Flexibility": ef,
        "Courage to Lead": cl
    }
    reasons = {
        "Just Cause": jc_r,
        "Trusting Teams": tt_r,
        "Worthy Rivals": wr_r,
        "Existential Flexibility": ef_r,
        "Courage to Lead": cl_r
    }
    flags = list(dict.fromkeys(tt_f + wr_f))  # unique, order-preserving

    avg_score = stats.mean(pillars.values())
    strong_count = sum(1 for v in pillars.values() if v >= 0.60)

    # red-flag pressure
    red_flag_penalty = 0.0
    if "Fear-based or extractive decisions." in flags:
        red_flag_penalty += 0.15
    if "Obsessed with beating rivals/optics." in flags:
        red_flag_penalty += 0.15

    # Tuned but principled infinite-minded rule (see header discussion)
    infinite_minded = (avg_score >= 0.60 and strong_count >= 3 and red_flag_penalty < 0.25)
    classification = "infinite-minded" if infinite_minded else "finite-minded"

    def pct(x): return f"{round(100*x)}%"
    reason_text = (
        f"Avg {pct(avg_score)} with {strong_count}/5 pillars ≥60%. "
        f"Classification: {classification.upper()}."
    )
    # Add brief, human-readable signals (concise)
    brief = []
    for k, lst in reasons.items():
        if lst:
            brief.append(f"{k}: {', '.join(lst)}")
    if flags:
        brief.append("Red flags: " + "; ".join(flags))
    if brief:
        reason_text += " " + " ".join(brief)

    return {
        "name": org.name,
        "classification": classification,
        "pillars": pillars,
        "average": avg_score,
        "strong_count": strong_count,
        "reason_text": reason_text,
        "flags": flags,
    }

# -----------------------------
# Proof: invariants & unit tests
# -----------------------------

def invariant_checks(results: List[Dict]) -> List[str]:
    """
    Sanity checks that should always hold for any data set.
    """
    issues = []
    for r in results:
        avg = r["average"]
        expected = sum(r["pillars"].values()) / 5
        if abs(avg - expected) > 1e-9:
            issues.append(f"{r['name']}: average mismatch")
        # If average is very low and flags exist, it must be finite-minded
        if avg <= 0.45 and len(r["flags"]) >= 2 and r["classification"] != "finite-minded":
            issues.append(f"{r['name']}: should be finite-minded given low avg and multiple red flags")
    return issues

def unit_tests() -> List[str]:
    """
    Tiny, deterministic tests to catch oversights in the rule set.
    """
    problems = []
    # high pillars => should be infinite-minded
    high = Org(
        name="All-High",
        just_cause="Advance human flourishing for generations.",
        traits={"trust": 0.9, "rival_learning": 0.9, "pivot_readiness": 0.9, "courage": 0.9},
        decisions=[Decision(2020, "Partner with rival; pause harmful line", ["learn_from_rival", "existential_flex", "long_term", "courage_under_pressure"])]
    )
    r = evaluate_org(high)
    if r["classification"] != "infinite-minded":
        problems.append("All-High should classify as infinite-minded")

    # low pillars + flags => should be finite-minded
    low = Org(
        name="All-Low",
        just_cause="Be #1 this quarter.",
        traits={"trust": 0.2, "rival_learning": 0.2, "pivot_readiness": 0.2, "courage": 0.2},
        decisions=[Decision(2021, "Layoffs to meet target; chest-beating ads", ["fear_based", "quarterly_target", "win_oriented"])]
    )
    r2 = evaluate_org(low)
    if r2["classification"] != "finite-minded":
        problems.append("All-Low should classify as finite-minded")
    return problems

# -----------------------------
# Runner (prints ONLY the triad)
# -----------------------------

def main():
    results = [evaluate_org(org) for org in DATA]

    issues = invariant_checks(results)
    tests = unit_tests()
    ok = (not issues) and (not tests)

    triad = {
        "Answer": {r["name"]: r["classification"] for r in results},
        "Reason Why": {r["name"]: r["reason_text"] for r in results},
        "Check": {"ok": ok, "invariant_issues": issues, "unit_test_issues": tests}
    }

    # Print sections clearly; this is the ONLY output.
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

