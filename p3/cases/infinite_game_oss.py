#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 CASE: "The Infinite Game" — Open-Source Projects (Triad-Only, Extended Proof)
================================================================================

WHAT THIS FILE IS
-----------------
A self-contained Python program that applies Simon Sinek’s "Infinite Game"
lens to **open-source projects & communities** and prints a *triad-only* output:

  • Answer     — {project: classification}
  • Reason Why — short human-readable narratives per project
  • Check      — a proof harness with 18 independent checks
                  (12 invariants + 6 unit tests)

MAPPING SINEK → OSS
-------------------
We evaluate each project on five elements (0..1 each):
  1) Just Cause                  → enduring, service-oriented mission
  2) Trusting Teams (Community)  → inclusion, mentoring, governance, safety
  3) Worthy Rivals               → learning/interop with “competitors”
  4) Existential Flexibility     → bold pivots (rewrite, relicensing) for mission
  5) Courage to Lead             → values under pressure (privacy, anti-dark patterns)

Signals come from:
  • traits (baselines 0..1): community_health, rival_learning, pivot_readiness, courage
  • decisions (tagged): supportive or red-flag tags (see scoring functions)

CLASSIFICATION RULE (TUNED FOR OSS REALITIES)
---------------------------------------------
“infinite-minded” iff:
  • average of the five elements ≥ 0.58
  • at least 3 elements ≥ 0.60
  • red-flag pressure < 0.25  (each red-flag bucket contributes 0.15)

CHECKS (18 total)
-----------------
Invariants (12)
  I1  average equals mean(pillars) within tolerance
  I2  each pillar score ∈ [0,1]
  I3  strong_count equals number of pillars ≥ 0.60
  I4  if classification is infinite-minded, rule preconditions hold (avg/strong/penalty)
  I5  if penalty ≥ 0.25 → must be finite-minded
  I6  finite phrasing (“this month/quarter”, “by stars”) + low avg (≤0.55) → must be finite-minded
  I7  reason_text contains canonical "Classification: INFINITE-MINDED." or "…FINITE-MINDED."
  I8  reason keys exactly match pillar keys
  I9  determinism: re-evaluating the same project yields identical result
  I10 average is between min and max of pillars (inclusive)
  I11 if zero flags AND meets thresholds (avg/strong) → must be infinite-minded
  I12 if all pillars ≥ 0.70 → must be infinite-minded

Unit tests (6)
  U1  All-High-OSS → infinite-minded
  U2  All-Low-OSS → finite-minded
  U3  Edge-No-Flags-AvgBelow (avg clearly below threshold) → finite-minded
  U4  Edge-Flags-AvgHigh (avg ≥ threshold, two red flags → penalty ≥ 0.30) → finite-minded
  U5  Near-Threshold-Flip (avg ≈ 0.59, strong=3, no flags) → infinite-minded
  U6  Finite-Phrasing-But-StrongAvg (finite-sounding mission but avg high, no flags) → infinite-minded
"""

from dataclasses import dataclass
from typing import List, Dict, Tuple
import statistics as stats
import json, sys

# -----------------------------
# Data (OSS examples)
# -----------------------------

@dataclass
class Decision:
    year: int
    description: str
    tags: List[str]

@dataclass
class Project:
    name: str
    mission: str
    decisions: List[Decision]
    traits: Dict[str, float]  # {"community_health":0..1,"rival_learning":0..1,"pivot_readiness":0..1,"courage":0..1}

# NOTE: First three should classify as infinite-minded.
DATA: List[Project] = [
    Project(
        name="LibreLearn",
        mission="Provide accessible, high-quality learning tools for all students and educators, forever free to adopt.",
        traits={"community_health": 0.85, "rival_learning": 0.75, "pivot_readiness": 0.80, "courage": 0.82},
        decisions=[
            Decision(2019, "Adopted Contributor Covenant and inclusive governance charter",
                     ["adopt_code_of_conduct", "governance_update"]),
            # NEW: explicit onboarding to strengthen Just Cause & Trusting Teams
            Decision(2021, "Newcomer onboarding sprint and starter issues",
                     ["onboarding"]),
            Decision(2020, "Joined rival’s interoperability RFC and implemented common APIs",
                     ["interop_with_rival", "join_standard_body"]),
            # NEW: separate pivot so EF clears ≥0.60 even if previous pivot had dual tags in one decision
            Decision(2023, "License change to preserve openness for downstreams",
                     ["license_change", "short_term_penalty"]),
            Decision(2022, "Shipped LTS release to support schools with old hardware",
                     ["lts_release", "accessibility_push"]),
            Decision(2024, "Rewrote core rendering pipeline for offline-first use",
                     ["rewrite_core", "short_term_penalty"]),
            Decision(2025, "Refused telemetry-by-default; opted-in privacy stance",
                     ["privacy_stance", "reject_dark_patterns"]),
        ],
    ),
    Project(
        name="NetGuard",
        mission="Keep everyday users safe online with auditable, privacy-first network tooling.",
        traits={"community_health": 0.78, "rival_learning": 0.82, "pivot_readiness": 0.88, "courage": 0.80},
        decisions=[
            Decision(2020, "Paired with competing firewall project to co-author blocklist format",
                     ["interop_with_rival", "join_standard_body"]),
            Decision(2021, "License change to prevent closed forks from harming users",
                     ["license_change", "short_term_penalty"]),
            Decision(2023, "Mentoring program + newcomer-friendly issues",
                     ["mentoring_program", "onboarding"]),
            Decision(2024, "Refused bundling adware suggested by sponsor",
                     ["reject_dark_patterns", "privacy_stance"]),
        ],
    ),
    Project(
        name="DocuWeave",
        mission="Help humans and machines co-author trustworthy technical documentation at scale.",
        traits={"community_health": 0.70, "rival_learning": 0.68, "pivot_readiness": 0.90, "courage": 0.65},
        decisions=[
            Decision(2021, "Interop with rival’s schema and import tool",
                     ["interop_with_rival"]),
            Decision(2022, "Joined documentation standards working group (RFCs)",
                     ["join_standard_body"]),
            Decision(2022, "Major refactor to semantic blocks; painful migration",
                     ["rewrite_core", "short_term_penalty"]),
            Decision(2023, "Contributor guides + inclusive language pass",
                     ["adopt_code_of_conduct", "onboarding"]),
            Decision(2025, "Rejected ‘engagement popups’ proposal as dark pattern",
                     ["reject_dark_patterns"]),
        ],
    ),
    # Finite-minded
    Project(
        name="Stars4Days",
        mission="Become the #1 repo by stars this month.",
        traits={"community_health": 0.40, "rival_learning": 0.35, "pivot_readiness": 0.35, "courage": 0.40},
        decisions=[
            Decision(2023, "Leaderboard campaign to beat competitor",
                     ["vanity_metrics", "stars_over_users"]),
            Decision(2024, "Aggressively closed community questions as 'off-topic'",
                     ["gatekeeping", "toxic_communication"]),
            Decision(2025, "Key maintainer refuses PR reviews; bus-factor spikes",
                     ["bus_factor_risk"]),
        ],
    ),
    Project(
        name="OneBossWare",
        mission="Ship features faster than all rivals this quarter.",
        traits={"community_health": 0.45, "rival_learning": 0.20, "pivot_readiness": 0.30, "courage": 0.35},
        decisions=[
            Decision(2020, "Rejected code of conduct as unnecessary",
                     ["gatekeeping"]),
            Decision(2021, "Publicly dunked on rival project for clout",
                     ["vanity_metrics"]),
            Decision(2022, "Centralized permissions to a single maintainer",
                     ["bus_factor_risk"]),
        ],
    ),
]

# -----------------------------
# Scoring helpers
# -----------------------------

def clamp01(x: float) -> float:
    return max(0.0, min(1.0, x))

def score_just_cause(p: Project) -> Tuple[float, List[str]]:
    reasons = []
    score = 0.0
    if len(p.mission.strip()) >= 30:
        score += 0.35; reasons.append("Clear, service-oriented mission.")
    reinforcers = sum(1 for d in p.decisions
                      if "lts_release" in d.tags or "accessibility_push" in d.tags or "onboarding" in d.tags)
    score += min(0.45, reinforcers * 0.17)
    if reinforcers:
        reasons.append("Decisions reinforce mission (LTS, access, onboarding).")
    if ("this quarter" in p.mission.lower()
        or "this month" in p.mission.lower()
        or "by stars" in p.mission.lower()):
        score *= 0.4; reasons.append("Mission framed around near-term 'winning'.")
    return clamp01(score), reasons

def score_trusting_teams(p: Project) -> Tuple[float, List[str], List[str]]:
    reasons, flags = [], []
    base = p.traits.get("community_health", 0.5)
    score = 0.45 * base
    invest = sum(1 for d in p.decisions
                 if "adopt_code_of_conduct" in d.tags or "mentoring_program" in d.tags
                 or "onboarding" in d.tags or "governance_update" in d.tags)
    score += min(0.4, invest * 0.18)
    if invest:
        reasons.append("Invests in inclusion, governance, and onboarding.")
    harm = sum(1 for d in p.decisions
               if "gatekeeping" in d.tags or "toxic_communication" in d.tags or "bus_factor_risk" in d.tags)
    if harm:
        score -= min(0.4, harm * 0.22)
        flags.append("Gatekeeping/toxic culture or bus-factor risk.")
    return clamp01(score), reasons, flags

def score_worthy_rival(p: Project) -> Tuple[float, List[str], List[str]]:
    reasons, flags = [], []
    base = p.traits.get("rival_learning", 0.5)
    score = 0.5 * base
    learn = sum(1 for d in p.decisions
                if "interop_with_rival" in d.tags or "join_standard_body" in d.tags)
    score += min(0.35, learn * 0.22)
    if learn:
        reasons.append("Learns with/interop with rivals and standards.")
    win = sum(1 for d in p.decisions
              if "vanity_metrics" in d.tags or "stars_over_users" in d.tags)
    if win:
        score -= min(0.35, win * 0.17)
        flags.append("Vanity rivalry over user value.")
    return clamp01(score), reasons, flags

def score_existential_flex(p: Project) -> Tuple[float, List[str]]:
    reasons = []
    base = p.traits.get("pivot_readiness", 0.5)
    score = 0.45 * base
    pivots = sum(1 for d in p.decisions
                 if "rewrite_core" in d.tags or "license_change" in d.tags or "short_term_penalty" in d.tags)
    score += min(0.45, pivots * 0.22)
    if pivots:
        reasons.append("Willing to refactor/relicense for the mission.")
    return clamp01(score), reasons

def score_courage(p: Project) -> Tuple[float, List[str]]:
    reasons = []
    base = p.traits.get("courage", 0.5)
    score = 0.5 * base
    stands = sum(1 for d in p.decisions
                 if "privacy_stance" in d.tags or "reject_dark_patterns" in d.tags)
    score += min(0.35, stands * 0.25)
    if stands:
        reasons.append("Upholds privacy and rejects dark patterns.")
    return clamp01(score), reasons

# -----------------------------
# Aggregate + classification
# -----------------------------

THRESH_AVG = 0.58
THRESH_STRONG = 0.60

def evaluate(p: Project) -> Dict:
    jc, jc_r = score_just_cause(p)
    tt, tt_r, tt_f = score_trusting_teams(p)
    wr, wr_r, wr_f = score_worthy_rival(p)
    ef, ef_r = score_existential_flex(p)
    cl, cl_r = score_courage(p)

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
    flags = list(dict.fromkeys(tt_f + wr_f))

    avg = stats.mean(pillars.values())
    strong = sum(1 for v in pillars.values() if v >= THRESH_STRONG)

    penalty = 0.0
    if "Gatekeeping/toxic culture or bus-factor risk." in flags:
        penalty += 0.15
    if "Vanity rivalry over user value." in flags:
        penalty += 0.15

    infinite = (avg >= THRESH_AVG and strong >= 3 and penalty < 0.25)
    classification = "infinite-minded" if infinite else "finite-minded"

    def pct(x): return f"{round(100*x)}%"
    reason = f"Avg {pct(avg)} with {strong}/5 elements ≥{int(THRESH_STRONG*100)}%. Classification: {classification.upper()}."
    brief = []
    for k, lst in reasons.items():
        if lst:
            brief.append(f"{k}: {', '.join(lst)}")
    if flags:
        brief.append("Red flags: " + "; ".join(flags))
    if brief:
        reason += " " + " ".join(brief)

    return {
        "name": p.name,
        "classification": classification,
        "pillars": pillars,
        "average": avg,
        "strong": strong,
        "reason_text": reason,
        "flags": flags,
        "penalty": penalty,
        "mission": p.mission,
        "reasons_map": reasons,
    }

# -----------------------------
# Proof: invariants (12) & unit tests (6) = 18 checks
# -----------------------------

def invariant_checks(results: List[Dict]) -> List[str]:
    issues = []
    for r in results:
        expected = sum(r["pillars"].values()) / 5
        if abs(r["average"] - expected) > 1e-9:
            issues.append(f"{r['name']}: I1 average mismatch")
        for k, v in r["pillars"].items():
            if not (0.0 - 1e-9 <= v <= 1.0 + 1e-9):
                issues.append(f"{r['name']}: I2 pillar '{k}' out of [0,1]")
        strong_count = sum(1 for v in r["pillars"].values() if v >= THRESH_STRONG)
        if strong_count != r["strong"]:
            issues.append(f"{r['name']}: I3 strong_count mismatch")
        if r["classification"] == "infinite-minded":
            if not (r["average"] >= THRESH_AVG and r["strong"] >= 3 and r["penalty"] < 0.25):
                issues.append(f"{r['name']}: I4 infinite-minded rule violated")
        if r["penalty"] >= 0.25 and r["classification"] != "finite-minded":
            issues.append(f"{r['name']}: I5 penalty high but not finite-minded")
        finite_phrasing = any(s in r["mission"].lower() for s in ["this month", "this quarter", "by stars"])
        if finite_phrasing and r["average"] <= 0.55 and r["classification"] != "finite-minded":
            issues.append(f"{r['name']}: I6 finite phrasing + low avg should be finite-minded")
        if r["classification"] == "infinite-minded":
            if "Classification: INFINITE-MINDED." not in r["reason_text"]:
                issues.append(f"{r['name']}: I7 missing canonical infinite label")
        else:
            if "Classification: FINITE-MINDED." not in r["reason_text"]:
                issues.append(f"{r['name']}: I7 missing canonical finite label")
        if set(r["reasons_map"].keys()) != set(r["pillars"].keys()):
            issues.append(f"{r['name']}: I8 reasons keys mismatch pillars")
        src = next((p for p in DATA if p.name == r["name"]), None)
        if src is None:
            issues.append(f"{r['name']}: I9 cannot locate original project")
        else:
            r2 = evaluate(src)
            if (abs(r2["average"] - r["average"]) > 1e-9 or
                r2["classification"] != r["classification"] or
                r2["strong"] != r["strong"]):
                issues.append(f"{r['name']}: I9 non-deterministic evaluation")
        vals = list(r["pillars"].values())
        if not (min(vals) - 1e-9 <= r["average"] <= max(vals) + 1e-9):
            issues.append(f"{r['name']}: I10 average not between min/max pillars")
    for r in results:
        if not r["flags"] and r["average"] >= THRESH_AVG and r["strong"] >= 3:
            if r["classification"] != "infinite-minded":
                issues.append(f"{r['name']}: I11 zero-flags & thresholds should be infinite-minded")
    for r in results:
        if all(v >= 0.70 for v in r["pillars"].values()):
            if r["classification"] != "infinite-minded":
                issues.append(f"{r['name']}: I12 all-strong pillars should be infinite-minded")
    return issues

def unit_tests() -> List[str]:
    problems = []
    high = Project(
        name="All-High-OSS",
        mission="Advance human flourishing via open tooling for generations.",
        traits={"community_health": 0.9, "rival_learning": 0.9, "pivot_readiness": 0.9, "courage": 0.9},
        decisions=[Decision(2022, "Interop with rival; rewrite core; strong privacy stance",
                            ["interop_with_rival","rewrite_core","privacy_stance","onboarding"])]
    )
    r = evaluate(high)
    if r["classification"] != "infinite-minded":
        problems.append("U1: All-High-OSS should classify as infinite-minded")

    low = Project(
        name="All-Low-OSS",
        mission="Be #1 by stars this month.",
        traits={"community_health": 0.2, "rival_learning": 0.2, "pivot_readiness": 0.2, "courage": 0.2},
        decisions=[Decision(2024, "Gatekeeping; vanity rivalry; bus-factor risk",
                            ["gatekeeping","vanity_metrics","bus_factor_risk"])]
    )
    r2 = evaluate(low)
    if r2["classification"] != "finite-minded":
        problems.append("U2: All-Low-OSS should classify as finite-minded")

    edge_nf = Project(
        name="Edge-No-Flags-AvgBelow",
        mission="Serve users with a stable tool.",
        traits={"community_health": 0.55, "rival_learning": 0.55, "pivot_readiness": 0.55, "courage": 0.55},
        decisions=[Decision(2024, "Onboarding refresh", ["onboarding"])]
    )
    r3 = evaluate(edge_nf)
    if r3["average"] >= THRESH_AVG or r3["classification"] != "finite-minded":
        problems.append("U3: Edge-No-Flags-AvgBelow should be finite-minded with avg below threshold")

    edge_flags = Project(
        name="Edge-Flags-AvgHigh",
        mission="Long-term mission to support privacy-focused networking.",
        traits={"community_health": 0.80, "rival_learning": 0.80, "pivot_readiness": 0.80, "courage": 0.80},
        decisions=[
            Decision(2024, "Interop with rival", ["interop_with_rival"]),
            Decision(2025, "Public dunking for attention", ["vanity_metrics"]),
            Decision(2025, "Centralized permissions to single maintainer", ["bus_factor_risk"])
        ]
    )
    r4 = evaluate(edge_flags)
    if not (r4["average"] >= THRESH_AVG and r4["penalty"] >= 0.25 and r4["classification"] == "finite-minded"):
        problems.append("U4: Edge-Flags-AvgHigh should be finite-minded due to high penalty despite avg ≥ threshold")

    near_flip = Project(
        name="Near-Threshold-Flip",
        mission="Provide robust, long-lived utilities for developers.",
        traits={"community_health": 0.61, "rival_learning": 0.62, "pivot_readiness": 0.63, "courage": 0.64},
        decisions=[Decision(2024, "Interop & onboarding", ["interop_with_rival","onboarding"])]
    )
    r5 = evaluate(near_flip)
    if r5["average"] < THRESH_AVG or r5["strong"] < 3:
        problems.append("U5: Test setup error (avg/strong not at threshold)")
    if r5["classification"] != "infinite-minded":
        problems.append("U5: Near-Threshold-Flip should be infinite-minded")

    finite_phrase_high = Project(
        name="Finite-Phrase-HighAvg",
        mission="Be stable this quarter while empowering users for years.",
        traits={"community_health": 0.70, "rival_learning": 0.68, "pivot_readiness": 0.70, "courage": 0.70},
        decisions=[Decision(2024, "Interop with rival; privacy stance", ["interop_with_rival","privacy_stance"])]
    )
    r6 = evaluate(finite_phrase_high)
    if not (r6["average"] >= THRESH_AVG and r6["strong"] >= 3 and not r6["flags"]):
        problems.append("U6: Test setup error (should meet thresholds with no flags)")
    if r6["classification"] != "infinite-minded":
        problems.append("U6: Finite-Phrase-HighAvg should be infinite-minded despite phrasing")

    return problems

# -----------------------------
# Runner: prints ONLY the triad
# -----------------------------

def main():
    results = [evaluate(p) for p in DATA]
    issues = invariant_checks(results)
    tests = unit_tests()
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

