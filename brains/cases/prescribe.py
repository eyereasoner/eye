#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Prescription model — ARC (Answer / Reason / Check), self-contained

Model (mirrors original):
  • Diseases (independent priors): P(flu)=0.10, P(covid)=0.25
  • Symptoms (noisy-OR given disease presence)
      cough:  P(.|flu)=0.40, P(.|covid)=0.90
      fever:  P(.|flu)=0.20, P(.|covid)=0.80
  • Drug efficacy (noisy-OR across active diseases)
      oseltamivir: eff_flu=0.85, eff_covid=0.10, side_effect=0.15
      paxlovid:    eff_flu=0.05, eff_covid=0.90, side_effect=0.25

Posterior target per drug D:
  P(good(D) | evidence) = Σ_world P(world | evidence) * P(effective(D)|world) * P(no side-effect|D)

We report those posteriors and recommend the argmax.
"""

from __future__ import annotations
from dataclasses import dataclass
from itertools import product
from typing import Dict, List, Tuple

# -------------------- Model parameters ---------------------------
P_FLU, P_COVID = 0.10, 0.25

SYMPTOMS = {
    "cough": {"flu": 0.40, "covid": 0.90},
    "fever": {"flu": 0.20, "covid": 0.80},
}

EFFECTIVE = {
    "oseltamivir": {"flu": 0.85, "covid": 0.10},
    "paxlovid":    {"flu": 0.05, "covid": 0.90},
}

SIDE_EFFECT = {"oseltamivir": 0.15, "paxlovid": 0.25}

DRUGS = ("oseltamivir", "paxlovid")


# -------------------- Utilities ---------------------------
def noisy_or(ps: List[float]) -> float:
    prod = 1.0
    for p in ps:
        prod *= (1.0 - p)
    return 1.0 - prod

def symptom_prob(sym: str, flu: bool, covid: bool) -> float:
    terms: List[float] = []
    if flu:
        terms.append(SYMPTOMS[sym]["flu"])
    if covid:
        terms.append(SYMPTOMS[sym]["covid"])
    return noisy_or(terms) if terms else 0.0

def effective_prob(drug: str, flu: bool, covid: bool) -> float:
    terms: List[float] = []
    if flu:
        terms.append(EFFECTIVE[drug]["flu"])
    if covid:
        terms.append(EFFECTIVE[drug]["covid"])
    return noisy_or(terms) if terms else 0.0


# -------------------- Inference core ---------------------------
@dataclass
class World:
    flu: bool
    covid: bool
    prior: float
    likelihood: float
    weight: float
    p_cough: float
    p_fever: float

def enumerate_worlds(e_cough: bool, e_fever: bool) -> Tuple[List[World], float]:
    """Enumerate the 4 worlds, compute weights given evidence, and return evidence probability."""
    worlds: List[World] = []
    for flu, covid in product([False, True], repeat=2):
        prior = (P_FLU if flu else (1 - P_FLU)) * (P_COVID if covid else (1 - P_COVID))
        p_cough = symptom_prob("cough", flu, covid)
        p_fever = symptom_prob("fever", flu, covid)
        likelihood = (p_cough if e_cough else (1 - p_cough)) * (p_fever if e_fever else (1 - p_fever))
        weight = prior * likelihood
        worlds.append(World(flu, covid, prior, likelihood, weight, p_cough, p_fever))
    evidence_prob = sum(w.weight for w in worlds)
    return worlds, evidence_prob

@dataclass
class DrugPosterior:
    posterior: float
    by_world: List[Tuple[str, float, float, float]]  # (world_str, p_eff, p_good, posterior_contrib)

def drug_posteriors(e_cough: bool, e_fever: bool) -> Tuple[Dict[str, DrugPosterior], float, List[World]]:
    worlds, Z = enumerate_worlds(e_cough, e_fever)
    posts: Dict[str, DrugPosterior] = {}
    for drug in DRUGS:
        p_se = SIDE_EFFECT[drug]
        numer = 0.0
        details: List[Tuple[str, float, float, float]] = []
        for w in worlds:
            p_eff = effective_prob(drug, w.flu, w.covid)
            p_good = p_eff * (1.0 - p_se)
            contrib = (w.weight * p_good / Z) if Z > 0 else 0.0
            numer += w.weight * p_good
            world_str = f"{'flu' if w.flu else '¬flu'} ∧ {'covid' if w.covid else '¬covid'}"
            details.append((world_str, p_eff, p_good, contrib))
        posterior = (numer / Z) if Z > 0 else 0.0
        posts[drug] = DrugPosterior(posterior=posterior, by_world=details)
    return posts, Z, worlds

def recommend(e_cough: bool, e_fever: bool) -> Tuple[str, float, Dict[str, DrugPosterior], float, List[World]]:
    posts, Z, worlds = drug_posteriors(e_cough, e_fever)
    best_drug = max(posts.items(), key=lambda kv: kv[1].posterior)[0]
    return best_drug, posts[best_drug].posterior, posts, Z, worlds


# ----------------------------- ARC: Answer -----------------------------
def print_answer(e_cough: bool = True, e_fever: bool = True) -> None:
    print("Answer")
    print("======")
    print(f"Evidence: cough={e_cough}, fever={e_fever}\n")

    best, best_p, posts, Z, worlds = recommend(e_cough, e_fever)

    # Show worlds and evidence contributions
    for i, w in enumerate(worlds, 1):
        ws = f"{'flu' if w.flu else '¬flu'} ∧ {'covid' if w.covid else '¬covid'}"
        print(f"World {i}: {ws}")
        print(f"  Prior={w.prior:.6f}  Likelihood={w.likelihood:.6f}  Evidence contrib={w.weight:.6f}")
    print(f"\nEvidence probability Z = {Z:.6f}\n")

    # Per-drug traces
    for drug in DRUGS:
        print(f"--- Drug: {drug} ---")
        for (ws, p_eff, p_good, contrib) in posts[drug].by_world:
            print(f"  {ws}: p_eff={p_eff:.2f}  p_good={p_good:.2f}  posterior contrib={contrib:.5f}")
        print(f"Posterior P(good({drug}) | evidence) = {posts[drug].posterior:.3f}\n")

    print(f"➡ Recommend {best}  (probability of net benefit ≈ {best_p:.1%})")

# -------------------------- ARC: Reason why ---------------------------
def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We model uncertainty over four disease worlds (flu ∈ {0,1} × covid ∈ {0,1}).")
    print("Priors multiply because flu and covid are treated as independent in this toy.")
    print("Symptom likelihoods use a noisy-OR: if either disease can cause the symptom,")
    print("the chance we see it is 1 − Π(1 − p_cause).")
    print("Bayesian update gives each world a weight ∝ prior × likelihood(evidence | world).")
    print("For a drug D, net benefit in a world is:")
    print("  p_good = p_effective(D | world) × (1 − p_side-effect(D))")
    print("Then P(good(D) | evidence) is the weighted average of p_good across worlds.")
    print("We recommend the drug with the highest posterior probability of net benefit.")

# ------------------------ ARC: Check (harness) ------------------------
def almost_eq(a: float, b: float, tol: float = 1e-9) -> bool:
    return abs(a - b) <= tol * max(1.0, abs(a), abs(b))

def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Normalization: Z equals sum of world weights; weights ∈ [0,1]
    for ev in [(True, True), (True, False), (False, True), (False, False)]:
        worlds, Z = enumerate_worlds(*ev)
        sumw = sum(w.weight for w in worlds)
        in_range = all(0.0 <= w.weight <= 1.0 for w in worlds) and 0.0 <= Z <= 1.0
        print(f"Z equals sum(weights) for evidence={ev}? {almost_eq(Z, sumw)}")
        ok_all &= almost_eq(Z, sumw) and in_range

    # 2) Posterior computation matches “sum of contributions”
    posts, Z, worlds = drug_posteriors(True, True)
    for d in DRUGS:
        summed = sum(contrib for (_, _, _, contrib) in posts[d].by_world)
        print(f"Contribs sum to posterior for {d}? {almost_eq(summed, posts[d].posterior)}")
        ok_all &= almost_eq(summed, posts[d].posterior)

    # 3) Sanity on (True, True): paxlovid should dominate (covid-heavy evidence)
    best, best_p, posts, Z, _ = recommend(True, True)
    expect = "paxlovid"
    print(f"(cough,fever)=(True,True) recommends paxlovid? {best == expect}")
    ok_all &= (best == expect)

    # 4) Monotonicity hint: adding a positive covid symptom shouldn’t reduce paxlovid’s posterior
    p_no_fever = drug_posteriors(True, False)[0]["paxlovid"].posterior
    p_both     = posts["paxlovid"].posterior
    print(f"P(paxlovid good | cough) ≤ P(paxlovid good | cough+fever)? {p_no_fever <= p_both + 1e-12}")
    ok_all &= (p_no_fever <= p_both + 1e-12)

    # 5) Bounds: posteriors are in [0,1]
    bounds_ok = all(0.0 <= posts[d].posterior <= 1.0 for d in DRUGS)
    print(f"Posteriors within [0,1]? {bounds_ok}")
    ok_all &= bounds_ok

    print(f"\nAll checks passed? {ok_all}")

# ------------------------------- Main -------------------------------
if __name__ == "__main__":
    # Default demo: both symptoms observed (as in the original)
    print_answer(True, True)
    print_reason()
    print_check()

