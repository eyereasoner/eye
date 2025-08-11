#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Diagnose — ARC (Answer / Reason / Check), self-contained

Model
  Diseases (independent priors):
    P(flu)=0.10, P(covid)=0.25
  Symptoms (noisy-OR given disease presence):
    cough: P(.|flu)=0.40, P(.|covid)=0.90
    fever: P(.|flu)=0.20, P(.|covid)=0.80
Output
  Posterior P(flu | evidence), P(covid | evidence) with a transparent world-by-world trace.
"""

from __future__ import annotations
from dataclasses import dataclass
from itertools import product
from typing import List, Tuple

# ───────────────────────── Model parameters ─────────────────────────
P_FLU, P_COVID = 0.10, 0.25
P_COUGH_FLU, P_COUGH_COVID = 0.40, 0.90
P_FEVER_FLU, P_FEVER_COVID = 0.20, 0.80

def noisy_or(ps: List[float]) -> float:
    prod = 1.0
    for p in ps:
        prod *= (1.0 - p)
    return 1.0 - prod

def symptom_prob(sym: str, flu: bool, covid: bool) -> float:
    terms: List[float] = []
    if sym == "cough":
        if flu:  terms.append(P_COUGH_FLU)
        if covid: terms.append(P_COUGH_COVID)
    else:
        if flu:  terms.append(P_FEVER_FLU)
        if covid: terms.append(P_FEVER_COVID)
    return noisy_or(terms) if terms else 0.0

# ─────────────────────── Inference by enumeration ───────────────────────
@dataclass
class World:
    flu: bool
    covid: bool
    prior: float
    p_cough: float
    p_fever: float
    likelihood: float
    weight: float

def enumerate_worlds(e_cough: bool, e_fever: bool) -> Tuple[List[World], float]:
    worlds: List[World] = []
    for flu, covid in product([False, True], repeat=2):
        prior = (P_FLU if flu else 1 - P_FLU) * (P_COVID if covid else 1 - P_COVID)
        p_c = symptom_prob("cough", flu, covid)
        p_f = symptom_prob("fever", flu, covid)
        like = (p_c if e_cough else 1 - p_c) * (p_f if e_fever else 1 - p_f)
        weight = prior * like
        worlds.append(World(flu, covid, prior, p_c, p_f, like, weight))
    Z = sum(w.weight for w in worlds)
    return worlds, Z

def posteriors(e_cough: bool, e_fever: bool) -> Tuple[float, float, float, List[World]]:
    worlds, Z = enumerate_worlds(e_cough, e_fever)
    p_flu   = sum(w.weight for w in worlds if w.flu)   / Z if Z > 0 else 0.0
    p_covid = sum(w.weight for w in worlds if w.covid) / Z if Z > 0 else 0.0
    return p_flu, p_covid, Z, worlds

# ───────────────────────────────── Answer ─────────────────────────────────
def print_answer(e_cough: bool = True, e_fever: bool = True) -> None:
    print("Answer")
    print("======")
    print(f"Evidence: cough={e_cough}, fever={e_fever}\n")

    p_flu, p_covid, Z, worlds = posteriors(e_cough, e_fever)

    for i, w in enumerate(worlds, 1):
        ws = f"{'flu' if w.flu else '¬flu'} ∧ {'covid' if w.covid else '¬covid'}"
        print(f"World {i}: {ws}")
        print(f"  Prior={w.prior:.6f}  Likelihood={w.likelihood:.6f}  Evidence contrib={w.weight:.6f}")
    print(f"\nEvidence probability Z = {Z:.6f}\n")

    print(f"Posterior P(flu   | evidence) = {p_flu:.3f}")
    print(f"Posterior P(covid | evidence) = {p_covid:.3f}")

# ─────────────────────────────── Reason why ───────────────────────────────
def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We enumerate four disease worlds (flu ∈ {0,1} × covid ∈ {0,1}).")
    print("Each world gets weight  prior × likelihood, where the likelihood of symptoms")
    print("uses a noisy-OR per symptom (e.g., cough caused by flu and/or covid).")
    print("Bayes’ rule normalizes by Z = Σ_world prior×likelihood to yield P(flu|e) and P(covid|e).")

# ──────────────────────────────── Check (harness) ────────────────────────────────
def almost_eq(a: float, b: float, tol: float = 1e-12) -> bool:
    return abs(a - b) <= tol * max(1.0, abs(a), abs(b))

def print_check() -> None:
    import itertools
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    # 1) Normalization: Z equals sum of contributions and sits in [0,1]
    for ev in [(True, True), (True, False), (False, True), (False, False)]:
        worlds, Z = enumerate_worlds(*ev)
        sumw = sum(w.weight for w in worlds)
        in_range = 0.0 <= Z <= 1.0 and all(0.0 <= w.weight <= 1.0 for w in worlds)
        print(f"Z == Σ weights for evidence={ev}? {almost_eq(Z, sumw)}")
        ok_all &= almost_eq(Z, sumw) and in_range

    # 2) Bounds for posteriors
    for ev in [(True, True), (True, False), (False, True), (False, False)]:
        p_flu, p_covid, _, _ = posteriors(*ev)
        bounds_ok = (0.0 <= p_flu <= 1.0) and (0.0 <= p_covid <= 1.0)
        print(f"Posteriors in [0,1] for evidence={ev}? {bounds_ok}")
        ok_all &= bounds_ok

    # 3) Sanity: (cough∧fever) → higher P(covid) than no-symptoms; also raises P(flu)
    p_flu_none, p_cov_none, _, _ = posteriors(False, False)
    p_flu_both, p_cov_both, _, _ = posteriors(True, True)
    print(f"P(covid|cough,fever) > P(covid|none)? {p_cov_both > p_cov_none}")
    print(f"P(flu  |cough,fever) > P(flu  |none)? {p_flu_both > p_flu_none}")
    ok_all &= (p_cov_both > p_cov_none) and (p_flu_both > p_flu_none)

    # 4) Cough is slightly more indicative of flu than fever (with these params)
    p_flu_cough, _, _, _ = posteriors(True, False)
    p_flu_fever, _, _, _ = posteriors(False, True)
    print(f"P(flu|cough) ≥ P(flu|fever)? {p_flu_cough >= p_flu_fever}")
    ok_all &= (p_flu_cough >= p_flu_fever)

    # 5) Determinism / idempotence
    a = posteriors(True, True)
    b = posteriors(True, True)
    print(f"Deterministic (same inputs ⇒ same result)? {a == b}")
    ok_all &= (a == b)

    print(f"\nAll checks passed? {ok_all}")

# ─────────────────────────────────── Main ───────────────────────────────────
if __name__ == "__main__":
    # Default demo matches the original: both symptoms observed
    print_answer(True, True)
    print_reason()
    print_check()

