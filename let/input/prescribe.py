"""
prescribe.py
============

Model
-----
Independent diseases
    P(flu)   = 0.10
    P(covid) = 0.25

Symptom generation (noisy-OR)
    P(cough | flu)   = 0.40     P(fever | flu)   = 0.20
    P(cough | covid) = 0.90     P(fever | covid) = 0.80

Drug effectiveness (noisy-OR of disease-conditioned causes)
    oseltamivir : 0.85 if flu   , 0.10 if covid
    paxlovid    : 0.05 if flu   , 0.90 if covid

Side-effect rates (independent)
    P(side_effect | oseltamivir) = 0.15
    P(side_effect | paxlovid)    = 0.25

good(D)  ≜  effective(D) ∧ ¬side_effect(D)

Function
--------
    prescribe(e_cough: bool, e_fever: bool)
        → {"oseltamivir": p, "paxlovid": p}
returns P(good(D) | evidence) for each drug.
"""

from itertools import product
from typing import Dict, Tuple


# ───────────────────────────────────────────────────────────────
# 1 ▸  Model parameters
# ───────────────────────────────────────────────────────────────
P_FLU     = 0.10
P_COVID   = 0.25

SYMPTOMS = {
    "cough": {"flu": 0.40, "covid": 0.90},
    "fever": {"flu": 0.20, "covid": 0.80},
}

EFFECTIVE = {
    "oseltamivir": {"flu": 0.85, "covid": 0.10},
    "paxlovid":    {"flu": 0.05, "covid": 0.90},
}

SIDE_EFFECT = {
    "oseltamivir": 0.15,
    "paxlovid":    0.25,
}


# ───────────────────────────────────────────────────────────────
# 2 ▸  Helper utilities
# ───────────────────────────────────────────────────────────────
def noisy_or(triggers) -> float:
    """Return 1 − ∏(1−p_i) for a list of probabilities p_i."""
    prod = 1.0
    for p in triggers:
        prod *= 1.0 - p
    return 1.0 - prod


def symptom_prob(symptom: str, flu: bool, covid: bool) -> float:
    """P(symptom | flu?, covid?) via noisy-OR of active causes."""
    triggers = []
    if flu:
        triggers.append(SYMPTOMS[symptom]["flu"])
    if covid:
        triggers.append(SYMPTOMS[symptom]["covid"])
    return noisy_or(triggers) if triggers else 0.0


def effective_prob(drug: str, flu: bool, covid: bool) -> float:
    """P(effective(drug) | flu?, covid?)"""
    triggers = []
    if flu:
        triggers.append(EFFECTIVE[drug]["flu"])
    if covid:
        triggers.append(EFFECTIVE[drug]["covid"])
    return noisy_or(triggers) if triggers else 0.0


# ───────────────────────────────────────────────────────────────
# 3 ▸  Exact Bayesian inference by enumeration
# ───────────────────────────────────────────────────────────────
def prescribe(evidence_cough: bool, evidence_fever: bool) -> Dict[str, float]:
    """
    Return posterior probability of good(drug) for each drug,
    conditioning on observed cough / fever.
    """
    worlds = []        # list of (flu?, covid?, joint weight P(w|e))
    for flu, covid in product([False, True], repeat=2):
        # prior over diseases (independent)
        p_prior = (P_FLU if flu else 1 - P_FLU) * \
                  (P_COVID if covid else 1 - P_COVID)

        # likelihood of observed symptoms
        p_cough = symptom_prob("cough", flu, covid)
        p_fever = symptom_prob("fever", flu, covid)

        p_lik  = (p_cough if evidence_cough else 1 - p_cough) * \
                 (p_fever if evidence_fever else 1 - p_fever)

        worlds.append((flu, covid, p_prior * p_lik))

    norm = sum(w[2] for w in worlds)             # P(evidence)

    post_good: Dict[str, float] = {d: 0.0 for d in EFFECTIVE}
    for drug in EFFECTIVE:
        p_se = SIDE_EFFECT[drug]

        for flu, covid, weight in worlds:
            p_eff  = effective_prob(drug, flu, covid)
            p_good = p_eff * (1 - p_se)          # independent side-effect
            post_good[drug] += weight * p_good

        post_good[drug] /= norm                  # normalise

    return post_good


# ───────────────────────────────────────────────────────────────
# 4 ▸  Demo
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    # Example: patient presents with both cough **and** fever
    posterior = prescribe(True, True)

    # pretty-print sorted by probability
    for drug, prob in sorted(posterior.items(), key=lambda kv: kv[1], reverse=True):
        print(f"Posterior P(good({drug})) = {prob:.3f}")

    best_drug, best_prob = max(posterior.items(), key=lambda kv: kv[1])
    print(f"\n--> Recommend {best_drug} "
          f"(probability of net benefit ≈ {best_prob:.1%})")

