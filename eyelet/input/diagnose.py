"""
diagnose.py
===========

Model
-----
Priors
    P(flu)   = 0.10
    P(covid) = 0.25        (independent)

Symptom generators
    P(cough | flu)   = 0.40
    P(cough | covid) = 0.90
    P(fever | flu)   = 0.20
    P(fever | covid) = 0.80

If both diseases are present, each triggers the symptom independently
(“noisy-OR”).  With neither disease present the symptom probability is 0.

Function
--------
    diagnose(e_cough: bool, e_fever: bool) -> dict
returns  {"flu": <posterior>, "covid": <posterior>}.

Usage example

    if __name__ == "__main__":
        post = diagnose(True, True)   # saw cough & fever
        for d, p in post.items():
            print(f"Posterior P({d}) = {p:.3f}")
"""

from itertools import product
from typing import Dict, Tuple


# ------------------------------------------------------------------
# 1 ▸  Model parameters
# ------------------------------------------------------------------
P_FLU        = 0.10
P_COVID      = 0.25

P_COUGH_FLU   = 0.40
P_COUGH_COVID = 0.90

P_FEVER_FLU   = 0.20
P_FEVER_COVID = 0.80


# ------------------------------------------------------------------
# 2 ▸  Helper: symptom probability given the diseases (noisy-OR)
# ------------------------------------------------------------------
def noisy_or(p_list):
    """Return 1 − ∏(1−p_i)  for the probabilities in p_list."""
    prod = 1.0
    for p in p_list:
        prod *= 1.0 - p
    return 1.0 - prod


def symptom_probs(flu: bool, covid: bool) -> Tuple[float, float]:
    """P(cough|…) and P(fever|…) for this disease combination."""
    cough_triggers = []
    fever_triggers = []
    if flu:
        cough_triggers.append(P_COUGH_FLU)
        fever_triggers.append(P_FEVER_FLU)
    if covid:
        cough_triggers.append(P_COUGH_COVID)
        fever_triggers.append(P_FEVER_COVID)

    p_cough = noisy_or(cough_triggers) if cough_triggers else 0.0
    p_fever = noisy_or(fever_triggers) if fever_triggers else 0.0
    return p_cough, p_fever


# ------------------------------------------------------------------
# 3 ▸  Main inference routine (exact enumeration)
# ------------------------------------------------------------------
def diagnose(evidence_cough: bool, evidence_fever: bool) -> Dict[str, float]:
    """
    Return posterior probabilities { "flu": p, "covid": p } given
    Boolean evidence on cough and fever.
    """
    worlds = []   # (flu?, covid?, joint probability)

    # iterate over the four combinations of (flu, covid)
    for flu, covid in product([False, True], repeat=2):
        # ─ prior ──────────────────────────────────────────────
        prior = (P_FLU if flu else 1 - P_FLU) * \
                (P_COVID if covid else 1 - P_COVID)

        # ─ likelihood P(evidence | world) ─────────────────────
        p_cough, p_fever = symptom_probs(flu, covid)
        like = (p_cough if evidence_cough else 1 - p_cough) * \
               (p_fever if evidence_fever else 1 - p_fever)

        worlds.append((flu, covid, prior * like))

    # ─ normalise ───────────────────────────────────────────────
    total = sum(w[2] for w in worlds)
    post_flu   = sum(w[2] for w in worlds if w[0]) / total
    post_covid = sum(w[2] for w in worlds if w[1]) / total

    return {"flu": post_flu, "covid": post_covid}


# ------------------------------------------------------------------
# 4 ▸  Demo (mirrors original script)
# ------------------------------------------------------------------
if __name__ == "__main__":
    posterior = diagnose(True, True)      # observed cough & fever
    for disease, prob in posterior.items():
        print(f"Posterior P({disease}) = {prob:.3f}")

