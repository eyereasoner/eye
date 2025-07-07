# Backward‑chained style trace for the prescription model
#
# It enumerates the four disease worlds, prints their contribution to
# the evidence, then for **each drug** shows how those same worlds
# accumulate to the posterior P(good(drug) | evidence).
# ------------------------------------------------------------------

from itertools import product

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

# -------------------- Utility functions --------------------------
def noisy_or(ps):
    prod = 1.0
    for p in ps:
        prod *= 1 - p
    return 1 - prod

def symptom_prob(sym, flu, covid):
    t = []
    if flu:   t.append(SYMPTOMS[sym]["flu"])
    if covid: t.append(SYMPTOMS[sym]["covid"])
    return noisy_or(t) if t else 0.0

def effective_prob(drug, flu, covid):
    t = []
    if flu:   t.append(EFFECTIVE[drug]["flu"])
    if covid: t.append(EFFECTIVE[drug]["covid"])
    return noisy_or(t) if t else 0.0

# -------------------- Traced inference ---------------------------
def trace_prescribe(e_cough, e_fever):
    print(f"=== Proof for prescribe(cough={e_cough}, fever={e_fever}) ===\n")
    
    # 1 ▸ Enumerate worlds & compute evidence weight ----------------
    worlds = []   # list of (flu?, covid?, weight)
    step = 1
    for flu, covid in product([False, True], repeat=2):
        prior = (P_FLU if flu else 1 - P_FLU) * (P_COVID if covid else 1 - P_COVID)
        p_cough = symptom_prob("cough", flu, covid)
        p_fever = symptom_prob("fever", flu, covid)
        likelihood = (p_cough if e_cough else 1 - p_cough) * \
                     (p_fever if e_fever else 1 - p_fever)
        weight = prior * likelihood
        worlds.append((flu, covid, weight))
        
        flu_str  = "flu" if flu else "¬flu"
        cov_str  = "covid" if covid else "¬covid"
        print(f"Step {step:02d}: world ({flu_str} ∧ {cov_str})")
        print(f"  • Prior      = {prior:.6f}")
        print(f"  • Likelihood = {likelihood:.6f}")
        print(f"  • Contribution to evidence = {weight:.6f}\n")
        step += 1

    evidence_prob = sum(w[2] for w in worlds)
    print(f"✔ Evidence probability = {evidence_prob:.6f}\n")
    
    # 2 ▸ Evaluate each drug ----------------------------------------
    best_drug, best_prob = None, -1.0
    for drug in ("oseltamivir", "paxlovid"):
        p_se = SIDE_EFFECT[drug]
        posterior_good = 0.0
        print(f"--- Drug = {drug} ---")
        for flu, covid, weight in worlds:
            p_eff  = effective_prob(drug, flu, covid)
            p_good = p_eff * (1 - p_se)          # need efficacy & no side‑effect
            contrib = weight * p_good / evidence_prob
            posterior_good += weight * p_good
            
            flu_str  = "flu" if flu else "¬flu"
            cov_str  = "covid" if covid else "¬covid"
            print(f"  ∙ ({flu_str} ∧ {cov_str})  "
                  f"p_eff={p_eff:.2f}  p_good={p_good:.2f}  "
                  f"posterior contrib={contrib:.5f}")
        posterior_good /= evidence_prob
        print(f"Posterior P(good({drug})) = {posterior_good:.3f}\n")
        
        if posterior_good > best_prob:
            best_drug, best_prob = drug, posterior_good
    
    print(f"➡ Recommend **{best_drug}** "
          f"(probability of net benefit ≈ {best_prob:.1%})")

# Demo for both symptoms observed
trace_prescribe(True, True)

