# Backward‑chained style trace for the Bayesian diagnosis model
#
# We mirror the structure of the earlier logic proof, but now each
# “world” (flu?, covid?) is examined, its probability mass is
# computed, and we finish with the posterior P(flu) and P(covid).
# ------------------------------------------------------------------

from itertools import product

# model parameters --------------------------------------------------
P_FLU, P_COVID          = 0.10, 0.25
P_COUGH_FLU, P_COUGH_COVID = 0.40, 0.90
P_FEVER_FLU, P_FEVER_COVID = 0.20, 0.80

def noisy_or(ps):
    prod = 1.0
    for p in ps:
        prod *= (1.0 - p)
    return 1.0 - prod

def symptom_probs(flu, covid):
    coughs, fevers = [], []
    if flu:
        coughs.append(P_COUGH_FLU)
        fevers.append(P_FEVER_FLU)
    if covid:
        coughs.append(P_COUGH_COVID)
        fevers.append(P_FEVER_COVID)
    return (noisy_or(coughs) if coughs else 0.0,
            noisy_or(fevers) if fevers else 0.0)

def trace_diagnose(e_cough, e_fever):
    print(f"=== Proof for diagnose(cough={e_cough}, fever={e_fever}) ===\n")
    worlds = []
    step = 1
    for flu, covid in product([False, True], repeat=2):
        prior = (P_FLU if flu else 1-P_FLU) * (P_COVID if covid else 1-P_COVID)
        p_cough, p_fever = symptom_probs(flu, covid)
        like = (p_cough if e_cough else 1-p_cough) * \
               (p_fever if e_fever else 1-p_fever)
        joint = prior * like
        worlds.append((flu, covid, joint))
        
        flu_str   = "flu"   if flu   else "¬flu"
        cov_str   = "covid" if covid else "¬covid"
        print(f"Step {step:02d}: world ({flu_str} ∧ {cov_str})")
        print(f"  • Prior      = {prior:.6f}")
        print(f"  • Likelihood = {like:.6f}")
        print(f"  • Contribution to evidence = {joint:.6f}\n")
        step += 1

    total = sum(w[2] for w in worlds)
    post_flu   = sum(w[2] for w in worlds if w[0]) / total
    post_covid = sum(w[2] for w in worlds if w[1]) / total

    print("✔ Evidence probability =", f"{total:.6f}\n")
    print(f"Posterior P(flu)   = {post_flu:.3f}")
    print(f"Posterior P(covid) = {post_covid:.3f}")

# Demo for the “both symptoms observed” case
trace_diagnose(True, True)

