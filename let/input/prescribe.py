# prescribe.py  —  probabilistic drug-selection demo with ProbLog 2
#
# Requires:  pip install problog

from problog.program import PrologString
from problog import get_evaluatable


def prescribe_with_problog(evidence_cough: bool, evidence_fever: bool):
    """
    Return the probability that each drug produces a *good outcome*
    (effective AND no serious side-effect) given observed symptoms.
    """
    model = f"""
    %==========================
    %  DISEASE PRIORS
    %==========================
    0.10::flu.            % prior P(flu)
    0.25::covid.          % prior P(covid)

    %==========================
    %  SYMPTOM CAUSATION
    %==========================
    0.40::cough :- flu.
    0.90::cough :- covid.
    0.20::fever :- flu.
    0.80::fever :- covid.

    %==========================
    %  DRUG MODELS
    %  (effectiveness conditioned on disease)
    %==========================
    % -- oseltamivir (Tamiflu) mainly targets flu -------------
    0.85::effective(oseltamivir) :- flu.
    0.10::effective(oseltamivir) :- covid.     % off-label, weak

    0.15::side_effect(oseltamivir).            % generic risk

    % -- paxlovid targets covid --------------------------------
    0.90::effective(paxlovid) :- covid.
    0.05::effective(paxlovid) :- flu.          % incidental

    0.25::side_effect(paxlovid).               % higher risk

    %==========================
    %  NET BENEFIT RULE
    %==========================
    good(D) :- effective(D), \\+ side_effect(D).

    %==========================
    %  OBSERVED SYMPTOMS
    %==========================
    evidence(cough,{str(evidence_cough).lower()}).
    evidence(fever,{str(evidence_fever).lower()}).

    %==========================
    %  QUERIES
    %==========================
    query(good(oseltamivir)).
    query(good(paxlovid)).
    """
    return (
        get_evaluatable()
        .create_from(PrologString(model))
        .evaluate()
    )


if __name__ == "__main__":
    # Example: patient presents with BOTH cough and fever
    posterior = prescribe_with_problog(evidence_cough=True, evidence_fever=True)

# ----------------------------------------------
#  pretty-print and choose the best drug
# ----------------------------------------------
# We sort by probability (kv[1]), highest first.
for term, prob in sorted(posterior.items(),
                         key=lambda kv: kv[1],
                         reverse=True):
    print(f"Posterior P({term}) = {prob:.3f}")

# pick the term with maximum probability
best_term  = max(posterior, key=posterior.get)
best_prob  = posterior[best_term]
drug_name  = best_term.args[0]          # extract 'oseltamivir' / 'paxlovid'

print(f"\n--> Recommend {drug_name} "
      f"(probability of net benefit ≈ {best_prob:.1%})")
