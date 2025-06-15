from problog.program import PrologString
from problog import get_evaluatable

def diagnose_with_problog(evidence_cough: bool, evidence_fever: bool):
    model = f"""
    % ---- priors ----------------------------------------------------
    0.10::flu.
    0.25::covid.

    % ---- symptom generation ----------------------------------------
    0.40::cough :- flu.
    0.90::cough :- covid.
    0.20::fever :- flu.
    0.80::fever :- covid.

    % ---- observed evidence -----------------------------------------
    evidence(cough,{str(evidence_cough).lower()}).
    evidence(fever,{str(evidence_fever).lower()}).

    % ---- queries ----------------------------------------------------
    query(flu).
    query(covid).
    """

    result = (get_evaluatable()
              .create_from(PrologString(model))
              .evaluate())
    return result

if __name__ == "__main__":
    posterior = diagnose_with_problog(True, True)
    for disease, prob in posterior.items():
        print(f"Posterior P({disease}) = {prob:.3f}")
