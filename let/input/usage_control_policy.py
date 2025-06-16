"""
usage_control_policy.py
───────────────────────
Decision-Theoretic ProbLog (DTProbLog) demo for *usage-control*.

               ┌────────────┐
      Request  │  Dataset   │   Benefit  +25 €  if Carol uses it correctly
   ───────────▶│   Carol    │
               └────────────┘
            ▲                ▼
 require_obligation?     misuse risk –80 € if Carol leaks the data
      (cost −5 €)

A simple UCON-A style policy:
   • at decision time we may attach an *obligation* (“delete after use”)
   • the obligation reduces the probability that Carol misuses the data
   • we weigh that against its monitoring/enforcement cost
"""

from problog.program import PrologString
from problog.tasks.dtproblog import dtproblog
from problog.logic import Term

# ───────────────────────────────────────────────────────────────
# 1 ▸  ProbLog model
# ───────────────────────────────────────────────────────────────
MODEL = r"""
% ==============  DECISION  =====================================
?::require_obligation.          % attach deletion-obligation & audit?

% ==============  (Uncertain) compliance behaviour  =============
0.90::compliant :- require_obligation.
0.40::compliant :- \+require_obligation.

misuse :- \+compliant.          % misuse if Carol is not compliant
success :-  compliant.          % proper use

% ==============  UTILITIES  ====================================
utility(require_obligation,  -5).     % enforcement cost
utility(success,             25).     % legitimate value obtained
utility(misuse,             -80).     % damage from leakage
"""

# ───────────────────────────────────────────────────────────────
# 2 ▸  Run DTProbLog optimisation
# ───────────────────────────────────────────────────────────────
decisions, max_eu, _ = dtproblog(PrologString(MODEL))

# helper to read either the literal or its negation
def truth(atom: str) -> bool:
    pos = decisions.get(Term(atom))
    if pos is not None:
        return bool(pos)
    neg = decisions.get(Term(r'\+' + atom))
    return not bool(neg) if neg is not None else False

attach_obligation = truth('require_obligation')

# ───────────────────────────────────────────────────────────────
# 3 ▸  Human-readable report
# ───────────────────────────────────────────────────────────────
print("Usage-control policy selected by DTProbLog")
print("------------------------------------------")
print(f"Attach deletion obligation? : {attach_obligation}")
print(f"Expected utility            : {max_eu:.2f} €\n")

print("Raw DTProbLog decision literals:")
for lit, val in decisions.items():
    print(f"  {lit}: {val}")
