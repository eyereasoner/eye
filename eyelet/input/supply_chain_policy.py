"""
supply_chain_policy.py
----------------------
Decision-Theoretic ProbLog demo (DTProbLog).

Choose:
  • which supplier to order from  (A = cheaper / riskier, B = safer / pricier)
  • whether to pay for expedited shipping

The model weighs purchase & shipping costs against
probabilistic delay risk and market-demand revenue.

Decisions
---------
  • Supplier choice      : A (cheaper / riskier)  vs.  B (safer / pricier)
  • Shipping mode        : Expedited vs. Normal

Uncertainties
-------------
  • supplier_disruption  – depends on chosen supplier
  • ship_delay           – depends on shipping mode
  • demand_high          – exogenous market demand

Utilities (EUR, illustrative)
-----------------------------
  +100  profit if goods arrive on time AND demand is high
   +60  profit if goods arrive on time AND demand is low
   -40  penalty if delivery is late (any cause)
   -30  purchase cost when ordering from Supplier A
   -50  purchase cost when ordering from Supplier B
   -15  surcharge for expedited shipping
"""

from problog.program import PrologString
from problog.tasks.dtproblog import dtproblog
from problog.logic import Term


MODEL = r"""
% =========  DECISIONS  =========
?::use_supplier_a.              % false ⇒ Supplier B
?::expedite.                    % false ⇒ Normal shipping

% =========  UNCERTAINTIES =====
0.20::supplier_disruption :- use_supplier_a.
0.05::supplier_disruption :- \+use_supplier_a.

0.05::ship_delay :- expedite.
0.15::ship_delay :- \+expedite.

0.40::demand_high.

% =========  DERIVED PREDICATES =
delay   :- supplier_disruption.
delay   :- ship_delay.
ontime  :- \+delay.

profit_high :- ontime, demand_high.
profit_low  :- ontime, \+demand_high.

% =========  UTILITIES ==========
utility(use_supplier_a,  -30).
use_supplier_b :- \+use_supplier_a.
utility(use_supplier_b,  -50).

utility(expedite,        -15).

utility(profit_high,     100).
utility(profit_low,       60).

utility(delay,           -40).
"""


# ────────────────────────────────────────────────────────────────
#  Helper: return truth of a positive atom, whatever DTProbLog gave
# ────────────────────────────────────────────────────────────────
def atom_truth(decisions, atom):
    pos = decisions.get(Term(atom))
    if pos is not None:
        return bool(pos)
    neg = decisions.get(Term(r'\+' + atom))
    if neg is not None:
        return not bool(neg)
    return None  # literal absent


def chosen_supplier(decisions):
    """Return 'A' or 'B' based on any supplier literal present."""
    t_a = atom_truth(decisions, 'use_supplier_a')
    if t_a is not None:
        return 'A' if t_a else 'B'

    # fall-back to the B-literal set
    t_b = atom_truth(decisions, 'use_supplier_b')
    return 'B' if t_b else 'A'


def chosen_shipping(decisions):
    return 'Expedited' if atom_truth(decisions, 'expedite') else 'Normal'


# ────────────────────────────────────────────────────────────────
#  Run optimisation & print results
# ────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    decision_map, exp_profit, _ = dtproblog(PrologString(MODEL))

    print("Optimal policy (maximum expected utility)")
    print("----------------------------------------")
    print(f"Chosen supplier : {chosen_supplier(decision_map)}")
    print(f"Shipping mode   : {chosen_shipping(decision_map)}")
    print(f"Expected profit : {exp_profit:.2f}\n")

    print("Raw DTProbLog decision literals:")
    for lit, val in decision_map.items():
        print(f"  {lit}: {val}")
