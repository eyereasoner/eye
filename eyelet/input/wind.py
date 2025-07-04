#!/usr/bin/env python3
"""
wind.py
────────────────────────────────────────────────────────────────
Goal-directed (depth-first, backward) proof in a Bayesian network.

Changes compared with the original version
------------------------------------------
* The knowledge base is now *only* a list of rules.
  Each rule is a Horn-clause-like mapping
      head  ←  body  (with attached probability).
* The inference engine is a single recursive function
  `prob(var, value, evidence, depth=0)` that:
      – tries to satisfy the current sub-goal from evidence,
      – otherwise expands the rule whose head matches the sub-goal,
      – sums over all ways the (unbound) body literals can be proven.
* The printed trace is a *goal-directed proof*: you see the call-stack
  with indentation, the rule being fired, and the contribution that
  rule makes to the final probability.
"""

#!/usr/bin/env python3
"""
wind_rules_fixed.py  –  goal-directed proof with exact probabilities
"""

# ──────────────────────────────────────────────────────────────
# 0.  Imports & variables
# ──────────────────────────────────────────────────────────────
from itertools import product
from typing import Dict, Tuple, List

VARS = ['A', 'M', 'W', 'G', 'V', 'F', 'R']          # topological order

# ──────────────────────────────────────────────────────────────
# 1.  Rules  (as before)
# ──────────────────────────────────────────────────────────────
Rule = Tuple[Tuple[str, bool], List[Tuple[str, bool]], float]
rules: List[Rule] = [
    # Priors
    (("A", True), [], 0.4),
    (("M", True), [], 0.3),
    (("W", True), [], 0.5),

    # Gearbox wear
    (("G", True), [("A", True),  ("M", True )], 0.30),
    (("G", True), [("A", True),  ("M", False)], 0.70),
    (("G", True), [("A", False), ("M", True )], 0.05),
    (("G", True), [("A", False), ("M", False)], 0.20),

    # Vibration
    (("V", True), [("W", True),  ("G", True )], 0.90),
    (("V", True), [("W", True),  ("G", False)], 0.50),
    (("V", True), [("W", False), ("G", True )], 0.60),
    (("V", True), [("W", False), ("G", False)], 0.10),

    # Fault
    (("F", True), [("V", True),  ("G", True )], 0.95),
    (("F", True), [("V", True),  ("G", False)], 0.70),
    (("F", True), [("V", False), ("G", True )], 0.80),
    (("F", True), [("V", False), ("G", False)], 0.05),

    # Maintenance decision
    (("R", True), [("F", True)],  0.90),
    (("R", True), [("F", False)], 0.10),
]

# Quick index:  var → list of (body, p_true)
rule_index: Dict[str, List[Tuple[List[Tuple[str, bool]], float]]] = {}
for (head, body, p) in rules:
    rule_index.setdefault(head[0], []).append((body, p))

# ──────────────────────────────────────────────────────────────
# 2.  Evidence
# ──────────────────────────────────────────────────────────────
evidence: Dict[str, bool] = dict(A=True, M=False, W=True)

# ──────────────────────────────────────────────────────────────
# 3.  Exact joint-probability helper (built *from the rules*)
# ──────────────────────────────────────────────────────────────
def joint(world: Dict[str, bool]) -> float:
    """P(world) by chaining through the rule base."""
    p = 1.0
    for var in VARS:
        for body, p_true in rule_index[var]:
            if all(world[b] == v for b, v in body):
                p *= p_true if world[var] else 1.0 - p_true
                break
    return p

# ──────────────────────────────────────────────────────────────
# 4.  Conjunction probability  P(body | current evidence)
# ──────────────────────────────────────────────────────────────
def conj_prob(body: List[Tuple[str, bool]], ev: Dict[str, bool]) -> float:
    """Return exact P(⋀body | ev) by enumerating the (few) remaining vars."""
    unknown = [v for v in VARS if v not in ev]
    num = den = 0.0
    for values in product([False, True], repeat=len(unknown)):
        world = dict(zip(unknown, values), **ev)
        jp = joint(world)
        den += jp
        if all(world[b] == v for b, v in body):
            num += jp
    return num / den

# ──────────────────────────────────────────────────────────────
# 5.  Goal-directed probability with proof trace
# ──────────────────────────────────────────────────────────────
def prob(var: str, val: bool, ev: Dict[str, bool], depth: int = 0) -> float:
    ind = "  " * depth
    if var in ev:                    # observed
        p = 1.0 if ev[var] == val else 0.0
        print(f"{ind}Evidence {var}={ev[var]}  ⇒  P({var}={val})={p}")
        return p

    total = 0.0
    for body, p_true in rule_index[var]:
        p_body = conj_prob(body, ev)
        contrib = (p_true if val else 1.0 - p_true) * p_body
        total += contrib
        print(f"{ind}{var}= {val}  ← {body}  "
              f"[P(body|ev)={p_body:.4f}]  contributes {contrib:.4f}")
    print(f"{ind}⇒ P({var}={val}) = {total:.4f}")
    return total

# ──────────────────────────────────────────────────────────────
# 6.  Posterior of the goal
# ──────────────────────────────────────────────────────────────
def posterior(var: str, ev: Dict[str, bool]) -> float:
    print("\nGoal-directed proof begins:\n")
    p_true  = prob(var, True,  ev, 1)
    p_false = prob(var, False, ev, 1)
    result  = p_true / (p_true + p_false)
    print(f"\nNormalisation: True={p_true:.6f} False={p_false:.6f}"
          f"  ⇒  P({var}=True | evidence) = {result:.4f}")
    return result

# ──────────────────────────────────────────────────────────────
# 7.  Run
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    target = "R"
    post = posterior(target, evidence)

    THRESHOLD = 0.60
    print("\nVerdict:",
          "Maintenance LIKELY required (goal proved)."
          if post > THRESHOLD else
          "Maintenance unlikely (goal NOT proved).")

