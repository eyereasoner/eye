#!/usr/bin/env python3
"""
wind.py
────────────────────────────────────────────────────────────────
Goal-directed (depth-first, backward) proof in a Bayesian network.

Description
-----------
We model a small BN as a list of conditional rules of the form:

    (HeadVar=True)  ←  [(Parent1=v1), (Parent2=v2), ...]   with probability p

From these we derive:
  • An exact joint P(world) by multiplying conditionals in topological order.
  • A goal-directed posterior:
        P(X=v | ev) = Σ_rules  P(X=v | body) · P(body | ev),
    where P(body | ev) is computed exactly by enumerating the remaining vars.

The “Reason why” section prints an indented, goal-directed proof trace; the
“Answer” summarizes evidence, target, posterior, and verdict; and “Check”
verifies normalization and agreement with a brute-force posterior.
"""

from itertools import product
from typing import Dict, Tuple, List, Iterable

# ──────────────────────────────────────────────────────────────
# 0) Variables (topological order) and rule base
# ──────────────────────────────────────────────────────────────

VARS = ['A', 'M', 'W', 'G', 'V', 'F', 'R']  # A:Age, M:Maintenance, W:Wind, etc.

# A rule: ((head_var, True), body_literals, p_true)
Rule = Tuple[Tuple[str, bool], List[Tuple[str, bool]], float]

rules: List[Rule] = [
    # Priors
    (("A", True), [], 0.4),
    (("M", True), [], 0.3),
    (("W", True), [], 0.5),

    # Gearbox wear G depends on A and M
    (("G", True), [("A", True),  ("M", True )], 0.30),
    (("G", True), [("A", True),  ("M", False)], 0.70),
    (("G", True), [("A", False), ("M", True )], 0.05),
    (("G", True), [("A", False), ("M", False)], 0.20),

    # Vibration V depends on W and G
    (("V", True), [("W", True),  ("G", True )], 0.90),
    (("V", True), [("W", True),  ("G", False)], 0.50),
    (("V", True), [("W", False), ("G", True )], 0.60),
    (("V", True), [("W", False), ("G", False)], 0.10),

    # Fault F depends on V and G
    (("F", True), [("V", True),  ("G", True )], 0.95),
    (("F", True), [("V", True),  ("G", False)], 0.70),
    (("F", True), [("V", False), ("G", True )], 0.80),
    (("F", True), [("V", False), ("G", False)], 0.05),

    # Maintenance decision R depends on F
    (("R", True), [("F", True)],  0.90),
    (("R", True), [("F", False)], 0.10),
]

# Index rules by head variable
rule_index: Dict[str, List[Tuple[List[Tuple[str, bool]], float]]] = {}
for (head, body, p) in rules:
    rule_index.setdefault(head[0], []).append((body, p))

# Evidence example (observations)
evidence: Dict[str, bool] = dict(A=True, M=False, W=True)

# ──────────────────────────────────────────────────────────────
# 1) Exact joint from the rules
# ──────────────────────────────────────────────────────────────
def joint(world: Dict[str, bool]) -> float:
    """P(world) by chaining through the rule base in topological order."""
    p = 1.0
    for var in VARS:
        # find the unique body that matches this world's parents assignment
        for body, p_true in rule_index[var]:
            if all(world[b] == v for b, v in body):
                p *= p_true if world[var] else (1.0 - p_true)
                break
    return p

# ──────────────────────────────────────────────────────────────
# 2) Condition conjunction probability  P(∧body | ev)
# ──────────────────────────────────────────────────────────────
def worlds_consistent_with(ev: Dict[str, bool]) -> Iterable[Dict[str, bool]]:
    unknown = [v for v in VARS if v not in ev]
    for values in product([False, True], repeat=len(unknown)):
        yield dict(zip(unknown, values), **ev)

def conj_prob(body: List[Tuple[str, bool]], ev: Dict[str, bool]) -> float:
    """Exact P(∧body | ev) by enumeration."""
    num = den = 0.0
    for world in worlds_consistent_with(ev):
        jp = joint(world)
        den += jp
        if all(world[b] == v for b, v in body):
            num += jp
    return 0.0 if den == 0.0 else num / den

# ──────────────────────────────────────────────────────────────
# 3) Goal-directed probability with optional proof trace
# ──────────────────────────────────────────────────────────────
def fmt_body(body: List[Tuple[str, bool]]) -> str:
    return "[" + ", ".join(f"{v}={val}" for v, val in body) + "]"

def prob(var: str, val: bool, ev: Dict[str, bool], depth: int = 0,
         log: List[str] | None = None) -> float:
    """
    P(var=val | ev) = Σ_bodies  P(var=val | body) * P(body | ev)
    If `log` is given, append readable proof lines with indentation.
    """
    ind = "  " * depth
    if var in ev:
        p = 1.0 if ev[var] == val else 0.0
        if log is not None:
            log.append(f"{ind}Evidence {var}={ev[var]}  ⇒  P({var}={val})={p:.4f}")
        return p

    total = 0.0
    for body, p_true in rule_index[var]:
        p_body = conj_prob(body, ev)
        contrib = (p_true if val else (1.0 - p_true)) * p_body
        total += contrib
        if log is not None:
            log.append(f"{ind}{var}={val:<5} ← {fmt_body(body):<20} "
                       f"[P(body|ev)={p_body:.4f}]  contributes {contrib:.4f}")
    if log is not None:
        log.append(f"{ind}⇒ P({var}={val}) = {total:.4f}")
    return total

def posterior(var: str, ev: Dict[str, bool], log: List[str] | None = None) -> float:
    if log is not None:
        log.append("Goal-directed proof begins:\n")
    p_true  = prob(var, True,  ev, 1, log)
    p_false = prob(var, False, ev, 1, log)
    denom = p_true + p_false
    result = 0.0 if denom == 0.0 else p_true / denom
    if log is not None:
        log.append(f"\nNormalisation: True={p_true:.6f}  False={p_false:.6f}"
                   f"  ⇒  P({var}=True | evidence) = {result:.4f}")
    return result

# ──────────────────────────────────────────────────────────────
# 4) Brute-force posterior for cross-checks
# ──────────────────────────────────────────────────────────────
def brute_posterior(var: str, ev: Dict[str, bool]) -> float:
    num = den = 0.0
    for world in worlds_consistent_with(ev):
        jp = joint(world)
        den += jp
        if world[var]:
            num += jp
    return 0.0 if den == 0.0 else num / den

# ──────────────────────────────────────────────────────────────
# 5) ARC sections
# ──────────────────────────────────────────────────────────────
def arc_answer(target: str, ev: Dict[str, bool], threshold: float, post: float) -> None:
    print("Answer")
    print("------")
    print("Evidence:")
    for k in ('A','M','W','G','V','F','R'):
        if k in ev:
            print(f"  {k} = {ev[k]}")
    print(f"\nTarget:   P({target}=True | evidence)")
    print(f"Posterior: {post:.4f}")
    print("Verdict:  " + ("Maintenance LIKELY required (goal proved)."
                          if post > threshold else
                          "Maintenance unlikely (goal NOT proved)."))
    print()

def arc_reason(target: str, ev: Dict[str, bool]) -> None:
    print("Reason why")
    print("----------")
    log: List[str] = []
    _ = posterior(target, ev, log)
    for line in log:
        print(line)
    print()

def arc_check(target: str, ev: Dict[str, bool]) -> None:
    print("Check (harness)")
    print("---------------")
    eps = 1e-9

    # 1) Joint normalization over all 2^|VARS| worlds
    total = 0.0
    for values in product([False, True], repeat=len(VARS)):
        world = dict(zip(VARS, values))
        total += joint(world)
    assert abs(total - 1.0) < 1e-9, f"Joint does not normalize to 1 (got {total})"

    # 2) Goal-directed vs brute-force posterior
    gd = posterior(target, ev)
    bf = brute_posterior(target, ev)
    assert abs(gd - bf) < 1e-9, f"Posterior mismatch: goal-directed {gd} vs brute {bf}"

    # 3) For each variable, the rule bodies form a partition (given ev):
    #    Σ_bodies P(body|ev) ≈ 1
    for var in VARS:
        s = sum(conj_prob(body, ev) for body, _ in rule_index[var])
        assert abs(s - 1.0) < 1e-9, f"Bodies for {var} not exhaustive under evidence (sum={s})"

    # 4) Sanity checks of obvious conditionals via enumeration
    #    R depends only on F
    for v in (True, False):
        evF = dict(ev, F=v)
        pr = posterior("R", evF)
        expected = 0.9 if v else 0.1
        assert abs(pr - expected) < 1e-9, f"P(R=True|F={v})={pr} != {expected}"

    #    Check one row for V and G conditionals under neutral evidence
    neutral_ev: Dict[str,bool] = {}
    # V | W, G
    for w in (False, True):
        for g in (False, True):
            ev2 = dict(neutral_ev, W=w, G=g)
            pv = posterior("V", ev2)
            table = {
                (True,  True): 0.90,
                (True,  False):0.50,
                (False, True): 0.60,
                (False, False):0.10,
            }
            assert abs(pv - table[(w,g)]) < 1e-9, f"P(V|W={w},G={g}) wrong"

    print("OK: joint normalization, posterior agreement, body partition, and key conditionals verified.\n")

# ──────────────────────────────────────────────────────────────
# 6) Main
# ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    target = "R"
    THRESHOLD = 0.60

    post = posterior(target, evidence)  # silent compute for Answer
    arc_answer(target, evidence, THRESHOLD, post)
    arc_reason(target, evidence)
    arc_check(target, evidence)

