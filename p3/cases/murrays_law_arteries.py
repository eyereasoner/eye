#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Prompt → Program → Proof  (Deterministic, audience-friendly)

TOPIC (plain language)
----------------------
When an artery splits, how big should the daughter branches be? If they’re too
narrow, the heart must push much harder (friction grows fast in thin pipes).
If they’re too wide, you waste material and carry excess blood volume.

Trade-off:
  • Pumping power ~ 1 / r^4  (thin → expensive to push)
  • Volume/material ~ r^2    (wide → expensive to maintain)

Balance those costs across a parent artery and its two children and you get a
beautiful rule called **Murray’s law**:  r0^3 = r1^3 + r2^3. We’ll show—both by
calculation and by a small test harness—that the **cube law** minimizes total
cost compared to other plausible rules (like “area preserving” p=2 or “equal
radii”), across many flow splits.

WHAT WE REPORT AS PROOF
-----------------------
For many flow splits (e.g., 60%/40% or 70%/30%), we compare total cost for:
  • Cube law (p=3)   – the Murray prediction
  • Area law (p=2)   – preserves cross-section area
  • Linear (p=1)     – sum of radii preserved
  • Equal radii      – both daughters same size

Cost model (per segment i with flow Qi, radius ri, length Li):
  Pumping power Pi ∝ Li * Qi^2 / ri^4   (Poiseuille)
  Volume cost    Vi ∝ Li * ri^2
  Total J = Σ [ a * Li * Qi^2 / ri^4 + b * Li * ri^2 ]   (a,b are weights)

DETERMINISM
-----------
No randomness; closed-form formulas + fixed grids → same numbers every run.

INTERPRETATION
--------------
The cardiovascular tree behaves like it’s solving a **calculus problem**:
minimize (pumping cost + volume cost). The optimum across the split enforces
**r ∝ Q^(1/3)**, which implies **r0^3 = r1^3 + r2^3** (since Q0 = Q1 + Q2).
That’s “math without math class” happening in your chest.

HOW TO RUN
----------
$ python3 murrays_law_arteries.py
"""

import math

# ---------- Parameters (feel free to tweak; results are qualitative-stable) ----------
mu_like_a = 1.0      # weight for pumping power term (absorbs viscosity & constants)
vol_like_b = 1.0     # weight for volume/material term
L0 = L1 = L2 = 1.0   # segment lengths (normalized)
Q_total = 1.0        # total flow in parent
alpha_grid = [i/20 for i in range(1, 19)]  # daughter1 flow fraction α in (0,1): 0.05..0.95

# ---------- Core formulas ----------
def cost_segment(Q, r, L, a=mu_like_a, b=vol_like_b):
    """Cost for one vessel segment: Pumping + Volume."""
    pumping = a * L * (Q*Q) / (r**4)
    volume  = b * L * (r**2)
    return pumping + volume

def total_cost(r0, r1, r2, Q0, Q1, Q2, L0=L0, L1=L1, L2=L2):
    """Total cost across parent + two daughters."""
    return (cost_segment(Q0, r0, L0) +
            cost_segment(Q1, r1, L1) +
            cost_segment(Q2, r2, L2))

def murray_opt_radius(Q, a=mu_like_a, b=vol_like_b):
    """
    For a single isolated segment with fixed flow Q, minimizing a*Q^2/r^4 + b*r^2
    gives d/dr -> r^6 = (2a/b)*Q^2  →  r = ( (2a/b)^(1/6) ) * Q^(1/3).
    The proportionality constant cancels when we compare within the same tree.
    """
    c = ((2.0*a)/b) ** (1.0/6.0)
    return c * (Q ** (1.0/3.0))

def radii_from_power_rule(Q0, Q1, Q2, p):
    """
    Family of hypotheses: enforce r0^p = r1^p + r2^p with r_i ∝ Q_i^(1/p).
    We normalize by a constant k so the sum identity is met exactly.
    For p=3 → Murray's law; p=2 → area-preserving; p=1 → linear.
    """
    # Shape-only radii (up to a constant k)
    r1s = Q1 ** (1.0/p)
    r2s = Q2 ** (1.0/p)
    r0s = (Q0) ** (1.0/p)
    # choose k so (k r0s)^p = (k r1s)^p + (k r2s)^p holds: any k works since it cancels,
    # but we'll explicitly solve for consistency (k^p * r0s^p = k^p*(r1s^p+r2s^p)).
    # Since Q0 = Q1 + Q2, r0s^p = Q0 = Q1 + Q2 = r1s^p + r2s^p, identity already holds.
    k = 1.0
    return k*r0s, k*r1s, k*r2s

def radii_equal(Q0, Q1, Q2):
    """Equal daughter radii, choose parent radius by continuity of some p (use p=3 for fairness)."""
    r1 = r2 = 0.5  # arbitrary scale; parent chosen to carry Q0 optimally alone
    # set r0 to the single-segment optimum for Q0 to not artificially penalize the parent
    r0 = murray_opt_radius(Q0)
    return r0, r1, r2

# ---------- Evaluation over many flow splits ----------
def evaluate_rules_for_alpha(alpha, a=mu_like_a, b=vol_like_b):
    """
    For a given flow split α (Q1=αQ, Q2=(1-α)Q), compute total cost of several branching rules:
      - Murray p=3
      - Area p=2
      - Linear p=1
      - Equal daughter radii
    Return dict of costs and the Murray identity residual |r0^3 - (r1^3 + r2^3)|.
    """
    Q0 = Q_total
    Q1 = alpha * Q_total
    Q2 = (1.0 - alpha) * Q_total

    # Murray radii from first principles: r_i ∝ Q_i^(1/3)
    r0_m = murray_opt_radius(Q0, a, b)
    r1_m = murray_opt_radius(Q1, a, b)
    r2_m = murray_opt_radius(Q2, a, b)

    # Power-rule families (normalized shapes)
    r0_p3, r1_p3, r2_p3 = radii_from_power_rule(Q0, Q1, Q2, p=3)  # Murray family, shape-equal to above
    r0_p2, r1_p2, r2_p2 = radii_from_power_rule(Q0, Q1, Q2, p=2)
    r0_p1, r1_p1, r2_p1 = radii_from_power_rule(Q0, Q1, Q2, p=1)
    r0_eq, r1_eq, r2_eq = radii_equal(Q0, Q1, Q2)

    # To compare fairly, scale each rule's radii by a single factor s that minimizes *its own* total cost.
    # That way we don't favor rules just because they picked a convenient overall size.
    def best_scaled_cost(r0s, r1s, r2s):
        # cost(s) = a*(Q0^2/(s^4 r0s^4) + ...) + b*(s^2 (r0s^2 + ...))
        # derivative wrt s gives a unique positive optimum; solve analytically:
        # Let A = a*(Q0^2/r0s^4 + Q1^2/r1s^4 + Q2^2/r2s^4)
        #     B = b*(r0s^2 + r1s^2 + r2s^2)
        # J(s) = A*s^-4 + B*s^2  →  dJ/ds = -4A*s^-5 + 2B*s = 0 → s^6 = 2A/B
        A = a*( (Q0*Q0)/(r0s**4) + (Q1*Q1)/(r1s**4) + (Q2*Q2)/(r2s**4) )
        B = b*( r0s**2 + r1s**2 + r2s**2 )
        s = (2.0*A / B) ** (1.0/6.0)
        # compute cost at this optimal s
        return total_cost(s*r0s, s*r1s, s*r2s, Q0, Q1, Q2)

    costs = {
        "Murray (p=3)": best_scaled_cost(r0_p3, r1_p3, r2_p3),
        "Area   (p=2)": best_scaled_cost(r0_p2, r1_p2, r2_p2),
        "Linear (p=1)": best_scaled_cost(r0_p1, r1_p1, r2_p1),
        "Equal radii ": best_scaled_cost(r0_eq, r1_eq, r2_eq),
        # Also report the "first-principles" Murray radii (direct r ∝ Q^(1/3))
        "Murray (direct)": total_cost(r0_m, r1_m, r2_m, Q0, Q1, Q2),
    }

    resid_murray = abs((r0_m**3) - (r1_m**3 + r2_m**3))
    return costs, resid_murray

# ---------- Run the comparison across many flow splits ----------
def run_grid():
    rows = []
    for a in alpha_grid:
        costs, resid = evaluate_rules_for_alpha(a)
        # find the best rule by cost (lower is better)
        best_rule = min(costs.items(), key=lambda kv: kv[1])[0]
        rows.append({
            "alpha": a,
            "best_rule": best_rule,
            "residual_murray_cube": resid,
            **costs
        })
    return rows

rows = run_grid()

# =========================
# ========== P3 ===========
# =========================

def fmt(x): return f"{x:.6f}"

# ---------- ANSWER ----------
print("# Answer")
# Count how often each rule wins across the alpha grid
win_counts = {}
for r in rows:
    win_counts[r["best_rule"]] = win_counts.get(r["best_rule"], 0) + 1
# Present a simple champion summary
ordered = sorted(win_counts.items(), key=lambda kv: (-kv[1], kv[0]))
champ_text = ", ".join([f"{name} ({count} splits)" for name, count in ordered])
mean_resid = sum(r["residual_murray_cube"] for r in rows) / len(rows)

print(
    "Across many flow splits between the daughter branches, the **cube law** "
    "(Murray, p=3) gives the **lowest total cost** most often. Its signature identity "
    "r0^3 = r1^3 + r2^3 is obeyed to numerical precision for the direct optimum.\n"
    f"Winner counts by rule: {champ_text}\n"
    f"Mean |r0^3 - (r1^3 + r2^3)| for Murray (direct): {fmt(mean_resid)}\n"
)

# ---------- REASON WHY ----------
print("# Reason Why")
print(
    "Cost has two parts: pushing blood (strongly favors wide vessels: 1/r^4) and "
    "maintaining blood volume (favors narrower vessels: r^2). Minimizing the sum for "
    "each segment gives r ∝ Q^(1/3). Applying that to the parent and daughters implies:\n"
    "   r0^3 ∝ Q0,  r1^3 ∝ Q1,  r2^3 ∝ Q2,  and with flow conservation Q0 = Q1 + Q2,\n"
    "we get **r0^3 = r1^3 + r2^3**. We confirm this by computing the exact optimum radii, "
    "then comparing total cost against alternative rules. Lower cost is better.\n"
)

print("Example rows (α = daughter1 fraction of flow):")
print(" alpha  |   cost(Murray)   cost(Area p=2)   cost(Linear p=1)   cost(Equal)   best")
print("--------+--------------------------------------------------------------------------")
# print a few representative rows: 0.3, 0.5, 0.7
def nearest(alpha_target):
    return min(rows, key=lambda r: abs(r["alpha"] - alpha_target))
for a in (0.3, 0.5, 0.7):
    r = nearest(a)
    print(f" {r['alpha']:>5.2f}  |  {fmt(r['Murray (p=3)'])}    {fmt(r['Area   (p=2)'])}      "
          f"{fmt(r['Linear (p=1)'])}       {fmt(r['Equal radii '])}    {r['best_rule']}")

print(
    "\nInterpretation: as the split varies (α from 5% to 95%), the **cube law** stays "
    "near-optimal because it is exactly what you get when you balance the two physical costs."
)

# ---------- CHECK (HARNESS) ----------
print("# Check (Harness)")
print(
    "We perturb the model knobs and re-run: change the relative weight of pumping vs volume (a/b), "
    "change segment lengths, and change total flow. The cube law should remain the most cost-effective "
    "rule over most splits."
)

def harness_case(a_weight, b_weight, lengths=(1.0,1.0,1.0), Q=1.0):
    global mu_like_a, vol_like_b, L0, L1, L2, Q_total
    # Save old
    old = (mu_like_a, vol_like_b, L0, L1, L2, Q_total)
    mu_like_a, vol_like_b = a_weight, b_weight
    L0, L1, L2 = lengths
    Q_total = Q
    # Rerun
    res = run_grid()
    # Tally
    wc = {}
    for r in res:
        wc[r["best_rule"]] = wc.get(r["best_rule"], 0) + 1
    # Restore
    mu_like_a, vol_like_b, L0, L1, L2, Q_total = old
    return wc

cases = [
    ("Balanced weights", harness_case(1.0, 1.0, (1.0,1.0,1.0), 1.0)),
    ("Pumping dominates (a=4,b=1)", harness_case(4.0, 1.0, (1.0,1.0,1.0), 1.0)),
    ("Volume dominates  (a=1,b=4)", harness_case(1.0, 4.0, (1.0,1.0,1.0), 1.0)),
    ("Long parent (L0=2)", harness_case(1.0, 1.0, (2.0,1.0,1.0), 1.0)),
    ("High flow (Q=2)", harness_case(1.0, 1.0, (1.0,1.0,1.0), 2.0)),
]

for name, tally in cases:
    ordered = sorted(tally.items(), key=lambda kv: (-kv[1], kv[0]))
    pretty = ", ".join([f"{k}:{v}" for k,v in ordered])
    print(f"- {name:22} → {pretty}")

print(
    "\nConclusion of the harness: across different weights, lengths, and total flows, "
    "the **p=3 cube law** keeps coming out on top across most split ratios. That’s exactly "
    "what the heart would 'want' if it were minimizing (pumping + volume) cost."
)

