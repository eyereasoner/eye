#!/usr/bin/env python3
"""
P3 — Program 2: Menu & Budget Planner

Short story (header):
  With headcount and diets known, decide quantities for pizza, cupcakes, and
  drinks, price it, and see if you’re under the host budget.

Sections map & intent:
  ### DATA
      - Simple unit prices
      - Slice assumptions (adults=3, kids=2), pie size=8 slices
  ### LOGIC
      - Compute pizzas (veg share informed by veg/vegan counts)
      - Cupcakes with 10% buffer; drink quantities by cohort
      - Total cost & within-budget flag
  ### CHECK (harness)
      - Integer, non-negative quantities
      - Slices coverage invariant, veg vs total pies sane
      - Cost arithmetic exactness and budget flag correctness
"""

from __future__ import annotations
import argparse, json, math
from typing import Dict, Any

### DATA -----------------------------------------------------------------------
PRICES = {
    "pizza_large": 12.0,        # each (8 slices)
    "pizza_large_veg": 12.5,    # each (8 slices)
    "cupcake": 1.5,             # each
    "juice_box": 0.6,           # each
    "soft_drink_05l": 1.2       # each 0.5L
}
SLICES_PER_PIE = 8
SLICES_PER_ADULT = 3
SLICES_PER_CHILD = 2
CUPCAKE_BUFFER = 0.10

### LOGIC ----------------------------------------------------------------------
def plan_menu(roster: Dict[str, Any]) -> Dict[str, Any]:
    adults = roster["counts"]["adults"]
    kids   = roster["counts"]["kids"]
    total  = roster["counts"]["total"]
    veg_need = roster["diet_counts"].get("veg", 0) + roster["diet_counts"].get("vegan", 0)

    required_slices = adults * SLICES_PER_ADULT + kids * SLICES_PER_CHILD
    total_pies = math.ceil(required_slices / SLICES_PER_PIE)

    # naive veg split: at least 1 veg pie if there is any veg_need; cap at total pies
    # rough proportionality: 1 veg pie per ~8 guests with veg/vegan tag
    veg_pies = 0
    if veg_need > 0:
        veg_pies = max(1, math.ceil(veg_need / max(1, total / SLICES_PER_PIE)))
        veg_pies = min(veg_pies, total_pies)
    reg_pies = total_pies - veg_pies

    cupcakes = math.ceil(total * (1.0 + CUPCAKE_BUFFER))
    juice    = math.ceil(kids * 1.5)  # some seconds
    soft     = math.ceil(adults * 1.0)

    cost = (reg_pies * PRICES["pizza_large"] +
            veg_pies * PRICES["pizza_large_veg"] +
            cupcakes * PRICES["cupcake"] +
            juice * PRICES["juice_box"] +
            soft * PRICES["soft_drink_05l"])
    cost = round(cost, 2)
    within_budget = cost <= roster["event"]["host_budget_eur"]

    reasons = [
        f"Slices: adults={adults}×{SLICES_PER_ADULT} + kids={kids}×{SLICES_PER_CHILD} = {required_slices} → pies={total_pies}.",
        f"Veg pies based on veg/vegan={veg_need}: veg={veg_pies}, reg={reg_pies}.",
        f"Cupcakes: ceil({total}×(1+{CUPCAKE_BUFFER}))={cupcakes}; drinks: juice={juice}, soft={soft}.",
        f"Cost≈€{cost} vs budget €{roster['event']['host_budget_eur']} → within_budget={within_budget}."
    ]
    return {
        "schema": "menu_plan.v1",
        "items": {"pizza_large": reg_pies, "pizza_large_veg": veg_pies, "cupcake": cupcakes,
                  "juice_box": juice, "soft_drink_05l": soft},
        "assumptions": {
            "slices_per_adult": SLICES_PER_ADULT,
            "slices_per_child": SLICES_PER_CHILD,
            "slices_per_pie": SLICES_PER_PIE
        },
        "estimated_cost_eur": cost,
        "within_budget": within_budget,
        "reasons": reasons
    }

### CHECK (harness) ------------------------------------------------------------
def run_harness(roster: Dict[str, Any], plan: Dict[str, Any]) -> None:
    print("Check 1 — Integer non-negative quantities:")
    for k, q in plan["items"].items():
        assert isinstance(q, int) and q >= 0, f"{k} not non-negative int"

    print("Check 2 — Slices coverage invariant:")
    adults = roster["counts"]["adults"]; kids = roster["counts"]["kids"]
    need = adults * SLICES_PER_ADULT + kids * SLICES_PER_CHILD
    have = (plan["items"]["pizza_large"] + plan["items"]["pizza_large_veg"]) * SLICES_PER_PIE
    assert have >= need, f"not enough slices: need {need} have {have}"
    assert plan["items"]["pizza_large_veg"] <= (plan["items"]["pizza_large"] + plan["items"]["pizza_large_veg"])

    print("Check 3 — Cost arithmetic & budget flag consistent:")
    calc = (plan["items"]["pizza_large"] * PRICES["pizza_large"] +
            plan["items"]["pizza_large_veg"] * PRICES["pizza_large_veg"] +
            plan["items"]["cupcake"] * PRICES["cupcake"] +
            plan["items"]["juice_box"] * PRICES["juice_box"] +
            plan["items"]["soft_drink_05l"] * PRICES["soft_drink_05l"])
    calc = round(calc, 2)
    assert calc == plan["estimated_cost_eur"], "cost mismatch"
    assert plan["within_budget"] == (plan["estimated_cost_eur"] <= roster["event"]["host_budget_eur"])

    print("Check 4 — Cupcakes >= headcount and drinks reasonable:")
    assert plan["items"]["cupcake"] >= roster["counts"]["total"]
    assert plan["items"]["juice_box"] >= roster["counts"]["kids"]
    assert plan["items"]["soft_drink_05l"] >= 0

def main():
    ap = argparse.ArgumentParser(description="Menu & Budget Planner.")
    ap.add_argument("--infile", default="./cases/bus/party/guest_roster.json")
    ap.add_argument("--out", default="./cases/bus/party/menu_plan.json")
    args = ap.parse_args()

    roster = json.load(open(args.infile, "r", encoding="utf-8"))
    plan = plan_menu(roster)

    print("# ANSWER")
    print(json.dumps({"estimated_cost_eur": plan["estimated_cost_eur"], "within_budget": plan["within_budget"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in plan["reasons"]:
        print("-", r)

    json.dump(plan, open(args.out, "w", encoding="utf-8"), indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(roster, plan)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

