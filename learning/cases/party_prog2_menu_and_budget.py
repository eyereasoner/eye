#!/usr/bin/env python3
"""
EYE Learning — Program 2: Menu & Budget Planner

Short story (header):
  With headcount and diets known, decide quantities for pizza, cupcakes, and
  drinks, price it, and see if you’re under the host budget.

Overview:
  - Reads './resources/guest_roster.json'.
  - Adults → 3 pizza slices each; kids → 2 slices each.
  - Large pizza = 8 slices; veg/vegan preference drives veg pizzas share.
  - 1 cupcake per person (+10% buffer), juice for kids, soft drinks for adults.
  - Prints Answer/Reason/Check; writes './resources/menu_plan.json' for Program 3.
"""
from __future__ import annotations
import argparse, json, math
from typing import Dict, Any

### DATA (unit prices, simple demo)
PRICES = {
    "pizza_large": 12.0,    # each (8 slices)
    "pizza_large_veg": 12.5,# each (8 slices)
    "cupcake": 1.5,         # each
    "juice_box": 0.6,       # each
    "soft_drink_05l": 1.2   # each 0.5L
}

### LOGIC
def plan_menu(roster: Dict[str,Any]) -> Dict[str,Any]:
    adults = roster["counts"]["adults"]; kids = roster["counts"]["kids"]; total = roster["counts"]["total"]
    veg_need = roster["diet_counts"].get("veg",0) + roster["diet_counts"].get("vegan",0)
    slices = adults*3 + kids*2
    pizzas_total = math.ceil(slices / 8.0)
    veg_pizzas = min(pizzas_total, max(1, math.ceil(veg_need / max(1, total/8)))) if veg_need>0 else 0
    reg_pizzas = pizzas_total - veg_pizzas
    cupcakes = math.ceil(total * 1.10)
    juice = math.ceil(kids * 1.5)  # some kids want seconds
    soft = math.ceil(adults * 1.0) # 0.5L each

    cost = (reg_pizzas*PRICES["pizza_large"] + veg_pizzas*PRICES["pizza_large_veg"] +
            cupcakes*PRICES["cupcake"] + juice*PRICES["juice_box"] + soft*PRICES["soft_drink_05l"])
    within_budget = cost <= roster["event"]["host_budget_eur"]
    reasons = [
        f"Slices: adults={adults}×3 + kids={kids}×2 = {slices} → pizzas={pizzas_total} (veg={veg_pizzas}, reg={reg_pizzas}).",
        f"Cupcakes: 1×{total} + 10% = {cupcakes}.",
        f"Drinks: juice={juice}, soft drinks={soft}.",
        f"Cost≈€{round(cost,2)} vs budget €{roster['event']['host_budget_eur']} → within_budget={within_budget}."
    ]
    return {
        "items": {"pizza_large": reg_pizzas, "pizza_large_veg": veg_pizzas, "cupcake": cupcakes,
                  "juice_box": juice, "soft_drink_05l": soft},
        "estimated_cost_eur": round(cost,2),
        "within_budget": within_budget,
        "reasons": reasons
    }

### CHECK
def run_harness(roster: Dict[str,Any], plan: Dict[str,Any]) -> None:
    print("Check 1 — Non-negative integer quantities:")
    assert all(isinstance(q,int) and q>=0 for q in plan["items"].values())
    print("Check 2 — Cupcakes at least headcount:")
    total = roster["counts"]["total"]; assert plan["items"]["cupcake"] >= total

def main():
    ap = argparse.ArgumentParser(description="Menu & Budget Planner.")
    ap.add_argument("--infile", default="./resources/guest_roster.json")
    ap.add_argument("--out", default="./resources/menu_plan.json")
    args = ap.parse_args()

    roster = json.load(open(args.infile,"r",encoding="utf-8"))
    plan = plan_menu(roster)

    print("# ANSWER"); print(json.dumps({"estimated_cost_eur": plan["estimated_cost_eur"], "within_budget": plan["within_budget"]}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in plan["reasons"]]

    json.dump(plan, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(roster, plan); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

