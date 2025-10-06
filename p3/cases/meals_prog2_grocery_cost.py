
#!/usr/bin/env python3
"""
P3 — Program 2: Grocery List & Cost Estimator

Short story (header):
  With the dinners picked, the family needs a shopping list. The task is to figure
  out what’s missing from the pantry, estimate the cost, and show the math.

Overview:
  Consume ./cases/bus/meal/meal_plan.json, compare with pantry and a simple price list, and generate
  a deduplicated grocery list with totals. Prints Answer/Reason/Check and writes
  ./cases/bus/meal/grocery_list.json for Program 3.

Run:
  python meals_prog2_grocery_cost.py --in ./cases/bus/meal/meal_plan.json --out ./cases/bus/meal/grocery_list.json
"""
from __future__ import annotations

import argparse, json, dataclasses
from typing import Dict, Any

### DATA
# For the demo we re-embed the pantry and a small price list (€/unit). In real life,
# you'd read pantry from storage or carry it forward; keeping it here makes the file self-contained.
PANTRY = {
    "spaghetti": 500, "tomato_sauce": 700, "olive_oil": 100, "basil": 10,
    "rice": 500, "mixed_veg": 0, "soy_sauce": 60, "sesame_oil": 20,
    "tortillas": 0, "chicken_breast": 0, "lettuce": 0, "salsa": 50,
    "mushrooms": 0, "stock": 1200, "parmesan": 30,
    "beef": 0, "broccoli": 0, "lentils": 250, "carrots": 150
}

PRICES_EUR = {
    "spaghetti": 0.003, "tomato_sauce": 0.004, "olive_oil": 0.02, "basil": 0.1,
    "rice": 0.003, "mixed_veg": 0.004, "soy_sauce": 0.01, "sesame_oil": 0.02,
    "tortillas": 0.30, "chicken_breast": 0.012, "lettuce": 0.004, "salsa": 0.01,
    "mushrooms": 0.006, "stock": 0.001, "parmesan": 0.03, "beef": 0.015,
    "broccoli": 0.006, "lentils": 0.004, "carrots": 0.003
}

### LOGIC
def diff_and_cost(required: Dict[str,float]) -> Dict[str,Any]:
    to_buy = {}
    reasons = []
    total = 0.0
    for item, need in required.items():
        have = PANTRY.get(item, 0.0)
        buy = max(0.0, need - have)
        price = PRICES_EUR.get(item, 0.0)
        cost = round(buy * price, 2)
        if buy > 0:
            to_buy[item] = round(buy,2)
            total += cost
            reasons.append(f"{item}: need {need}, have {have} → buy {round(buy,2)} @ €{price}/unit = €{cost}")
        else:
            reasons.append(f"{item}: need {need}, have {have} → buy 0 (covered)")
    return {"to_buy": to_buy, "estimated_total_eur": round(total,2), "reasons": reasons}

### CHECK
def run_harness(required: Dict[str,float], result: Dict[str,Any]) -> None:
    print("Check 1 — Coverage: pantry + purchases meet or exceed required:")
    for item, need in required.items():
        have = PANTRY.get(item,0.0) + result["to_buy"].get(item,0.0)
        ok = have + 1e-9 >= need
        print(f"  - {item}: need={need}, have+buy={have} → ok={ok}")
        assert ok

    print("Check 2 — Total equals sum of line items:")
    recompute = 0.0
    for item, qty in result["to_buy"].items():
        recompute += round(qty * PRICES_EUR.get(item,0.0), 2)
    recompute = round(recompute, 2)
    print(f"  - reported total={result['estimated_total_eur']} vs recomputed={recompute}")
    assert recompute == result["estimated_total_eur"]

def main():
    ap = argparse.ArgumentParser(description="Build grocery list and estimate cost.")
    ap.add_argument("--in", dest="infile", default="./cases/bus/meal/meal_plan.json")
    ap.add_argument("--out", default="./cases/bus/meal/grocery_list.json")
    args = ap.parse_args()

    with open(args.infile,"r",encoding="utf-8") as f:
        plan = json.load(f)

    required = plan["required_ingredients"]
    result = diff_and_cost(required)

    print("# ANSWER")
    print(json.dumps({"to_buy": result["to_buy"], "estimated_total_eur": result["estimated_total_eur"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in result["reasons"]:
        print("-", r)

    with open(args.out,"w",encoding="utf-8") as f:
        json.dump({"required": required, **result}, f, indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(required, result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
