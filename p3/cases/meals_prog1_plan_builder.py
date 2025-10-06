
#!/usr/bin/env python3
"""
P3 — Program 1: Weekly Dinner Plan Builder

Short story (header):
  A family wants five simple dinners this week. They’d like to use what’s in the pantry,
  avoid mushrooms, and keep two vegetarian nights. The task is to pick the meals and
  explain why each one fits.

Overview:
  Build a 5‑dinner plan from a small recipe set, honoring constraints and using pantry items.
  Prints Answer/Reason/Check and writes ./bus/meal/meal_plan.json for Program 2.

Run:
  python meals_prog1_plan_builder.py --out ./bus/meal/meal_plan.json --asof 2025-09-18
"""
from __future__ import annotations

import argparse, json, dataclasses, datetime as dt
from typing import List, Dict, Any, Set

### DATA
@dataclasses.dataclass(frozen=True)
class Recipe:
    id: str
    name: str
    vegetarian: bool
    ingredients: Dict[str, float]       # name → units
    dislikes_flag: bool                 # True if contains a disliked ingredient (e.g., mushrooms)
    nutrition: Dict[str, float]         # per serving: kcal, protein_g, carbs_g, fat_g
    servings: int

RECIPES: List[Recipe] = [
    Recipe("r1","Tomato Basil Pasta", True,
           {"spaghetti": 400, "tomato_sauce": 500, "olive_oil": 20, "basil": 5},
           False, {"kcal": 520, "protein_g": 16, "carbs_g": 88, "fat_g": 14}, servings=4),
    Recipe("r2","Veggie Stir‑Fry", True,
           {"rice": 300, "mixed_veg": 400, "soy_sauce": 30, "sesame_oil": 15},
           False, {"kcal": 480, "protein_g": 14, "carbs_g": 80, "fat_g": 12}, servings=4),
    Recipe("r3","Chicken Tacos", False,
           {"tortillas": 8, "chicken_breast": 600, "lettuce": 150, "salsa": 100},
           False, {"kcal": 600, "protein_g": 35, "carbs_g": 65, "fat_g": 20}, servings=4),
    Recipe("r4","Mushroom Risotto", True,
           {"rice": 300, "mushrooms": 300, "stock": 800, "parmesan": 60},
           True, {"kcal": 570, "protein_g": 15, "carbs_g": 85, "fat_g": 16}, servings=4),
    Recipe("r5","Beef & Broccoli", False,
           {"beef": 500, "broccoli": 350, "rice": 300, "soy_sauce": 30},
           False, {"kcal": 650, "protein_g": 40, "carbs_g": 60, "fat_g": 28}, servings=4),
    Recipe("r6","Lentil Soup", True,
           {"lentils": 300, "carrots": 200, "stock": 1000, "olive_oil": 20},
           False, {"kcal": 430, "protein_g": 22, "carbs_g": 60, "fat_g": 10}, servings=4),
]

PANTRY: Dict[str, float] = {
    "spaghetti": 500, "tomato_sauce": 700, "olive_oil": 100, "basil": 10,
    "rice": 500, "mixed_veg": 0, "soy_sauce": 60, "sesame_oil": 20,
    "tortillas": 0, "chicken_breast": 0, "lettuce": 0, "salsa": 50,
    "mushrooms": 0, "stock": 1200, "parmesan": 30,
    "beef": 0, "broccoli": 0,
    "lentils": 250, "carrots": 150
}

CONSTRAINTS = {
    "n_dinners": 5,
    "vegetarian_nights": 2,
    "avoid_dislikes": True,   # avoid recipes where dislikes_flag=True
}

### LOGIC
def score_pantry_use(recipe: Recipe, pantry: Dict[str,float]) -> float:
    have = 0.0; need = 0.0
    for item, qty in recipe.ingredients.items():
        need += max(qty, 0.0)
        have += min(pantry.get(item,0.0), qty)
    return have / need if need > 0 else 0.0

def pick_meals(asof: dt.date) -> Dict[str, Any]:
    # Filter by dislikes
    candidates = [r for r in RECIPES if not (CONSTRAINTS["avoid_dislikes"] and r.dislikes_flag)]
    # Sort by pantry-use score (desc), prefer vegetarian to satisfy nights, then by id
    ranked = sorted(candidates, key=lambda r: (score_pantry_use(r,PANTRY), r.vegetarian, -int(r.id[1:])), reverse=True)

    plan: List[Recipe] = []
    veg_count = 0
    for r in ranked:
        if len(plan) >= CONSTRAINTS["n_dinners"]:
            break
        # try to satisfy vegetarian nights
        if r.vegetarian and veg_count < CONSTRAINTS["vegetarian_nights"]:
            plan.append(r); veg_count += 1; continue
        # otherwise fill remaining slots by best pantry use
        if not r.vegetarian or veg_count >= CONSTRAINTS["vegetarian_nights"]:
            plan.append(r)

    # If vegetarian nights undersatisfied, backfill from remaining veg
    if veg_count < CONSTRAINTS["vegetarian_nights"]:
        remaining_veg = [r for r in candidates if r.vegetarian and r not in plan]
        for r in remaining_veg:
            if veg_count >= CONSTRAINTS["vegetarian_nights"]:
                break
            # swap out the worst non-veg if necessary
            nonveg = [p for p in plan if not p.vegetarian]
            if not nonveg: break
            worst_nonveg = nonveg[-1]
            plan.remove(worst_nonveg); plan.append(r); veg_count += 1

    # Build reasons and required ingredients
    reasons = []
    required: Dict[str,float] = {}
    for r in plan:
        pantry_score = round(100*score_pantry_use(r,PANTRY))
        reason = (f"{r.name}: uses {pantry_score}% pantry items; "
                  f"{'vegetarian' if r.vegetarian else 'non‑vegetarian'}; "
                  f"dislikes_ok={not r.dislikes_flag}.")
        reasons.append(reason)
        for item, qty in r.ingredients.items():
            required[item] = required.get(item,0.0) + qty

    return {
        "reference_date": asof.isoformat(),
        "constraints": CONSTRAINTS,
        "selected": [{"id": r.id, "name": r.name, "vegetarian": r.vegetarian, "servings": r.servings} for r in plan],
        "required_ingredients": {k: round(v,2) for k,v in sorted(required.items())},
        "reasons": reasons,
        "recipes_nutrition": {r.id: {"per_serving": r.nutrition, "servings": r.servings} for r in plan}
    }

### CHECK
def run_harness(result: Dict[str,Any]) -> None:
    print("Check 1 — Count & vegetarian nights:")
    n = len(result["selected"]); veg = sum(1 for s in result["selected"] if s["vegetarian"])
    print(f"  - dinners={n}, vegetarian_nights={veg}")
    assert n == CONSTRAINTS["n_dinners"]
    assert veg >= CONSTRAINTS["vegetarian_nights"]

    print("Check 2 — Dislikes avoided:")
    disliked_ids = {r.id for r in RECIPES if r.dislikes_flag}
    selected_ids = {s["id"] for s in result["selected"]}
    print(f"  - disliked in selection? {bool(disliked_ids & selected_ids)}")
    assert not (disliked_ids & selected_ids)

    print("Check 3 — Required ingredients computed consistently:")
    # recompute and compare
    recompute = {}
    for s in result["selected"]:
        r = next(r for r in RECIPES if r.id == s["id"])
        for item, qty in r.ingredients.items():
            recompute[item] = recompute.get(item,0.0)+qty
    assert {k: round(v,2) for k,v in sorted(recompute.items())} == result["required_ingredients"]

def main():
    ap = argparse.ArgumentParser(description="Build a 5‑dinner weekly plan.")
    ap.add_argument("--out", default="./bus/meal/meal_plan.json")
    ap.add_argument("--asof", default="2025-09-18")
    args = ap.parse_args()

    asof = dt.date.fromisoformat(args.asof)
    result = pick_meals(asof)

    print("# ANSWER")
    print(json.dumps({"selected": result["selected"]}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in result["reasons"]:
        print("-", r)

    with open(args.out,"w",encoding="utf-8") as f:
        json.dump(result,f,indent=2,ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(result)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()
