#!/usr/bin/env python3
"""
EYE Learning — Program 3: Shopping List & Vendor Split

Short story (header):
  Turn menu quantities into a clear shopping list and suggest where to get what
  (e.g., pizza shop, bakery, grocery).

Overview:
  - Reads './resources/menu_plan.json'.
  - Expands pizzas into large pies (veg vs regular), cupcakes, and drinks.
  - Adds disposables (plates/napkins/cups).
  - Prints Answer/Reason/Check; writes './resources/shopping_list.json' for Program 4.
"""
from __future__ import annotations
import argparse, json
from typing import Dict, Any

### DATA
VENDORS = {
    "pizza_large": "Pizza shop",
    "pizza_large_veg": "Pizza shop",
    "cupcake": "Bakery",
    "juice_box": "Grocery",
    "soft_drink_05l": "Grocery",
    "plates_pack": "Party store",
    "cups_pack": "Party store",
    "napkins_pack": "Party store"
}

### LOGIC
def build_list(menu: Dict[str,Any]) -> Dict[str,Any]:
    items = dict(menu["items"])
    # Add disposables (1 pack per 10 guests, round up)
    total_people = int(round(items.get("cupcake", 0) / 1.10)) or 10
    packs = (total_people + 9) // 10
    items["plates_pack"] = packs
    items["cups_pack"] = packs
    items["napkins_pack"] = packs
    by_vendor = {}
    reasons = []
    for k, q in items.items():
        vendor = VENDORS.get(k, "Grocery")
        by_vendor.setdefault(vendor, {})[k] = q
        reasons.append(f"{k}: qty={q} → {vendor}")
    return {"by_vendor": by_vendor, "reasons": reasons}

### CHECK
def run_harness(lst: Dict[str,Any]) -> None:
    print("Check — at least one vendor and non-negative quantities:")
    assert len(lst["by_vendor"]) >= 1
    for vend, kv in lst["by_vendor"].items():
        assert all(isinstance(q, int) and q >= 0 for q in kv.values())

def main():
    ap = argparse.ArgumentParser(description="Create shopping list & vendor split.")
    ap.add_argument("--infile", default="./resources/menu_plan.json")
    ap.add_argument("--out", default="./resources/shopping_list.json")
    args = ap.parse_args()

    menu = json.load(open(args.infile,"r",encoding="utf-8"))
    lst = build_list(menu)

    print("# ANSWER"); print(json.dumps({"vendors": list(lst["by_vendor"].keys())}, indent=2))
    print("\n# REASONS"); [print("-", r) for r in lst["reasons"]]

    json.dump(lst, open(args.out,"w",encoding="utf-8"), indent=2)
    print(f"\nWrote {args.out}")

    print("\n# CHECK"); run_harness(lst); print("✔ All checks passed.")

if __name__ == "__main__":
    main()

