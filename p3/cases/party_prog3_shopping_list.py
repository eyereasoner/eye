#!/usr/bin/env python3
"""
P3 — Program 3: Shopping List & Vendor Split

Short story (header):
  Turn menu quantities into a clear shopping list and suggest where to get what
  (pizza shop, bakery, grocery, party store). Add disposables.

Sections map & intent:
  ### DATA
      - Vendor mapping
      - Disposable pack sizing rule (1 pack per 10 people, ceil)
  ### LOGIC
      - Expand menu items + add disposables
      - Group by vendor
  ### CHECK (harness)
      - Non-negative integers everywhere
      - Vendor coverage for every item
      - by_vendor aggregation sums back to item totals; pack math correct
"""

from __future__ import annotations
import argparse, json, math
from typing import Dict, Any

### DATA -----------------------------------------------------------------------
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
PACK_SIZE = 10  # disposables pack serves ~10 people

### LOGIC ----------------------------------------------------------------------
def build_list(menu: Dict[str, Any]) -> Dict[str, Any]:
    items = dict(menu["items"])  # shallow copy
    # Derive headcount from cupcakes (we ensured cupcakes >= headcount)
    approx_total = max(1, round(items.get("cupcake", 0) / 1.10))
    packs = math.ceil(approx_total / PACK_SIZE)
    items["plates_pack"] = packs
    items["cups_pack"] = packs
    items["napkins_pack"] = packs

    by_vendor = {}
    reasons = []
    for k, q in items.items():
        vendor = VENDORS.get(k)
        reasons.append(f"{k}: qty={q} → {vendor}")
        by_vendor.setdefault(vendor, {})[k] = q

    return {
        "schema": "shopping_list.v1",
        "items": items,
        "by_vendor": by_vendor,
        "assumptions": {"pack_size": PACK_SIZE},
        "reasons": reasons
    }

### CHECK (harness) ------------------------------------------------------------
def run_harness(menu: Dict[str, Any], lst: Dict[str, Any]) -> None:
    print("Check 1 — All item quantities are non-negative integers:")
    for k, q in lst["items"].items():
        assert isinstance(q, int) and q >= 0, f"{k} bad quantity"

    print("Check 2 — Vendor mapping exists for all items:")
    for k in lst["items"].keys():
        assert k in VENDORS, f"missing vendor for {k}"
        assert VENDORS[k] in lst["by_vendor"], f"vendor group missing for {k}"

    print("Check 3 — by_vendor sums equal flat item totals:")
    # Re-aggregate from by_vendor
    flat = {}
    for vend, kv in lst["by_vendor"].items():
        for k, q in kv.items():
            flat[k] = flat.get(k, 0) + q
    assert flat == lst["items"], "aggregation mismatch"

    print("Check 4 — Disposable packs math sane:")
    total = menu["items"]["cupcake"]  # ≥ headcount
    approx_total = max(1, round(total / 1.10))
    expected_packs = math.ceil(approx_total / PACK_SIZE)
    for dn in ("plates_pack", "cups_pack", "napkins_pack"):
        assert lst["items"][dn] == expected_packs, f"{dn} pack count off"

def main():
    ap = argparse.ArgumentParser(description="Create shopping list & vendor split.")
    ap.add_argument("--infile", default="./bus/party/menu_plan.json")
    ap.add_argument("--out", default="./bus/party/shopping_list.json")
    args = ap.parse_args()

    menu = json.load(open(args.infile, "r", encoding="utf-8"))
    lst = build_list(menu)

    print("# ANSWER")
    print(json.dumps({"vendors": list(lst["by_vendor"].keys())}, indent=2, ensure_ascii=False))
    print("\n# REASONS")
    for r in lst["reasons"]:
        print("-", r)

    json.dump(lst, open(args.out, "w", encoding="utf-8"), indent=2, ensure_ascii=False)
    print(f"\nWrote {args.out}")

    print("\n# CHECK (harness) — detailed")
    run_harness(menu, lst)
    print("✔ All checks passed.")

if __name__ == "__main__":
    main()

