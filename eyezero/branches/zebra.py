#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Zebra (Einstein's Riddle) — ARC (Answer / Reason / Check), self-contained.

Model
  Five houses in a row, positions 1..5 (left→right). Each house has exactly one of each:
    • Color:        red, green, ivory, yellow, blue
    • Nationality:  Englishman, Spaniard, Ukrainian, Norwegian, Japanese
    • Beverage:     coffee, tea, milk, orange juice, water
    • Cigar:        Old Gold, Kools, Chesterfields, Lucky Strike, Parliaments
    • Pet:          dog, snails, fox, horse, zebra

Clues (classic formulation)
  1.  The Englishman lives in the red house.
  2.  The Spaniard owns the dog.
  3.  Coffee is drunk in the green house.
  4.  The Ukrainian drinks tea.
  5.  The green house is immediately to the right of the ivory house.
  6.  The Old Gold smoker owns snails.
  7.  Kools are smoked in the yellow house.
  8.  Milk is drunk in the middle house.
  9.  The Norwegian lives in the first house.
  10. The man who smokes Chesterfields lives in the house next to the man with the fox.
  11. Kools are smoked in the house next to the house where the horse is kept.
  12. The Lucky Strike smoker drinks orange juice.
  13. The Japanese smokes Parliaments.
  14. The Norwegian lives next to the blue house.
  15. Question: Who drinks water? Who owns the zebra?

Approach
  Backtracking over permutations with strong early pruning (color, nationality, beverage
  constraints first), then cigar/pet layers and adjacency checks.

Output
  • Answer — full house table (positions 1..5) and the specific “water” & “zebra” owners.
  • Reason why — short explanation of constraints & method.
  • Check (harness) — revalidates all clues against the found solution and uniqueness.
"""

from __future__ import annotations
from itertools import permutations
from typing import Dict, List, Tuple

# Canonical attribute orders (for presentation only)
COLORS        = ["red", "green", "ivory", "yellow", "blue"]
NATIONS       = ["Englishman", "Spaniard", "Ukrainian", "Norwegian", "Japanese"]
DRINKS        = ["coffee", "tea", "milk", "orange juice", "water"]
CIGARS        = ["Old Gold", "Kools", "Chesterfields", "Lucky Strike", "Parliaments"]
PETS          = ["dog", "snails", "fox", "horse", "zebra"]

HOUSES = [1, 2, 3, 4, 5]  # positions left→right

def idx_of(name_list: List[str], layout: Tuple[str, ...], value: str) -> int:
    """Return 1-based house index of value inside a layout tuple."""
    return layout.index(value) + 1

def solve_zebra(max_solutions: int = 3):
    """Return a list of solutions. Each solution is a dict: {aspect_name: tuple of length 5}."""
    sols = []

    # Colors first — enforce “green immediately right of ivory”
    for colors in permutations(COLORS):
        pos_green = colors.index("green")
        pos_ivory = colors.index("ivory")
        if pos_green != pos_ivory + 1:
            continue  # clue 5

        # Nationalities — enforce Englishman=red, Norwegian=first, and Norwegian next to blue
        for nations in permutations(NATIONS):
            if nations.index("Norwegian") != 0:             # clue 9 (house 1 is index 0)
                continue
            if nations.index("Englishman") != colors.index("red"):  # clue 1
                continue
            # clue 14: Norwegian next to blue
            if abs(nations.index("Norwegian") - colors.index("blue")) != 1:
                continue

            # Drinks — enforce coffee=green, tea=Ukrainian, milk=middle
            for drinks in permutations(DRINKS):
                if drinks.index("coffee") != colors.index("green"):     # clue 3
                    continue
                if drinks.index("tea") != nations.index("Ukrainian"):   # clue 4
                    continue
                if drinks.index("milk") != 2:                            # clue 8 (middle = pos 3 = index 2)
                    continue

                # Cigars — enforce Kools=yellow, LuckyStrike=OJ, Japanese=Parliaments
                for cigars in permutations(CIGARS):
                    if cigars.index("Kools") != colors.index("yellow"):             # clue 7
                        continue
                    if cigars.index("Lucky Strike") != drinks.index("orange juice"): # clue 12
                        continue
                    if cigars.index("Parliaments") != nations.index("Japanese"):    # clue 13
                        continue

                    # Pets — enforce Spaniard=dog, OldGold=snails
                    for pets in permutations(PETS):
                        if pets.index("dog") != nations.index("Spaniard"):     # clue 2
                            continue
                        if pets.index("snails") != cigars.index("Old Gold"):   # clue 6
                            continue

                        # Adjacency: Chesterfields next to fox; Kools next to horse
                        if abs(cigars.index("Chesterfields") - pets.index("fox")) != 1:  # clue 10
                            continue
                        if abs(cigars.index("Kools") - pets.index("horse")) != 1:        # clue 11
                            continue

                        # All constraints satisfied
                        sol = {
                            "colors": colors,
                            "nations": nations,
                            "drinks": drinks,
                            "cigars": cigars,
                            "pets": pets,
                        }
                        sols.append(sol)
                        if len(sols) >= max_solutions:
                            return sols
    return sols

# ───────────────────────────────── ARC: Answer ─────────────────────────────────

def print_answer() -> None:
    print("Answer")
    print("======")

    solutions = solve_zebra(max_solutions=5)
    if not solutions:
        print("No solution found.")
        return
    if len(solutions) > 1:
        print(f"(Multiple solutions found: showing the first of {len(solutions)})\n")

    s = solutions[0]
    colors, nations, drinks, cigars, pets = (s["colors"], s["nations"], s["drinks"], s["cigars"], s["pets"])

    # Who drinks water? Who owns the zebra?
    water_pos = drinks.index("water") + 1
    zebra_pos = pets.index("zebra") + 1
    water_owner = nations[water_pos - 1]
    zebra_owner = nations[zebra_pos - 1]

    # Print full table (house 1..5)
    print("Houses (left → right):")
    header = f"{'House':<7} {'Color':<10} {'Nation':<12} {'Drink':<14} {'Cigar':<15} {'Pet':<8}"
    print(header)
    print("-" * len(header))
    for i in range(5):
        print(f"{i+1:<7} {colors[i]:<10} {nations[i]:<12} {drinks[i]:<14} {cigars[i]:<15} {pets[i]:<8}")

    print("\nKey facts")
    print("---------")
    print(f"Water is drunk by the {water_owner} (house {water_pos}).")
    print(f"The zebra is owned by the {zebra_owner} (house {zebra_pos}).")

# ───────────────────────────────── ARC: Reason why ────────────────────────────────

def print_reason() -> None:
    print("\nReason why")
    print("==========")
    print("We model each attribute (color, nation, drink, cigar, pet) as a permutation of 5 values,")
    print("mapping to house positions 1..5. We then enforce the 14 clues:")
    print("  • direct equalities (e.g., Englishman ↔ red, coffee ↔ green, etc.),")
    print("  • position constraints (green is immediately to the right of ivory; milk is in the middle;")
    print("    Norwegian at the first house),")
    print("  • adjacency constraints (Chesterfields next to the fox; Kools next to the horse;")
    print("    Norwegian next to the blue house).")
    print("Backtracking with early pruning guarantees we explore only consistent partial assignments,")
    print("yielding the unique classic solution (water drinker and zebra owner as shown).")

# ─────────────────────────────── ARC: Check (harness) ──────────────────────────────

def check_solution(sol: Dict[str, Tuple[str, ...]]) -> bool:
    """Re-validate all clues against a given solution."""
    colors, nations, drinks, cigars, pets = (sol["colors"], sol["nations"], sol["drinks"], sol["cigars"], sol["pets"])

    def pos(layout, value): return layout.index(value)

    # 1 Englishman=red
    if pos(nations, "Englishman") != pos(colors, "red"): return False
    # 2 Spaniard=dog
    if pos(nations, "Spaniard") != pos(pets, "dog"): return False
    # 3 coffee=green
    if pos(drinks, "coffee") != pos(colors, "green"): return False
    # 4 Ukrainian=tea
    if pos(nations, "Ukrainian") != pos(drinks, "tea"): return False
    # 5 green right of ivory (immediately)
    if pos(colors, "green") != pos(colors, "ivory") + 1: return False
    # 6 Old Gold=snails
    if pos(cigars, "Old Gold") != pos(pets, "snails"): return False
    # 7 Kools=yellow
    if pos(cigars, "Kools") != pos(colors, "yellow"): return False
    # 8 milk=middle (index 2)
    if pos(drinks, "milk") != 2: return False
    # 9 Norwegian=first (index 0)
    if pos(nations, "Norwegian") != 0: return False
    # 10 Chesterfields next to fox
    if abs(pos(cigars, "Chesterfields") - pos(pets, "fox")) != 1: return False
    # 11 Kools next to horse
    if abs(pos(cigars, "Kools") - pos(pets, "horse")) != 1: return False
    # 12 Lucky Strike=orange juice
    if pos(cigars, "Lucky Strike") != pos(drinks, "orange juice"): return False
    # 13 Japanese=Parliaments
    if pos(nations, "Japanese") != pos(cigars, "Parliaments"): return False
    # 14 Norwegian next to blue
    if abs(pos(nations, "Norwegian") - pos(colors, "blue")) != 1: return False

    # All good
    return True

def print_check() -> None:
    print("\nCheck (harness)")
    print("===============")
    ok_all = True

    sols = solve_zebra(max_solutions=10)

    # A) At least one solution & all solutions satisfy all clues
    any_found = len(sols) > 0
    print(f"Any solution found? {any_found}")
    ok_all &= any_found

    all_valid = all(check_solution(s) for s in sols)
    print(f"All found solutions satisfy all 14 clues? {all_valid}")
    ok_all &= all_valid

    # B) Uniqueness (classic puzzle has exactly one solution)
    unique = (len(sols) == 1)
    print(f"Unique solution? {unique} (found {len(sols)})")
    ok_all &= unique

    # C) Derived answers are consistent
    if sols:
        s = sols[0]
        water_owner = s["nations"][s["drinks"].index("water")]
        zebra_owner = s["nations"][s["pets"].index("zebra")]
        consistent = (water_owner in NATIONS) and (zebra_owner in NATIONS)
        print(f"Derived 'water' and 'zebra' owners are valid names? {consistent}")
        ok_all &= consistent

    # D) Determinism/idempotence: repeated solve yields same unique layout count
    sols2 = solve_zebra(max_solutions=10)
    stable = (len(sols) == len(sols2))
    print(f"Deterministic solve (same #solutions on rerun)? {stable}")
    ok_all &= stable

    print(f"\nAll checks passed? {ok_all}")

# ───────────────────────────────────── Main ─────────────────────────────────────

if __name__ == "__main__":
    print_answer()
    print_reason()
    print_check()

