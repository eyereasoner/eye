"""
Einstein’s Riddle (aka Zebra Puzzle)
Translated from the original SWI-Prolog version at
https://github.com/DonaldKellett/Einsteins-Riddle-Prolog

Expected solution:
    Water drinker  = norwegian
    Zebra owner    = japanese
"""

from itertools import permutations


# ---------------------------------------------------------------------------
# Helper predicates → helper functions
# ---------------------------------------------------------------------------

def adjacent(i: int, j: int) -> bool:
    """Houses i and j are next to each other."""
    return abs(i - j) == 1


# ---------------------------------------------------------------------------
# Core solver – a literal transcription of the Prolog ‘zebra/2’ predicate
# ---------------------------------------------------------------------------

def zebra() -> tuple[str, str]:
    """
    Returns (WaterDrinkerNationality, ZebraOwnerNationality)
    following the same constraint logic as the original Prolog.
    """

    # Domain values
    COLORS       = ("red", "green", "ivory", "yellow", "blue")
    NATIONALITY  = ("english", "spanish", "ukrainian", "norwegian", "japanese")
    PETS         = ("dog", "snail", "fox", "horse", "zebra")
    BEVERAGES    = ("coffee", "tea", "milk", "orange_juice", "water")
    CIGARETTES   = ("old_gold", "kools", "chesterfields",
                    "lucky_strike", "parliaments")

    # Enumerate possibilities, pruning as soon as we can
    for colors in permutations(COLORS):
        # 6. green immediately to the right of ivory
        if colors.index("green") != colors.index("ivory") + 1:
            continue

        for nations in permutations(NATIONALITY):
            # 2. Englishman ↔ red
            if colors[nations.index("english")] != "red":
                continue
            # 10. Norwegian in first house (index 0)
            if nations[0] != "norwegian":
                continue
            # 15. Norwegian next to blue house
            if not adjacent(nations.index("norwegian"), colors.index("blue")):
                continue

            for drinks in permutations(BEVERAGES):
                # 4. Coffee ↔ green
                if drinks[colors.index("green")] != "coffee":
                    continue
                # 5. Ukrainian ↔ tea
                if drinks[nations.index("ukrainian")] != "tea":
                    continue
                # 9. Milk in middle house (index 2)
                if drinks[2] != "milk":
                    continue

                for smokes in permutations(CIGARETTES):
                    # 8. Kools ↔ yellow
                    if colors[smokes.index("kools")] != "yellow":
                        continue
                    # 13. Lucky Strike ↔ orange juice
                    if drinks[smokes.index("lucky_strike")] != "orange_juice":
                        continue
                    # 14. Japanese ↔ Parliaments
                    if nations[smokes.index("parliaments")] != "japanese":
                        continue

                    for pets in permutations(PETS):
                        # 3. Spaniard ↔ dog
                        if pets[nations.index("spanish")] != "dog":
                            continue
                        # 7. Old Gold ↔ snails
                        if pets[smokes.index("old_gold")] != "snail":
                            continue
                        # 11. Chesterfields next to fox
                        if not adjacent(smokes.index("chesterfields"),
                                        pets.index("fox")):
                            continue
                        # 12. Kools next to horse
                        if not adjacent(smokes.index("kools"),
                                        pets.index("horse")):
                            continue

                        # All constraints satisfied!
                        water_drinker = nations[drinks.index("water")]
                        zebra_owner   = nations[pets.index("zebra")]
                        return water_drinker, zebra_owner

    raise RuntimeError("No solution found – the constraints are inconsistent.")


# ---------------------------------------------------------------------------
# Demo (only runs when this file is executed directly)
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    water, zebra = zebra()
    print(f"{water.capitalize()} drinks water; {zebra.capitalize()} owns the zebra.")

