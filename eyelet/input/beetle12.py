"""
beetle12.py
===========

Facts
-----
    car(beetle).

Annotated disjunctions  (all branches weight ½, and are exclusive)
------------------------------------------------------------------
    green ∨ blue                       (root)
    nice ∨ pretty                      (under green)
    nice1 ∨ nice2 ,  pretty1 ∨ pretty2
    nice11 ∨ nice12, nice21 ∨ nice22,
    pretty11 ∨ pretty12, pretty21 ∨ pretty22

Deterministic rules
-------------------
    beautiful ← blue
    beautiful ← any of {pretty11, pretty12, pretty21, pretty22,
                        nice11, nice12, nice21, nice22}

Queries
-------
    prop(beautiful,beetle)
    prop(green,beetle)
    prop(blue,beetle)
"""

from typing import List, Tuple, Dict


# ───────────────────────────────────────────────────────────────
# 1 ▸  Enumerate mutually-exclusive worlds
# ───────────────────────────────────────────────────────────────
World = Tuple[str, float]           # (leaf atom, probability weight)

def generate_worlds() -> List[World]:
    worlds: List[World] = []

    for colour in ("green", "blue"):
        p_colour = 0.5

        if colour == "blue":                 # blue stops the branch
            worlds.append(("blue", p_colour))
            continue

        # colour == green: expand the full Beetle-12 tree
        for quality in ("nice", "pretty"):
            p_quality = p_colour * 0.5

            for idx1 in ("1", "2"):
                p_lvl3 = p_quality * 0.5
                prefix = f"{quality}{idx1}"

                for idx2 in ("1", "2"):
                    leaf  = f"{prefix}{idx2}"
                    p_leaf = p_lvl3 * 0.5
                    worlds.append((leaf, p_leaf))

    return worlds


# ───────────────────────────────────────────────────────────────
# 2 ▸  Accumulate query probabilities
# ───────────────────────────────────────────────────────────────
def evaluate_queries(worlds: List[World]) -> Dict[str, float]:
    probs = {
        "prop(beautiful,beetle)": 0.0,
        "prop(green,beetle)":     0.0,
        "prop(blue,beetle)":      0.0,
    }

    for atom, weight in worlds:
        # Identify the root colour:
        if atom == "blue":
            probs["prop(blue,beetle)"] += weight
        else:                      # every other leaf means the root was green
            probs["prop(green,beetle)"] += weight

        # Beauty rule covers blue plus every nice*/pretty* leaf
        if atom == "blue" or atom.startswith(("pretty", "nice")):
            probs["prop(beautiful,beetle)"] += weight

    return probs


# ───────────────────────────────────────────────────────────────
# 3 ▸  Main
# ───────────────────────────────────────────────────────────────
def main() -> None:
    worlds  = generate_worlds()
    results = evaluate_queries(worlds)

    for q in (
        "prop(beautiful,beetle)",
        "prop(green,beetle)",
        "prop(blue,beetle)",
    ):
        print(f"{q}: {results[q]}")

if __name__ == "__main__":
    main()

