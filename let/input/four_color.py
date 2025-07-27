#!/usr/bin/env python3
"""
four_color.py — Four-colour demo for all 47 sovereign European states
---------------------------------------------------------------------
Run:

  python four_color.py            # plain text listing
  python four_color.py --dot > europe.dot   &&   dot -Tpng europe.dot -o europe.png
"""

from typing import List, Set
import sys

MAX_COUNTRIES = 47
MAX_COLORS    = 4

# --------------------------------------------------------------------------- #
# Country index table (⚠ keep in sync with edges)
# --------------------------------------------------------------------------- #
country_names: List[str] = [
    "Albania",                #  0
    "Andorra",                #  1
    "Austria",                #  2
    "Belarus",                #  3
    "Belgium",                #  4
    "Bosnia and Herzegovina", #  5
    "Bulgaria",               #  6
    "Croatia",                #  7
    "Cyprus",                 #  8
    "Czechia",                #  9
    "Denmark",                # 10
    "Estonia",                # 11
    "Finland",                # 12
    "France",                 # 13
    "Germany",                # 14
    "Greece",                 # 15
    "Hungary",                # 16
    "Iceland",                # 17
    "Ireland",                # 18
    "Italy",                  # 19
    "Kosovo",                 # 20
    "Latvia",                 # 21
    "Liechtenstein",          # 22
    "Lithuania",              # 23
    "Luxembourg",             # 24
    "Malta",                  # 25
    "Moldova",                # 26
    "Monaco",                 # 27
    "Montenegro",             # 28
    "Netherlands",            # 29
    "North Macedonia",        # 30
    "Norway",                 # 31
    "Poland",                 # 32
    "Portugal",               # 33
    "Romania",                # 34
    "Russia",                 # 35
    "San Marino",             # 36
    "Serbia",                 # 37
    "Slovakia",               # 38
    "Slovenia",               # 39
    "Spain",                  # 40
    "Sweden",                 # 41
    "Switzerland",            # 42
    "Turkey",                 # 43
    "Ukraine",                # 44
    "United Kingdom",         # 45
    "Vatican City",           # 46
]

# --------------------------------------------------------------------------- #
# Land-border adjacency list (undirected edges)
# --------------------------------------------------------------------------- #
edges: List[tuple[int, int]] = [
    (0,15),(0,20),(0,28),(0,30),
    (1,13),(1,40),
    (2,9),(2,14),(2,16),(2,19),(2,22),(2,38),(2,39),(2,42),
    (3,21),(3,23),(3,32),(3,35),(3,44),
    (4,13),(4,14),(4,24),(4,29),
    (5,7),(5,28),(5,37),
    (6,15),(6,30),(6,34),(6,37),(6,43),
    (7,16),(7,28),(7,37),(7,39),
    (9,14),(9,32),(9,38),
    (10,14),
    (11,21),(11,35),
    (12,31),(12,35),(12,41),
    (13,14),(13,19),(13,24),(13,27),(13,40),
    (14,24),(14,29),(14,32),(14,42),
    (15,30),(15,43),
    (16,34),(16,37),(16,38),(16,39),(16,44),
    (18,45),
    (19,36),(19,39),(19,42),(19,46),
    (21,23),
    (22,42),
    (23,35),
    (26,34),(26,44),
    (28,37),
    (30,37),
    (31,35),(31,41),
    (32,35),(32,38),(32,44),
    (33,40),
    (34,37),(34,44),
    (35,44),
    (38,44),
    (40,45),
]
EDGE_COUNT = len(edges)

# --------------------------------------------------------------------------- #
# DSATUR colouring (guarantees ≤ 4 colours on planar graphs)
# --------------------------------------------------------------------------- #
def colour_europe() -> List[int]:
    """Return a list of colour indices (0-3) for all 47 countries."""
    # Build adjacency matrix and degree list (mirrors C code exactly)
    adj = [[False] * MAX_COUNTRIES for _ in range(MAX_COUNTRIES)]
    degree = [0] * MAX_COUNTRIES
    for u, v in edges:
        adj[u][v] = adj[v][u] = True
        degree[u] += 1
        degree[v] += 1

    # DSATUR bookkeeping
    colour       = [-1] * MAX_COUNTRIES
    saturation   = [0] * MAX_COUNTRIES
    neighbour_used = [[False] * MAX_COLORS for _ in range(MAX_COUNTRIES)]

    for painted in range(MAX_COUNTRIES):
        # 1️⃣  Pick uncoloured vertex with highest saturation (break ties by degree)
        chosen = -1
        best_sat = best_deg = -1
        for v in range(MAX_COUNTRIES):
            if colour[v] != -1:
                continue
            if (saturation[v] > best_sat or
               (saturation[v] == best_sat and degree[v] > best_deg)):
                chosen, best_sat, best_deg = v, saturation[v], degree[v]

        # 2️⃣  Allocate the lowest colour unused by its neighbours
        forbidden = [False] * MAX_COLORS
        for u in range(MAX_COUNTRIES):
            if adj[chosen][u] and colour[u] != -1:
                forbidden[colour[u]] = True

        try:
            c = forbidden.index(False)   # first allowed colour
        except ValueError:
            sys.exit(f"ERROR: Need more than {MAX_COLORS} colours!")

        colour[chosen] = c

        # 3️⃣  Update saturation of uncoloured neighbours
        for u in range(MAX_COUNTRIES):
            if adj[chosen][u] and colour[u] == -1 and not neighbour_used[u][c]:
                neighbour_used[u][c] = True
                saturation[u] += 1

    return colour

# --------------------------------------------------------------------------- #
# Pretty printing & Graphviz output
# --------------------------------------------------------------------------- #
def print_colour_list(colour: List[int]) -> None:
    clr = ["Red", "Green", "Blue", "Yellow"]
    print("Four-colouring of Europe:\n")
    for i, name in enumerate(country_names):
        print(f"{name:<25} : {clr[colour[i]]}")

def print_dot(colour: List[int]) -> None:
    clr = ["red", "green", "lightblue", "yellow"]
    print("graph europe {")
    print("    overlap=false;\n    splines=true;\n    layout=neato;")
    # Nodes
    for i, name in enumerate(country_names):
        print(f'    "{name}" [style=filled, fillcolor={clr[colour[i]]}];')
    # Edges
    for u, v in edges:
        print(f'    "{country_names[u]}" -- "{country_names[v]}";')
    print("}")

# --------------------------------------------------------------------------- #
def main(argv: List[str]) -> None:
    colours = colour_europe()
    dot_mode = len(argv) > 1 and argv[1] == "--dot"
    if dot_mode:
        print_dot(colours)
    else:
        print_colour_list(colours)

if __name__ == "__main__":
    main(sys.argv)

