#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
four_color.py — Four-colour demo for all 47 sovereign European states (ARC-ified)
────────────────────────────────────────────────────────────────────────────────────

Run
---
  python four_color_arc.py
      → prints ARC sections (Answer / Reason why / Check)

  python four_color_arc.py --dot > europe.dot
      → Graphviz DOT (nodes pre-coloured). Then:
         dot -Tpng europe.dot -o europe.png

Notes
-----
• Countries and land borders are hard-coded (undirected; no enclaves-by-sea).
• Colouring uses the DSATUR heuristic (Brelàz): pick the uncoloured vertex
  with maximum *saturation* (distinct neighbour colours), breaking ties by
  degree; assign the smallest available colour.
• DSATUR is deterministic here (fixed scan order).
"""

from __future__ import annotations
from typing import List, Tuple, Dict
import sys

MAX_COUNTRIES = 47
MAX_COLORS    = 4

# ─────────────────────────────────────────────────────────────
# Country index table (⚠ keep in sync with edges)
# ─────────────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────
# Land-border adjacency list (undirected edges)
# ─────────────────────────────────────────────────────────────
edges: List[Tuple[int, int]] = [
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

# ─────────────────────────────────────────────────────────────
# DSATUR colouring (≤ 4 colours expected for this planar map)
# ─────────────────────────────────────────────────────────────
def build_adj() -> Tuple[List[List[bool]], List[int]]:
    """Adjacency matrix and degree list."""
    adj = [[False] * MAX_COUNTRIES for _ in range(MAX_COUNTRIES)]
    degree = [0] * MAX_COUNTRIES
    for u, v in edges:
        adj[u][v] = adj[v][u] = True
        degree[u] += 1
        degree[v] += 1
    return adj, degree

def colour_europe() -> List[int]:
    """Return a list of colour indices (0..3) for all 47 countries."""
    adj, degree = build_adj()

    colour         = [-1] * MAX_COUNTRIES
    saturation     = [0] * MAX_COUNTRIES
    neighbour_used = [[False] * MAX_COLORS for _ in range(MAX_COUNTRIES)]

    for _painted in range(MAX_COUNTRIES):
        # 1) choose uncoloured vertex with highest saturation, tiebreak by degree
        chosen = -1
        best_sat = best_deg = -1
        for v in range(MAX_COUNTRIES):
            if colour[v] != -1:
                continue
            if (saturation[v] > best_sat or
               (saturation[v] == best_sat and degree[v] > best_deg)):
                chosen, best_sat, best_deg = v, saturation[v], degree[v]

        # 2) assign the smallest colour unused by its neighbours
        forbidden = [False] * MAX_COLORS
        for u in range(MAX_COUNTRIES):
            if adj[chosen][u] and colour[u] != -1:
                forbidden[colour[u]] = True
        try:
            c = forbidden.index(False)
        except ValueError:
            raise RuntimeError(f"Need more than {MAX_COLORS} colours for this graph.")
        colour[chosen] = c

        # 3) update saturation of uncoloured neighbours
        for u in range(MAX_COUNTRIES):
            if adj[chosen][u] and colour[u] == -1 and not neighbour_used[u][c]:
                neighbour_used[u][c] = True
                saturation[u] += 1

    return colour

# ─────────────────────────────────────────────────────────────
# Pretty printing & Graphviz output
# ─────────────────────────────────────────────────────────────
CLR_NAMES = ["Red", "Green", "Blue", "Yellow"]
DOT_COLORS = ["red", "green", "lightblue", "yellow"]

def print_colour_list(colour: List[int]) -> None:
    print("Four-colouring of Europe:\n")
    for i, name in enumerate(country_names):
        print(f"{name:<25} : {CLR_NAMES[colour[i]]}")

def print_dot(colour: List[int]) -> None:
    print("graph europe {")
    print("    overlap=false;\n    splines=true;\n    layout=neato;")
    for i, name in enumerate(country_names):
        print(f'    "{name}" [style=filled, fillcolor={DOT_COLORS[colour[i]]}];')
    for u, v in edges:
        print(f'    "{country_names[u]}" -- "{country_names[v]}";')
    print("}")

# ─────────────────────────────────────────────────────────────
# ARC — Answer / Reason why / Check
# ─────────────────────────────────────────────────────────────
def arc_answer(colour: List[int]) -> None:
    print("Answer")
    print("------")
    used = sorted(set(colour))
    palette = ", ".join(CLR_NAMES[c] for c in used)
    print(f"Countries: {MAX_COUNTRIES}   Edges: {EDGE_COUNT}")
    print(f"Used colours: {len(used)}  (≤ 4)   Palette: {palette}\n")

    # Per-colour clusters
    inv: Dict[int, List[str]] = {c: [] for c in used}
    for i, c in enumerate(colour):
        inv[c].append(country_names[i])
    for c in used:
        members = ", ".join(sorted(inv[c]))
        print(f"{CLR_NAMES[c]:>6}: {members}")
    print()

def arc_reason() -> None:
    print("Reason why")
    print("----------")
    print("We use DSATUR (degree of saturation): iteratively pick the uncoloured")
    print("vertex with the largest number of DISTINCT neighbour colours; break ties")
    print("by highest degree; assign the smallest colour not used by its neighbours.")
    print("This greedy heuristic reliably finds a 4-colouring on planar maps; here")
    print("it colours all 47 states using ≤ 4 colours, respecting all land borders.\n")

def arc_check(colour: List[int]) -> None:
    print("Check (harness)")
    print("---------------")
    # 0) basic sanity of tables
    assert len(country_names) == MAX_COUNTRIES, "Country table size mismatch."
    assert len(colour) == MAX_COUNTRIES, "Colour array size mismatch."
    assert len(edges) == EDGE_COUNT, "Edge count changed; update EDGE_COUNT."

    # 1) colour range
    for c in colour:
        assert 0 <= c < MAX_COLORS, f"Illegal colour index {c}"

    # 2) adjacency constraint: endpoints must differ
    bad = []
    for u, v in edges:
        assert 0 <= u < MAX_COUNTRIES and 0 <= v < MAX_COUNTRIES and u != v, "Bad edge index/self-loop."
        if colour[u] == colour[v]:
            bad.append((u, v))
    assert not bad, f"Adjacent countries share a colour: {[(country_names[u], country_names[v]) for u,v in bad]}"

    # 3) determinism: rerun DSATUR yields identical assignment
    again = colour_europe()
    assert again == colour, "Colouring not deterministic with current DSATUR choices."

    # 4) ≤ 4 colours actually used
    assert len(set(colour)) <= MAX_COLORS, "Used more than 4 colours."

    # 5) quick symmetric adjacency check
    adj, _deg = build_adj()
    for u in range(MAX_COUNTRIES):
        for v in range(MAX_COUNTRIES):
            assert adj[u][v] == adj[v][u], "Adjacency not symmetric."

    print("OK: constraints respected, deterministic DSATUR, ≤ 4 colours, symmetric adjacency.\n")

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────
def main(argv: List[str]) -> None:
    colours = colour_europe()
    if len(argv) > 1 and argv[1] == "--dot":
        print_dot(colours)
        return
    arc_answer(colours)
    arc_reason()
    arc_check(colours)

if __name__ == "__main__":
    main(sys.argv)

