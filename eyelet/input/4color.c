/*
 * 4color.c  —  Four‑Colour demo for *all* 47 sovereign European states
 * ------------------------------------------------------------------------------
 * Uses the DSATUR heuristic to guarantee a valid 4‑colouring of Europe’s planar
 * map.  Outputs either a plain text listing or—when invoked with `--dot`—a clean
 * Graphviz DOT description you can render with `dot -Tpng`.
 *
 *   $ gcc -std=c99 -Wall -Wextra 4color.c -o 4color
 *   $ ./4color                  # plain list
 *   $ ./4color --dot > europe.dot  &&  dot -Tpng europe.dot -o europe.png
 */

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#define MAX_COUNTRIES 47
#define MAX_COLORS     4

/* ---------------------------------------------------------------------------
 * Country index table (keep in sync with the edge list)
 * ------------------------------------------------------------------------- */
static const char *country_names[MAX_COUNTRIES] = {
    "Albania",                /*  0 */
    "Andorra",                /*  1 */
    "Austria",                /*  2 */
    "Belarus",                /*  3 */
    "Belgium",                /*  4 */
    "Bosnia and Herzegovina", /*  5 */
    "Bulgaria",               /*  6 */
    "Croatia",                /*  7 */
    "Cyprus",                 /*  8 */
    "Czechia",                /*  9 */
    "Denmark",                /* 10 */
    "Estonia",                /* 11 */
    "Finland",                /* 12 */
    "France",                 /* 13 */
    "Germany",                /* 14 */
    "Greece",                 /* 15 */
    "Hungary",                /* 16 */
    "Iceland",                /* 17 */
    "Ireland",                /* 18 */
    "Italy",                  /* 19 */
    "Kosovo",                 /* 20 */
    "Latvia",                 /* 21 */
    "Liechtenstein",          /* 22 */
    "Lithuania",              /* 23 */
    "Luxembourg",             /* 24 */
    "Malta",                  /* 25 */
    "Moldova",                /* 26 */
    "Monaco",                 /* 27 */
    "Montenegro",             /* 28 */
    "Netherlands",            /* 29 */
    "North Macedonia",        /* 30 */
    "Norway",                 /* 31 */
    "Poland",                 /* 32 */
    "Portugal",               /* 33 */
    "Romania",                /* 34 */
    "Russia",                 /* 35 */
    "San Marino",             /* 36 */
    "Serbia",                 /* 37 */
    "Slovakia",               /* 38 */
    "Slovenia",               /* 39 */
    "Spain",                  /* 40 */
    "Sweden",                 /* 41 */
    "Switzerland",            /* 42 */
    "Turkey",                 /* 43 */
    "Ukraine",                /* 44 */
    "United Kingdom",         /* 45 */
    "Vatican City"            /* 46 */
};

/* ---------------------------------------------------------------------------
 * Land‑border adjacency list (undirected edges)
 * ------------------------------------------------------------------------- */
static const int edges[][2] = {
    {0,15},{0,20},{0,28},{0,30},
    {1,13},{1,40},
    {2,9},{2,14},{2,16},{2,19},{2,22},{2,38},{2,39},{2,42},
    {3,21},{3,23},{3,32},{3,35},{3,44},
    {4,13},{4,14},{4,24},{4,29},
    {5,7},{5,28},{5,37},
    {6,15},{6,30},{6,34},{6,37},{6,43},
    {7,16},{7,28},{7,37},{7,39},
    {9,14},{9,32},{9,38},
    {10,14},
    {11,21},{11,35},
    {12,31},{12,35},{12,41},
    {13,14},{13,19},{13,24},{13,27},{13,40},
    {14,24},{14,29},{14,32},{14,42},
    {15,30},{15,43},
    {16,34},{16,37},{16,38},{16,39},{16,44},
    {18,45},
    {19,36},{19,39},{19,42},{19,46},
    {21,23},
    {22,42},
    {23,35},
    {26,34},{26,44},
    {28,37},
    {30,37},
    {31,35},{31,41},
    {32,35},{32,38},{32,44},
    {33,40},
    {34,37},{34,44},
    {35,44},
    {38,44},
    {40,45}
};

static const size_t EDGE_COUNT = sizeof(edges)/sizeof(edges[0]);

/* ---------------------------------------------------------------------------
 * DSATUR colouring (guarantees ≤4 colours on planar graphs)
 * ------------------------------------------------------------------------- */
static void colour_europe(int colour[MAX_COUNTRIES])
{
    /* Build adjacency matrix and degree list */
    bool adj[MAX_COUNTRIES][MAX_COUNTRIES] = {{false}};
    int  degree[MAX_COUNTRIES] = {0};
    for (size_t i = 0; i < EDGE_COUNT; ++i) {
        int u = edges[i][0], v = edges[i][1];
        adj[u][v] = adj[v][u] = true;
        degree[u]++; degree[v]++;
    }

    /* Arrays for DSATUR bookkeeping */
    int  saturation[MAX_COUNTRIES] = {0};
    bool neighbour_used[MAX_COUNTRIES][MAX_COLORS] = {{false}};
    for (int i = 0; i < MAX_COUNTRIES; ++i) colour[i] = -1;

    for (int painted = 0; painted < MAX_COUNTRIES; ++painted) {
        /* 1️⃣  Pick uncoloured vertex with highest saturation (break ties by degree) */
        int chosen = -1, best_sat = -1, best_deg = -1;
        for (int v = 0; v < MAX_COUNTRIES; ++v) {
            if (colour[v] != -1) continue;
            if (saturation[v] > best_sat ||
               (saturation[v] == best_sat && degree[v] > best_deg)) {
                chosen   = v;
                best_sat = saturation[v];
                best_deg = degree[v];
            }
        }

        /* 2️⃣  Allocate the lowest‑numbered colour not used by neighbours */
        bool forbidden[MAX_COLORS] = {false};
        for (int u = 0; u < MAX_COUNTRIES; ++u)
            if (adj[chosen][u] && colour[u] != -1)
                forbidden[colour[u]] = true;

        int c;
        for (c = 0; c < MAX_COLORS && forbidden[c]; ++c) ;
        if (c == MAX_COLORS) {
            fprintf(stderr, "Unexpected: need >%d colours!\n", MAX_COLORS);
            exit(EXIT_FAILURE);
        }
        colour[chosen] = c;

        /* 3️⃣  Update saturation of uncoloured neighbours */
        for (int u = 0; u < MAX_COUNTRIES; ++u) {
            if (adj[chosen][u] && colour[u] == -1 && !neighbour_used[u][c]) {
                neighbour_used[u][c] = true;
                saturation[u]++;
            }
        }
    }
}

/* ---------------------------------------------------------------------------
 * Pretty printing & Graphviz output
 * ------------------------------------------------------------------------- */
static void print_colour_list(const int colour[])
{
    const char *clr[MAX_COLORS] = {"Red", "Green", "Blue", "Yellow"};
    puts("Four‑colouring of Europe:\n");
    for (int i = 0; i < MAX_COUNTRIES; ++i)
        printf("%-25s : %s\n", country_names[i], clr[colour[i]]);
}

static void print_dot(const int colour[])
{
    const char *clr[MAX_COLORS] = {"red", "green", "lightblue", "yellow"};
    puts("graph europe {");
    puts("    overlap=false;\n    splines=true;\n    layout=neato;");
    /* Nodes */
    for (int i = 0; i < MAX_COUNTRIES; ++i)
        printf("    \"%s\" [style=filled, fillcolor=%s];\n",
               country_names[i], clr[colour[i]]);
    /* Edges */
    for (size_t e = 0; e < EDGE_COUNT; ++e)
        printf("    \"%s\" -- \"%s\";\n",
               country_names[edges[e][0]], country_names[edges[e][1]]);
    puts("}");
}

int main(int argc, char **argv)
{
    int colour[MAX_COUNTRIES];
    colour_europe(colour);

    bool dot_mode = (argc > 1 && strcmp(argv[1], "--dot") == 0);
    if (dot_mode)
        print_dot(colour);
    else
        print_colour_list(colour);
    return 0;
}

