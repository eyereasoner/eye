/*
 *  graph.c
 *
 *  gcc -std=c11 -O2 -Wall -Wextra graph.c -o graph
 *  ./path2n
 *
 *  Expected output:
 *      paris
 *      chartres
 *      lemans
 *      angers
 */

#include <stdio.h>
#include <stdbool.h>

/* ------------------------------------------------------  graph data -- */

/* 1.  City identifiers (order is arbitrary but must match the names[]) */
enum City {
    PARIS, ORLEANS, CHARTRES, AMIENS, BLOIS,
    BOURGES, TOURS, LEMANS, ANGERS, NANTES,
    N_CITIES                       /* <- automatically counts them     */
};

static const char *const names[N_CITIES] = {
    [PARIS]   = "paris",
    [ORLEANS] = "orleans",
    [CHARTRES]= "chartres",
    [AMIENS]  = "amiens",
    [BLOIS]   = "blois",
    [BOURGES] = "bourges",
    [TOURS]   = "tours",
    [LEMANS]  = "lemans",
    [ANGERS]  = "angers",
    [NANTES]  = "nantes"
};

/* 2.  Edge list — literally the oneway/2 facts                       */
struct Edge { enum City from, to; };

static const struct Edge edges[] = {
    { PARIS,    ORLEANS  },
    { PARIS,    CHARTRES },
    { PARIS,    AMIENS   },
    { ORLEANS,  BLOIS    },
    { ORLEANS,  BOURGES  },
    { BLOIS,    TOURS    },
    { CHARTRES, LEMANS   },
    { LEMANS,   ANGERS   },
    { LEMANS,   TOURS    },
    { ANGERS,   NANTES   }
};

static const int N_EDGES = sizeof edges / sizeof edges[0];

/* -------------------------------------------------  depth-first path -- */

static bool dfs(enum City src, enum City dst,
                bool visited[N_CITIES])
{
    if (src == dst) return true;
    visited[src] = true;

    for (int i = 0; i < N_EDGES; ++i)
        if (edges[i].from == src) {
            enum City v = edges[i].to;
            if (!visited[v] && dfs(v, dst, visited))
                return true;
        }
    return false;
}

static bool path(enum City a, enum City b)
{
    bool visited[N_CITIES] = { false };
    return dfs(a, b, visited);
}

/* -----------------------------------------------------------  query -- */

int main(void)
{
    for (enum City c = 0; c < N_CITIES; ++c)
        if (c != NANTES && path(c, NANTES))
            puts(names[c]);

    return 0;
}

