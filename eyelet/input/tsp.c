/*
 * tsp.c ―  A minimal genetic algorithm for the 25-city TSP
 *
 * Compile  :  gcc tsp.c -o tsp -lm
 * Run      :  ./tsp
 *
 * ------------------------------------------------------------
 * Main ingredients
 *   • Chromosome = permutation of {0 … NUM_CITIES-1}
 *   • Fitness    = total tour length   (smaller is better)
 *   • Selection  = 2-way tournament
 *   • Crossover  = Order Crossover (OX)
 *   • Mutation   = Swap two cities with probability MUTATION_RATE
 *   • Elitism    = best ELITE_COUNT individuals survive unchanged
 * ------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#define NUM_CITIES     25          /* problem size                     */
#define POP_SIZE       100         /* individuals per generation       */
#define GENERATIONS    2000        /* GA iterations                    */
#define MUTATION_RATE  0.06        /* probability of a swap mutation   */
#define ELITE_COUNT    1           /* # of elite individuals           */

/* ---------------- data types ---------------- */

typedef struct { double x, y; } City;

typedef struct {
    int    tour[NUM_CITIES];       /* permutation (genes)              */
    double dist;                   /* total tour length (fitness)      */
} Individual;

/* ---------------- globals ------------------- */

City        cities[NUM_CITIES];
double      dist_matrix[NUM_CITIES][NUM_CITIES];
Individual  pop[POP_SIZE], new_pop[POP_SIZE];

/* ------------ utility routines ------------- */

/* Euclidean distance using precalculated matrix */
static inline double distance(int i, int j) { return dist_matrix[i][j]; }

/* Fisher-Yates shuffle for random permutation */
void shuffle(int *a, int n)
{
    for (int i = n - 1; i > 0; --i) {
        int j = rand() % (i + 1);
        int tmp = a[i]; a[i] = a[j]; a[j] = tmp;
    }
}

/* Compute full tour length, including return to start */
double tour_length(const int *tour)
{
    double d = 0.0;
    for (int i = 0; i < NUM_CITIES; ++i) {
        int from = tour[i];
        int to   = tour[(i + 1) % NUM_CITIES];
        d += distance(from, to);
    }
    return d;
}

/* -------- genetic-algorithm primitives ------ */

/* Tournament selection (size 2) — return pointer to fitter parent */
Individual *tournament_select(void)
{
    int a = rand() % POP_SIZE;
    int b = rand() % POP_SIZE;
    return (pop[a].dist < pop[b].dist) ? &pop[a] : &pop[b];
}

/* Order Crossover (OX) — fills child[] from parent1 & parent2 */
void order_crossover(const int *p1, const int *p2, int *child)
{
    int start = rand() % NUM_CITIES;
    int end   = start + 1 + rand() % (NUM_CITIES - start);

    /* mark which genes have been copied */
    int used[NUM_CITIES] = {0};

    /* copy slice from parent1 */
    for (int i = start; i < end; ++i) {
        child[i] = p1[i];
        used[p1[i]] = 1;
    }

    /* fill remaining positions in order they appear in parent2 */
    int idx = end % NUM_CITIES;
    for (int k = 0; k < NUM_CITIES; ++k) {
        int gene = p2[(end + k) % NUM_CITIES];
        if (!used[gene]) {
            child[idx] = gene;
            used[gene] = 1;
            idx = (idx + 1) % NUM_CITIES;
        }
    }
}

/* Swap mutation: exchange two cities with probability MUTATION_RATE */
void mutate(int *tour)
{
    if ((double)rand() / RAND_MAX < MUTATION_RATE) {
        int i = rand() % NUM_CITIES;
        int j = rand() % NUM_CITIES;
        int tmp = tour[i]; tour[i] = tour[j]; tour[j] = tmp;
    }
}

/* ---------- GA population management -------- */

/* Sort ascending by distance (qsort comparator) */
int cmp_individuals(const void *a, const void *b)
{
    const Individual *ia = (const Individual *)a;
    const Individual *ib = (const Individual *)b;
    return (ia->dist > ib->dist) - (ia->dist < ib->dist);
}

/* Produce next generation into new_pop[] */
void evolve(void)
{
    /* --- elitism: copy best individuals unchanged --- */
    for (int e = 0; e < ELITE_COUNT; ++e) new_pop[e] = pop[e];

    /* --- create rest of population via GA operators --- */
    for (int i = ELITE_COUNT; i < POP_SIZE; ++i) {
        Individual *p1 = tournament_select();
        Individual *p2 = tournament_select();
        order_crossover(p1->tour, p2->tour, new_pop[i].tour);
        mutate(new_pop[i].tour);
        new_pop[i].dist = tour_length(new_pop[i].tour);
    }

    /* replace old population */
    for (int i = 0; i < POP_SIZE; ++i) pop[i] = new_pop[i];
}

/* -------------- program entry --------------- */

int main(void)
{
    srand(0);

    /* --- 1.  generate random coordinates & precompute matrix --- */
    for (int i = 0; i < NUM_CITIES; ++i) {
        cities[i].x = rand() % 1000;
        cities[i].y = rand() % 1000;
    }
    for (int i = 0; i < NUM_CITIES; ++i)
        for (int j = 0; j < NUM_CITIES; ++j)
            dist_matrix[i][j] = hypot(cities[i].x - cities[j].x,
                                      cities[i].y - cities[j].y);

    /* --- 2.  initialise population with random tours --- */
    for (int p = 0; p < POP_SIZE; ++p) {
        for (int c = 0; c < NUM_CITIES; ++c) pop[p].tour[c] = c;
        shuffle(pop[p].tour, NUM_CITIES);
        pop[p].dist = tour_length(pop[p].tour);
    }

    /* --- 3.  GA main loop --- */
    for (int g = 0; g < GENERATIONS; ++g) {
        qsort(pop, POP_SIZE, sizeof(Individual), cmp_individuals);

        /* progress output every 100 generations */
        if (g % 100 == 0)
            printf("Gen %4d | best distance = %.2f\n", g, pop[0].dist);

        evolve();
    }

    /* --- 4.  final result --- */
    qsort(pop, POP_SIZE, sizeof(Individual), cmp_individuals);
    printf("\n*** Best tour found ***\n");
    for (int i = 0; i < NUM_CITIES; ++i) printf("%2d ", pop[0].tour[i]);
    printf("\nDistance: %.2f\n", pop[0].dist);

    return 0;
}

