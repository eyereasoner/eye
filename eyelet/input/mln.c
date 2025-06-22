/*
 * mln.c — Tiny Markov‑Logic‑Network with Gibbs sampling
 *
 * ------------------------------------------------------------------------
 *  WHAT THIS PROGRAM DOES, IN PLAIN ENGLISH
 *  ---------------------------------------
 *  • We have three people: Alice(0), Bob(1), Charlie(2).
 *  • Random variables (atoms) are Ancestor(p,q) for every ordered pair p≠q.
 *    ► 3 people ⇒ 3×3 – 3(diagonal) = 6 Bernoulli variables.
 *  • Evidence (hard facts) is the small parent chain:
 *        Parent(Alice,Bob)   Parent(Bob,Charlie)
 *    All other Parent(.,.) facts are FALSE.  Evidence never changes.
 *  • Two weighted FIRST‑ORDER formulas (see below) give SOFT constraints.
 *    Their finite weights mean they are PREFERENCES, not hard rules.
 *  • We run Gibbs sampling to approximate the posterior P(Ancestor=1 | evidence).
 *    → The marginals printed are generally between 0 and 1.
 *      They are not 0/1 because (a) weights are finite, (b) we have
 *      incomplete evidence, and (c) a Monte‑Carlo estimate has noise.
 *
 *  Compile :  gcc -std=c11 -O2 mln.c -lm -o mln
 *  Run     :  ./mln
 * ------------------------------------------------------------------------
 *  FORMULAS AND WEIGHTS (MLN)
 *  --------------------------
 *  F1  weight w1 = 1.5   Parent(p,q)  ⇒ Ancestor(p,q)
 *  F2  weight w2 = 1.2   Ancestor(p,q) ∧ Ancestor(q,r) ⇒ Ancestor(p,r)   (transitivity)
 *
 *  A finite weight means: each TRUE grounding multiplies world‑probability
 *  by exp(weight).  Example: w = 1.5 ⇒ ≈4.5× more likely.  That is strong
 *  but NOT infinite, therefore marginals will rarely reach exactly 0 or 1.
 *
 *  If you *do* want 0/1 posteriors you can:
 *    • Increase the weights toward infinity (effectively hard constraints).
 *    • Add more hard evidence to fully determine the variables.
 *    • Use a logical KB instead of an MLN.
 * ------------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#define PERSONS   3                  /* Alice(0), Bob(1), Charlie(2) */
#define N_BURN    5000               /* burn‑in iterations */
#define N_SAMPLES 20000              /* samples collected after burn‑in */

/* ---------- HARD EVIDENCE: Parent relation ---------------------- */
/*
 * We encode Parent as a fixed 2‑D array. 1 = true, 0 = false.
 * Only two facts are true; everything else is false.
 */
static const int Parent[PERSONS][PERSONS] = {
/*          A  B  C */
  /* A */ { 0, 1, 0 },   /* Parent(Alice,Bob) */
  /* B */ { 0, 0, 1 },   /* Parent(Bob,Charlie) */
  /* C */ { 0, 0, 0 }
};

/* ---------- SOFT WEIGHTS (tweak to make distributions sharper) -- */
static const double W1 = 1.5;  /* Parent ⇒ Ancestor           */
static const double W2 = 1.2;  /* Transitivity of Ancestor     */

/* ---------- Helper macro: flatten pair index -------------------- */
#define IDX(p,q) ((p)*PERSONS + (q))

/* ---------- Compute log‑weight (score) of a world --------------- */
/*
 * The score is Σ_w weight_i × (# satisfied groundings of formula i).
 * We recompute from scratch each time to keep the code short; fine
 * for a toy network (6 variables × 2 formulas).
 */
static double world_log_weight(const int Anc[PERSONS][PERSONS])
{
    int trueF1 = 0, trueF2 = 0;

    /* F1:  Parent(p,q) ⇒ Ancestor(p,q) */
    for (int p = 0; p < PERSONS; ++p)
        for (int q = 0; q < PERSONS; ++q)
            if (p != q) {
                int satisfied = !Parent[p][q] || Anc[p][q];
                if (satisfied) ++trueF1;
            }

    /* F2:  Anc(p,q) ∧ Anc(q,r) ⇒ Anc(p,r)   (transitivity) */
    for (int p = 0; p < PERSONS; ++p)
        for (int q = 0; q < PERSONS; ++q)
            if (p != q && Anc[p][q])
                for (int r = 0; r < PERSONS; ++r)
                    if (q != r && p != r && Anc[q][r]) {
                        int satisfied = Anc[p][r];
                        if (satisfied) ++trueF2;
                    }

    return W1 * trueF1 + W2 * trueF2;  /* log of unnormalised weight */
}

/* ---------- Gibbs sampling routine ------------------------------ */
int main(void)
{
    srand(0);

    int Anc[PERSONS][PERSONS] = {0};           /* current world (all false) */
    double tally[PERSONS][PERSONS] = {0};      /* counts for marginals */

    for (int iter = 0; iter < N_BURN + N_SAMPLES; ++iter) {

        /* Gibbs sweep: update each Ancestor variable conditioned on the rest */
        for (int p = 0; p < PERSONS; ++p)
            for (int q = 0; q < PERSONS; ++q)
                if (p != q) {

                    /* log‑weight with Anc(p,q)=1 */
                    Anc[p][q] = 1;
                    double lw1 = world_log_weight(Anc);

                    /* log‑weight with Anc(p,q)=0 */
                    Anc[p][q] = 0;
                    double lw0 = world_log_weight(Anc);

                    /* Conditional probability via logistic function */
                    double prob1 = 1.0 / (1.0 + exp(lw0 - lw1));

                    Anc[p][q] = (rand() / (double)RAND_MAX < prob1);
                }

        /* After burn‑in we collect frequencies for estimating P(Ancestor=1) */
        if (iter >= N_BURN) {
            for (int p = 0; p < PERSONS; ++p)
                for (int q = 0; q < PERSONS; ++q)
                    if (p != q)
                        tally[p][q] += Anc[p][q];
        }
    }

    /* ---------- Print approximate marginals --------------------- */
    const char *name[PERSONS] = { "Alice", "Bob", "Charlie" };

    printf("Approximate marginals after %d Gibbs samples:\n\n", N_SAMPLES);
    for (int p = 0; p < PERSONS; ++p)
        for (int q = 0; q < PERSONS; ++q)
            if (p != q)
                printf("P(Ancestor(%s,%s)=true) ≈ %.3f\n",
                       name[p], name[q],
                       tally[p][q] / N_SAMPLES);

    /* Example point‑query */
    printf("\n=> Estimated P(Alice is ancestor of Charlie) = %.3f\n",
           tally[0][2] / N_SAMPLES);

    /*
     * NOTE ON OUTPUT:
     * ----------------
     * Results hover near 0 or 1 when the soft rules *strongly* favor a value,
     * but they rarely reach EXACTLY 0 or 1 unless:
     *   (a) a weight → ∞ (hard rule), or
     *   (b) the atom is fixed by evidence.
     * Finite weights + incomplete evidence ⇒ intermediate probabilities.
     */

    return 0;
}

