/*
 * diagnose.c — Simple Bayesian diagnosis demo in pure C
 *
 * Compile:   gcc -std=c11 -O2 diagnose.c -o diagnose
 * Run:       ./diagnose
 *
 * The program prints posterior probabilities for each disease
 * given three binary symptoms (cough, fever, sore throat).
 */

#include <stdio.h>

#define NUM_DISEASES 4
#define NUM_SYMPTOMS 3

/* ---------- tweak defaults here ---------------------------------- */
#define OBS_COUGH        1   /* 1 = symptom present, 0 = absent */
#define OBS_FEVER        1
#define OBS_SORE_THROAT  0
/* ----------------------------------------------------------------- */

/* Enumerations for clarity */
enum Disease  { FLU, COVID, COLD, NONE };
enum Symptom  { COUGH, FEVER, SORE_THROAT };

/* Priors P(D) — must sum to 1. */
static const double priors[NUM_DISEASES] = {
    0.10,   /* flu   */
    0.25,   /* covid */
    0.40,   /* cold  */
    0.25    /* none  */
};

/* Likelihood table P(symptom=1 | disease) */
static const double cond[NUM_DISEASES][NUM_SYMPTOMS] = {
    /* COUGH  FEVER  SORE_THROAT */
    { 0.40,  0.20,  0.30 },   /* flu   */
    { 0.90,  0.80,  0.25 },   /* covid */
    { 0.50,  0.10,  0.70 },   /* cold  */
    { 0.05,  0.02,  0.10 }    /* none  */
};

static const char *disease_name[NUM_DISEASES] = {
    "flu", "covid", "cold", "none"
};
static const char *symptom_name[NUM_SYMPTOMS] = {
    "cough", "fever", "sore throat"
};

/* Compute posterior P(D | evidence) */
static void posterior(const int evidence[NUM_SYMPTOMS],
                      double out[NUM_DISEASES])
{
    double total = 0.0;

    for (int d = 0; d < NUM_DISEASES; ++d) {
        double likelihood = 1.0;

        for (int s = 0; s < NUM_SYMPTOMS; ++s) {
            double p = cond[d][s];
            likelihood *= evidence[s] ? p : (1.0 - p);
        }

        out[d] = priors[d] * likelihood;
        total += out[d];
    }

    /* Normalise */
    for (int d = 0; d < NUM_DISEASES; ++d)
        out[d] /= total;
}

int main(void)
{
    /* Default evidence compiled in */
    int evidence[NUM_SYMPTOMS] = {
        OBS_COUGH, OBS_FEVER, OBS_SORE_THROAT
    };

    /* --- Optional interactive override --------------------------
    printf("Enter cough (0/1) fever (0/1) sore_throat (0/1): ");
    scanf("%d %d %d", &evidence[COUGH], &evidence[FEVER], &evidence[SORE_THROAT]);
    -------------------------------------------------------------- */

    for (int s = 0; s < NUM_SYMPTOMS; ++s)
        printf("Observed %-12s = %d\n", symptom_name[s], evidence[s]);

    double post[NUM_DISEASES];
    posterior(evidence, post);

    puts("\nPosterior probabilities:");
    for (int d = 0; d < NUM_DISEASES; ++d)
        printf("  P(%-5s | evidence) = %.3f\n", disease_name[d], post[d]);

    return 0;   /* success */
}

