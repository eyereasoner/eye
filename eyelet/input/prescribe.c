/*
 * prescribe.c  —  probabilistic drug-selection demo (plain C version)
 *
 * Compile:   gcc -O2 -std=c11 -o prescribe prescribe.c -lm
 * Run:       ./prescribe            # prints recommendation for cough+fever case
 *
 * The program reproduces the ProbLog model by exhaustive enumeration of the
 * four possible disease states (flu ∈{0,1}, covid ∈{0,1}) and then:
 *   1. computes the posterior P(flu, covid | evidence),
 *   2. marginalises to obtain P(good(oseltamivir)) and P(good(paxlovid)),
 *   3. prints the two posterior probabilities, sorted, and recommends the
 *      drug with the higher probability of *net benefit*.
 */

#include <stdio.h>
#include <stdbool.h>

/* ---------- fixed model parameters ---------- */
enum {   /* indices for disease combinations (flu<<1 | covid) */
    NONE = 0, FLU_ONLY = 2, COVID_ONLY = 1, BOTH = 3
};

/* Priors */
static const double P_FLU   = 0.10;
static const double P_COVID = 0.25;

/* Symptom generation */
static const double P_COUGH_FLU   = 0.40;
static const double P_COUGH_COVID = 0.90;
static const double P_FEVER_FLU   = 0.20;
static const double P_FEVER_COVID = 0.80;

/* Drug effectiveness */
static const double P_EFF_OSE_FLU   = 0.85;
static const double P_EFF_OSE_COVID = 0.10;
static const double P_EFF_PAX_COVID = 0.90;
static const double P_EFF_PAX_FLU   = 0.05;

/* Side-effect risks (independent of disease) */
static const double P_SE_OSE = 0.15;
static const double P_SE_PAX = 0.25;

/* ---------- helpers ---------- */

/* P(symptom | flu, covid)  —  independent noisy-OR */
static double
p_symptom(bool flu, bool covid, double p_given_flu, double p_given_covid)
{
    double p_no = 1.0;
    if (flu)   p_no *= (1.0 - p_given_flu);
    if (covid) p_no *= (1.0 - p_given_covid);
    return 1.0 - p_no;
}

/* P(effective(drug) | flu, covid)  —  independent noisy-OR */
static double
p_effective_ose(bool flu, bool covid)
{
    double p_no = 1.0;
    if (flu)   p_no *= (1.0 - P_EFF_OSE_FLU);
    if (covid) p_no *= (1.0 - P_EFF_OSE_COVID);
    return 1.0 - p_no;
}

static double
p_effective_pax(bool flu, bool covid)
{
    double p_no = 1.0;
    if (covid) p_no *= (1.0 - P_EFF_PAX_COVID);
    if (flu)   p_no *= (1.0 - P_EFF_PAX_FLU);
    return 1.0 - p_no;
}

/* ---------- core inference ---------- */
typedef struct {
    double p_good_ose;
    double p_good_pax;
} Posterior;

/* Returns P(good(oseltamivir)) and P(good(paxlovid)) given evidence */
static Posterior
prescribe(bool evidence_cough, bool evidence_fever)
{
    /* 1.  posterior over four disease states --------------------------- */
    double w[4] = {0};             /* unnormalised weights               */
    double norm = 0.0;

    for (int flu = 0; flu <= 1; ++flu) {
        for (int covid = 0; covid <= 1; ++covid) {
            double prior = (flu   ? P_FLU   : 1.0 - P_FLU) *
                           (covid ? P_COVID : 1.0 - P_COVID);

            double p_cough = p_symptom(flu, covid, P_COUGH_FLU, P_COUGH_COVID);
            double p_fever = p_symptom(flu, covid, P_FEVER_FLU, P_FEVER_COVID);

            double p_evidence =
                (evidence_cough ? p_cough : (1.0 - p_cough)) *
                (evidence_fever ? p_fever : (1.0 - p_fever));

            int idx = (flu << 1) | covid;  /* NONE, COVID_ONLY, FLU_ONLY, BOTH */
            w[idx]  = prior * p_evidence;
            norm   += w[idx];
        }
    }
    for (int i = 0; i < 4; ++i) w[i] /= norm;   /* normalise */

    /* 2.  marginalise to drug-level probabilities ---------------------- */
    double p_good_ose = 0.0, p_good_pax = 0.0;
    for (int flu = 0; flu <= 1; ++flu) {
        for (int covid = 0; covid <= 1; ++covid) {
            int idx = (flu << 1) | covid;

            double p_eff_ose = p_effective_ose(flu, covid);
            double p_eff_pax = p_effective_pax(flu, covid);

            double p_good_ose_state = p_eff_ose * (1.0 - P_SE_OSE);
            double p_good_pax_state = p_eff_pax * (1.0 - P_SE_PAX);

            p_good_ose += w[idx] * p_good_ose_state;
            p_good_pax += w[idx] * p_good_pax_state;
        }
    }
    return (Posterior){p_good_ose, p_good_pax};
}

/* ---------- demonstration (same as Python’s __main__) ----------------- */
int main(void)
{
    /* Example: patient presents with BOTH cough **and** fever */
    bool cough = true;
    bool fever = true;

    Posterior post = prescribe(cough, fever);

    /* pretty-print in descending order */
    if (post.p_good_ose > post.p_good_pax) {
        printf("Posterior P(good(oseltamivir)) = %.3f\n", post.p_good_ose);
        printf("Posterior P(good(paxlovid))    = %.3f\n", post.p_good_pax);
        printf("\n--> Recommend oseltamivir "
               "(probability of net benefit ≈ %.1f%%)\n",
               100.0 * post.p_good_ose);
    } else {
        printf("Posterior P(good(paxlovid))    = %.3f\n", post.p_good_pax);
        printf("Posterior P(good(oseltamivir)) = %.3f\n", post.p_good_ose);
        printf("\n--> Recommend paxlovid "
               "(probability of net benefit ≈ %.1f%%)\n",
               100.0 * post.p_good_pax);
    }
    return 0;
}

