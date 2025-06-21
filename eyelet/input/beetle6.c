/* beetle6.c  –  “Beetle-6 (beetle is nice)” in plain C
 *
 * Build:
 *     gcc -O2 -std=c11 beetle6.c -o beetle6
 *
 * The queries reproduced are:
 *     is(nice,beetle)
 *     is(beautiful,beetle)
 *     is(blue,beetle)
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* ------------------------------------------------------------------ */
/* MONTE-CARLO CONDITIONED SAMPLER                                    */
/* ------------------------------------------------------------------ */
static void simulate(unsigned long trials)
{
    unsigned long acc_nice = 0;          /* counts within ACCEPTED worlds */
    unsigned long acc_blue = 0;
    unsigned long acc_beaut = 0;         /* sanity check: should stay 0 */
    unsigned long accepted = 0;

    srand((unsigned)time(NULL));

    for (unsigned long i = 0; i < trials; ++i) {

        /* --------- first choice: colour --------------------------- */
        int colour = rand() & 1;         /* 0 = green, 1 = blue */
        int beautiful = 0;
        int nice = 0;

        if (colour == 1) {               /* blue branch */
            beautiful = 1;
        } else {                         /* green branch */
            int qual = rand() & 1;       /* 0 = nice, 1 = pretty */

            if (qual == 0) {
                nice = 1;                /* green ∧ nice */
            } else {
                beautiful = 1;           /* green ∧ pretty ⇒ beautiful */
            }
        }

        /* --------- integrity constraint --------------------------- */
        if (beautiful) continue;         /* REJECT world */

        /* --------- keep world, update tallies --------------------- */
        ++accepted;
        acc_nice      += nice;
        acc_blue      += (colour == 1);
        acc_beaut     += beautiful;      /* remains zero */
    }

    if (accepted == 0) {
        printf("No worlds satisfied the constraint – increase the trial count.\n");
        return;
    }

    printf("Trials: %lu\n", trials);
    printf("is(nice,beetle):      %.2f\n", (double)acc_nice  / accepted);
    printf("is(beautiful,beetle): %.2f\n", (double)acc_beaut / accepted);
    printf("is(blue,beetle):      %.2f\n", (double)acc_blue  / accepted);
}

/* ------------------------------------------------------------------ */
int main(void)
{
    simulate(1000000);
    return 0;
}
