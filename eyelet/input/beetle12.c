/* beetle12.c  –  “Beetle-12” in plain C
 *
 * Build:
 *     gcc -O2 -std=c11 beetle12.c -o beetle12
 *
 * The only queries reproduced are:
 *     is(beautiful,beetle)
 *     is(green,beetle)
 *     is(blue,beetle)
 *
 * All deeper attributes (pretty*, nice*, …) are generated during
 * simulation but not printed; they’re shown in comments for reference.
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* ------------------------------------------------------------------ */
/* MONTE-CARLO SAMPLER                                                */
/* ------------------------------------------------------------------ */
/* Each “world” picks one branch at every annotated-disjunction node.
 * We only *count* the final queries, but you can uncomment the deeper
 * counters to see their empirical frequencies.                        */
static void simulate(unsigned long worlds)
{
    unsigned long cnt_green = 0;
    unsigned long cnt_blue  = 0;
    unsigned long cnt_beautiful = 0;

    /* Uncomment if you want stats on the intermediate props
    unsigned long cnt_nice = 0, cnt_pretty = 0;
    unsigned long cnt_pretty11 = 0, cnt_pretty12 = 0, cnt_pretty21 = 0, cnt_pretty22 = 0;
    unsigned long cnt_nice11   = 0, cnt_nice12   = 0, cnt_nice21   = 0, cnt_nice22   = 0;
    */

    srand((unsigned)time(NULL));

    for (unsigned long i = 0; i < worlds; ++i) {

        /* -------- first disjunction: colour ------------------------ */
        int colour = rand() & 1;        /* 0 = green, 1 = blue */

        if (colour) {                   /* --- blue branch --------- */
            ++cnt_blue;
            ++cnt_beautiful;            /* blue ⇒ beautiful */
            continue;                   /* world finished          */
        }

        /* ---------------- green branch ---------------------------- */
        ++cnt_green;

        int qual = rand() & 1;          /* 0 = nice, 1 = pretty */

        if (qual == 1) {                /* ---- pretty subtree ---- */
            // ++cnt_pretty;
            int layer1 = rand() & 1;    /* pretty1 vs pretty2 */
            int layer2 = rand() & 1;    /* pretty11/12 or 21/22 */

            switch ((layer1 << 1) | layer2) {
                case 0: /* pretty11 */ /*++cnt_pretty11;*/ break;
                case 1: /* pretty12 */ /*++cnt_pretty12;*/ break;
                case 2: /* pretty21 */ /*++cnt_pretty21;*/ break;
                case 3: /* pretty22 */ /*++cnt_pretty22;*/ break;
            }

        } else {                        /* ---- nice subtree ------ */
            // ++cnt_nice;
            int layer1 = rand() & 1;    /* nice1 vs nice2  */
            int layer2 = rand() & 1;    /* nice11/12 or 21/22 */

            switch ((layer1 << 1) | layer2) {
                case 0: /* nice11  */ /*++cnt_nice11;*/ break;
                case 1: /* nice12  */ /*++cnt_nice12;*/ break;
                case 2: /* nice21  */ /*++cnt_nice21;*/ break;
                case 3: /* nice22  */ /*++cnt_nice22;*/ break;
            }
        }

        /* Any leaf under green ⇒ beautiful (by rule set) */
        ++cnt_beautiful;
    }

    printf("Worlds: %lu\n", worlds);
    printf("is(beautiful,beetle): %.2f\n", (double)cnt_beautiful / worlds);
    printf("is(green,beetle):     %.2f\n", (double)cnt_green     / worlds);
    printf("is(blue,beetle):      %.2f\n", (double)cnt_blue      / worlds);
}

/* ------------------------------------------------------------------ */
int main(void)
{
    simulate(1000000);
    return 0;
}
