/* water.c  –  “Water is observable” model in C
 *
 * Build:
 *     gcc -O2 -std=c11 water.c -o water
 *
 * The only queries reproduced are:
 *     is(observable,water)
 *     is(solid,water)
 *     is(liquid,water)
 *     is(gas,water)
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* ------------------------------------------------------------------ */
/* MONTE-CARLO SAMPLER                                                */
/* ------------------------------------------------------------------ */
static void simulate(unsigned long worlds)
{
    unsigned long cnt_solid = 0;
    unsigned long cnt_liquid = 0;
    unsigned long cnt_gas = 0;
    unsigned long cnt_observ = 0;

    srand(0);

    for (unsigned long i = 0; i < worlds; ++i) {
        /* 0 = solid, 1 = liquid, 2 = gas */
        int phase = rand() % 3;

        switch (phase) {
            case 0: ++cnt_solid;  break;
            case 1: ++cnt_liquid; break;
            case 2: ++cnt_gas;    break;
        }
        ++cnt_observ;  /* every phase ⇒ observable */
    }

    printf("Worlds: %lu\n", worlds);
    printf("is(observable,water): %.6f\n", (double)cnt_observ / worlds);
    printf("is(solid,water):      %.6f\n", (double)cnt_solid  / worlds);
    printf("is(liquid,water):     %.6f\n", (double)cnt_liquid / worlds);
    printf("is(gas,water):        %.6f\n", (double)cnt_gas    / worlds);
}

/* ------------------------------------------------------------------ */
int main(void)
{
    simulate(1000000);
    return 0;
}

