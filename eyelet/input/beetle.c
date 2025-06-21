/* beetle.c – Minimal Beetle in pure C
 *
 * Build:   gcc -O2 -std=c11 beetle.c -o beetle
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* ---------- Monte-Carlo simulation ----------------------------------- */
static void simulate(unsigned long n)
{
    unsigned long green_cnt = 0;
    unsigned long blue_cnt  = 0;
    unsigned long beautiful_cnt = 0;         /* for completeness */

    srand((unsigned)time(NULL));

    for (unsigned long i = 0; i < n; ++i) {
        /* Sample exclusive disjunction: 0 = green, 1 = blue */
        int coin = rand() & 1;               /* fast unbiased 0/1 */

        if (coin == 0) {
            ++green_cnt;
            ++beautiful_cnt;                 /* green ⇒ beautiful */
        } else {
            ++blue_cnt;
            ++beautiful_cnt;                 /* blue  ⇒ beautiful */
        }
    }

    printf("Samples: %lu\n", n);
    printf("is(beautiful,beetle): %.2f\n", (double)beautiful_cnt / n);
    printf("is(green,beetle):     %.2f\n", (double)green_cnt      / n);
    printf("is(blue,beetle):      %.2f\n", (double)blue_cnt       / n);
}

/* --------------------------------------------------------------------- */
int main(void)
{
    simulate(1000000);
    return 0;
}
