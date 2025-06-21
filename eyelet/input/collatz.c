/* collatz.c
   Verify the Collatz conjecture for n = 1 … 1 000 000
   build:  gcc -O2 collatz.c -o collatz
*/

#include <stdio.h>
#include <stdint.h>

#define LIMIT 1000000ULL        /* highest start value to test        */
#define MAX_STEPS   10000       /* practical guard against endless loops */

typedef unsigned long long u64; /* 64-bit integer type                */

int main(void)
{
    u64 worst_n    = 0;   /* starting value with the longest run     */
    u64 worst_step = 0;   /* corresponding step count                */
    u64 worst_peak = 0;   /* highest number reached over all runs    */

    for (u64 n = 1; n <= LIMIT; ++n) {
        u64 x     = n;
        u64 steps = 0;
        u64 peak  = x;

        while (x != 1) {
            if (++steps > MAX_STEPS) {
                fprintf(stderr,
                        "Exceeded %u steps for n = %llu → conjecture possibly false!\n",
                        MAX_STEPS, n);
                return 1;      /* give up early if something is fishy */
            }

            if (x & 1)           /* odd → 3x + 1  */
                x = 3 * x + 1;
            else                 /* even → x / 2  */
                x >>= 1;

            if (x > peak) peak = x;
        }

        if (steps > worst_step) {
            worst_step = steps;
            worst_peak = peak;
            worst_n    = n;
        }
    }

    printf("Checked n = 1 … %u\n", (unsigned)LIMIT);
    printf("Longest total-stopping-time   : %llu steps (for n = %llu)\n",
           worst_step, worst_n);
    printf("Highest value ever observed   : %llu\n", worst_peak);
    printf("Collatz conjecture holds for all tested numbers ✔️\n");
    return 0;
}

