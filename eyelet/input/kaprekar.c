/*
 * kaprekar.c  —  Verify Kaprekar's constant (6174) for all 4-digit numbers
 *
 *   gcc -O2 -std=c11 kaprekar.c -o kaprekar
 *   ./kaprekar
 *
 * References:
 *   • https://en.wikipedia.org/wiki/Kaprekar%27s_constant
 *   • For 4-digit numbers with ≥ 2 distinct digits, the map
 *       n  ↦  sort_desc(n) − sort_asc(n)
 *     reaches 6174 in ≤ 7 steps.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/* --------------------------------------------------  helpers  -------- */

static void split_digits(int n, int d[4])
{
    d[0] = n / 1000;
    d[1] = (n / 100) % 10;
    d[2] = (n / 10)  % 10;
    d[3] =  n        % 10;
}

static int join_digits(const int d[4])
{
    return d[0] * 1000 + d[1] * 100 + d[2] * 10 + d[3];
}

static int cmp_asc  (const void *a, const void *b) { return *(int*)a - *(int*)b; }
static int cmp_desc (const void *a, const void *b) { return *(int*)b - *(int*)a; }

static bool all_same_digits(int n)
{
    int d = n % 10;
    return (n / 1000 == d) && ((n / 100) % 10 == d) && ((n / 10) % 10 == d);
}

/* ------------------------------------------------ Kaprekar routine --- */

static int kaprekar_steps(int n)
/* Return iterations needed to reach 6174, or −1 if it somehow
 * fails to converge within 10 rounds (should never happen).      */
{
    int steps = 0;
    while (n != 6174 && steps < 10) {
        int d[4];
        split_digits(n, d);

        qsort(d, 4, sizeof(int), cmp_desc);
        int hi = join_digits(d);

        qsort(d, 4, sizeof(int), cmp_asc);
        int lo = join_digits(d);

        n = hi - lo;
        ++steps;
    }
    return (n == 6174) ? steps : -1;
}

/* ---------------------------------------------------  main  ---------- */

int main(void)
{
    int freq[8] = {0};           /* how many numbers need 1,2,…,7 steps */
    int max_steps = 0;
    int failures  = 0;

    for (int n = 1000; n <= 9999; ++n) {
        if (all_same_digits(n)) continue;    /* 0000,1111,… never change */

        int s = kaprekar_steps(n);
        if (s == -1) { ++failures; continue; }

        if (s > max_steps) max_steps = s;
        if (s <= 7) ++freq[s];
    }

    puts("Kaprekar-constant statistics for all 4-digit numbers:");
    for (int s = 1; s <= 7; ++s)
        printf("  %d numbers reach 6174 in %d step%s\n",
               freq[s], s, (s == 1 ? "" : "s"));

    printf("\nMaximum steps observed : %d\n", max_steps);
    printf("Non-convergent numbers  : %d\n", failures);

    return 0;
}

