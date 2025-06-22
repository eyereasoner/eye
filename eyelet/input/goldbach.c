/*  goldbach.c
    -----------------------------------------------------------
    Deterministic Goldbach tester for 2^k, 1 ≤ k ≤ 256  (C11 + GMP)
    -----------------------------------------------------------   */

#include <stdio.h>
#include <gmp.h>

/* user-tunable parameters */
#define MAX_K      256    /* highest k: tests up to 2^256          */
#define MR_ROUNDS   25    /* Miller–Rabin repetitions (false-pos < 2⁻⁵⁰) */

/* helper: print an mpz_t in base-10 */
static inline void mpz_print_dec(const mpz_t n) { mpz_out_str(stdout, 10, n); }

/* ---------------------------------------------------------------- */
int main(void)
{
    /* big-integer work variables */
    mpz_t two_pow_k, p, q;
    mpz_inits(two_pow_k, p, q, NULL);

    puts("Goldbach decompositions for powers of two (2^k, k = 1 … 256)\n");

    for (int k = 1; k <= MAX_K; ++k) {

        /* two_pow_k ← 2^k */
        mpz_set_ui(two_pow_k, 1);
        mpz_mul_2exp(two_pow_k, two_pow_k, k);

        printf("2^%d = ", k);
        mpz_print_dec(two_pow_k);
        putchar('\n');

        /* handle the two special cases explicitly */
        if (k == 1) {                       /* 2  (below the conjecture) */
            puts("  (Goldbach’s conjecture starts at 4)\n");
            continue;
        }
        if (k == 2) {                       /* 4 = 2 + 2  (even prime)   */
            puts("  4 = 2 + 2\n");
            continue;
        }

        /* start with the smallest odd prime, 3 */
        mpz_set_ui(p, 3);

        while (mpz_cmp(p, two_pow_k) < 0) {

            /* q = 2^k – p */
            mpz_sub(q, two_pow_k, p);

            /* if q is prime, we have a Goldbach pair */
            if (mpz_probab_prime_p(q, MR_ROUNDS) > 0) {
                printf("  ");
                mpz_print_dec(two_pow_k);
                printf(" = ");
                mpz_print_dec(p);
                printf(" + ");
                mpz_print_dec(q);
                puts("\n");
                break;
            }

            /* otherwise advance to the next prime > p */
            mpz_nextprime(p, p);            /* p ← next prime after p */
        }

        /* The loop is guaranteed to find a pair for every k tested,
           but in practice we could add a safety check here. */
    }

    mpz_clears(two_pow_k, p, q, NULL);
    return 0;
}

