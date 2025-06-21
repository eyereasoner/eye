/*
 * ackermann.c  —  translation of the EYEREASONER N3 rules
 *
 *  • Two-argument Ackermann  A₂(x,y)  is expressed as
 *        A₂(x,y) = H(x, y+3, 2) – 3
 *    where  H is the “hyper-operation” function implemented
 *    directly from the three-argument rules.
 *
 *  • H(0,y,z) = y+1                (successor)
 *    H(1,y,z) = y+z                (addition)
 *    H(2,y,z) = y·z                (multiplication)
 *    H(x≥3,0,z) = 1               (identity element)
 *    H(x≥3,y>0,z) = H(x–1, H(x,y–1,z), z)
 *
 *  This exactly mirrors the N3 clauses.
 */

#include <stdio.h>
#include <gmp.h>

/* hyper-operation H(x,y,z) → result  (all y,z ≥ 0) */
static void
hyper(unsigned int x, const mpz_t y, unsigned int z, mpz_t result)
{
    if (x == 0) {                        /* successor              */
        mpz_add_ui(result, y, 1);
    } else if (x == 1) {                 /* addition               */
        mpz_add_ui(result, y, z);
    } else if (x == 2) {                 /* multiplication         */
        mpz_mul_ui(result, y, z);
    } else {                             /* x ≥ 3   (exponentiation, tetration …) */
        if (mpz_cmp_ui(y, 0) == 0) {
            mpz_set_ui(result, 1);
        } else {
            mpz_t y_minus_1, inner;
            mpz_inits(y_minus_1, inner, NULL);

            mpz_sub_ui(y_minus_1, y, 1);         /* y-1 */
            hyper(x, y_minus_1, z, inner);       /* inner = H(x, y-1, z) */
            hyper(x - 1, inner, z, result);      /* result = H(x-1, inner, z) */

            mpz_clears(y_minus_1, inner, NULL);
        }
    }
}

/* two-argument Ackermann A₂(x,y) = H(x, y+3, 2) – 3 */
static void
ackermann2(unsigned int x, unsigned int y, mpz_t result)
{
    mpz_t yp3;
    mpz_init_set_ui(yp3, y + 3);          /* y+3 */
    hyper(x, yp3, 2, result);             /* H(x, y+3, 2) */
    mpz_sub_ui(result, result, 3);        /* −3 */
    mpz_clear(yp3);
}

int
main(void)
{
    struct { unsigned x, y; } cases[] = {
        {0,0}, {0,6}, {1,2}, {1,7}, {2,2}, {2,9},
        {3,4}, {3,14}, {4,0}, {4,1}, {4,2}, {5,0}
    };
    const char *names[] = {
        "A0","A1","A2","A3","A4","A5",
        "A6","A7","A8","A9","A10","A11"
    };

    for (int i = 0; i < 12; ++i) {
        mpz_t v;
        mpz_init(v);
        ackermann2(cases[i].x, cases[i].y, v);
        gmp_printf("%s = %Zd\n", names[i], v);
        mpz_clear(v);
    }
    return 0;
}
