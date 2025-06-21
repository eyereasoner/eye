#include <stdio.h>
#include <complex.h>
#include <math.h>

/*
 * Translation of the N3 rules for complex-number exponentiation,
 * asin, and acos into straightforward C using the C99 <complex.h> API.
 *
 * Each query triple has been mapped to a single line that computes the
 * required operation with the corresponding libc function:
 *
 *   • cpow  – complex exponentiation
 *   • casin – complex arcsine
 *   • cacos – complex arccosine
 *
 * The results are printed in the same order as the original query.
 */

int main(void)
{
    const double pi = 3.141592653589793;
    const double e  = 2.718281828459045;

    /* 1. ((-1 0) (0.5 0)) complex:exponentiation ?C1. */
    double complex C1 = cpow(-1.0 + 0.0*I, 0.5 + 0.0*I);

    /* 2. ((e 0) (0 π)) complex:exponentiation ?C2. */
    double complex C2 = cpow(e + 0.0*I, 0.0 + pi*I);

    /* 3. ((0 1) (0 1)) complex:exponentiation ?C3. */
    double complex C3 = cpow(0.0 + 1.0*I, 0.0 + 1.0*I);

    /* 4. ((e 0) (-π/2 0)) complex:exponentiation ?C4. */
    double complex C4 = cpow(e + 0.0*I, -pi/2.0 + 0.0*I);

    /* 5. (2 0) complex:asin ?C5. */
    double complex C5 = casin(2.0 + 0.0*I);

    /* 6. (2 0) complex:acos ?C6. */
    double complex C6 = cacos(2.0 + 0.0*I);

    printf("C1 = %.15f %+.15fi\n", creal(C1), cimag(C1));
    printf("C2 = %.15f %+.15fi\n", creal(C2), cimag(C2));
    printf("C3 = %.15f %+.15fi\n", creal(C3), cimag(C3));
    printf("C4 = %.15f %+.15fi\n", creal(C4), cimag(C4));
    printf("C5 = %.15f %+.15fi\n", creal(C5), cimag(C5));
    printf("C6 = %.15f %+.15fi\n", creal(C6), cimag(C6));
    return 0;
}

