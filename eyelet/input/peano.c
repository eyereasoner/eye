/*
 * Peano arithmetic demo that prints results as Peano terms
 *     A  = s(s(0))
 *     B  = s(s(s(s(s(0)))))
 *     B! = s(...(0))      // 120 nested “s(”
 *
 *   gcc -std=c99 -Wall -Wextra -O2 peano.c -o peano
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* ---------- arithmetic exactly as before ------------------------- */
int add(int A, int B)               { return B ? add(A, B - 1) + 1 : A; }
int multiply(int A, int B)          { return B ? add(A, multiply(A, B - 1)) : 0; }
int factorial(int N)                { return N ? multiply(factorial(N - 1), N) : 1; }

/* ---------- Peano pretty-printer --------------------------------- */
/* returns freshly malloc’d C-string such that:
 *   to_peano(0)  ->  "0"
 *   to_peano(3)  ->  "s(s(s(0)))"
 * caller frees the string.
 */
char *to_peano(int n)
{
    assert(n >= 0);
    /* Each layer contributes "s("  (2 bytes) and later ")" (1 byte)  */
    size_t len = n * 3 + 2;            /* "0" + final '\0'           */
    char *p = malloc(len);
    if (!p) { perror("malloc"); exit(EXIT_FAILURE); }

    char *cursor = p;
    for (int i = 0; i < n; ++i) { *cursor++ = 's'; *cursor++ = '('; }
    *cursor++ = '0';
    for (int i = 0; i < n; ++i) { *cursor++ = ')'; }
    *cursor   = '\0';

    return p;
}

/* ---------- main mimicking the original Prolog query ------------- */
int main(void)
{
    int A = multiply(1, 2);     /* s(0) * s(s(0))  = 1 * 2 = 2  */
    int B = add(A, 3);          /* A + s(s(s(0)))  = 2 + 3 = 5  */
    int factB = factorial(B);   /* 5! = 120                     */

    char *pA = to_peano(A);
    char *pB = to_peano(B);
    char *pFact = to_peano(factB);

    printf("A  = %s   // %d\n", pA, A);
    printf("B  = %s   // %d\n", pB, B);
    printf("B! = %s   // %d\n", pFact, factB);

    free(pA); free(pB); free(pFact);
    return 0;
}

