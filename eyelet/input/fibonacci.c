/*  fibonacci.c
 *
 *  Compile :  gcc -O2 -std=c11 -o fibonacci fibonacci.c -lm
 *  Run     :  ./fibonacci
 *
 *  Prints F(0), F(1), F(6), F(91), F(283) and F(3674).
 *  (You can add more indices in the tests[] array.)
 */
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define BASE       1000000000u          /* 10^9  */
#define BASE_DIGS  9
#define MAX_WORDS  512                  /* 512×9  ≈ 4600 decimal digits */

typedef struct {
    int      len;                       /* words actually used           */
    uint32_t w[MAX_WORDS];              /* w[0] = least-significant word */
} BigInt;

/* ---------------- low-level helpers ---------------- */
static void bi_zero(BigInt *a)
{
    a->len = 1;
    a->w[0] = 0;
}
static void bi_set_uint(BigInt *a, uint32_t v)
{
    if (v == 0) { bi_zero(a); return; }
    a->len = 0;
    while (v) {
        a->w[a->len++] = v % BASE;
        v /= BASE;
    }
}
static void bi_copy(BigInt *dst, const BigInt *src)
{
    dst->len = src->len;
    memcpy(dst->w, src->w, src->len * sizeof(uint32_t));
}
static void bi_add(BigInt *res, const BigInt *a, const BigInt *b)
/* res = a + b   (handles res == a or res == b) */
{
    uint64_t carry = 0;
    int n = (a->len > b->len) ? a->len : b->len;

    for (int i = 0; i < n; ++i) {
        uint64_t sum = carry;
        if (i < a->len) sum += a->w[i];
        if (i < b->len) sum += b->w[i];
        res->w[i] = (uint32_t)(sum % BASE);
        carry     = sum / BASE;
    }
    res->len = n;
    if (carry) res->w[res->len++] = (uint32_t)carry;
}
static void bi_print(const BigInt *a)
/* print as full decimal number */
{
    int i = a->len - 1;
    printf("%u", a->w[i]);              /* most-significant word un-padded */
    for (--i; i >= 0; --i)
        printf("%0*u", BASE_DIGS, a->w[i]);
}

/* ---------------- Fibonacci ---------------- */
static void fibonacci(int n, BigInt *out)
/* out ← F(n)  (iterative, O(n) additions) */
{
    BigInt a, b, tmp;
    bi_set_uint(&a, 0);         /* F(0) */
    bi_set_uint(&b, 1);         /* F(1) */
    if (n == 0) { bi_copy(out, &a); return; }
    if (n == 1) { bi_copy(out, &b); return; }

    for (int i = 2; i <= n; ++i) {
        bi_add(&tmp, &a, &b);   /* tmp = a + b */
        bi_copy(&a, &b);        /* a ← b */
        bi_copy(&b, &tmp);      /* b ← tmp */
    }
    bi_copy(out, &b);
}

/* ---------------- demo ---------------- */
int main(void)
{
    const int tests[] = {0, 1, 6, 91, 283, 3674};
    const int NTESTS = sizeof(tests) / sizeof(tests[0]);

    BigInt F;
    for (int i = 0; i < NTESTS; ++i) {
        int n = tests[i];
        fibonacci(n, &F);
        printf("F(%d) = ", n);
        bi_print(&F);
        putchar('\n');
    }
    return 0;
}

