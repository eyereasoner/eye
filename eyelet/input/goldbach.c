/*  goldbach_powers_of_two_128.c
    ------------------------------------------
    Verify Goldbach's conjecture for 2^k, k=1…127
    Build:  gcc -O2 -std=c11 goldbach_powers_of_two_128.c -o goldbach128
*/

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>

/* ---------- conveniences for 128-bit unsigned integers ---------- */

typedef unsigned __int128  u128;

static void print_u128(u128 x)
/* minimal decimal printing for u128 */
{
    char buf[50];
    int  len = 0;
    if (x == 0) { putchar('0'); return; }
    while (x) { buf[len++] = '0' + (x % 10); x /= 10; }
    while (len--) putchar(buf[len]);
}

/* ---------- modular arithmetic without 256-bit intermediates ---------- */

static inline u128 add_mod(u128 a, u128 b, u128 m)
{   u128 s = a + b;
    return (s >= m || s < a) ? s - m : s;
}

static u128 mul_mod(u128 a, u128 b, u128 m)
/* (a · b) mod m using "double-and-add"; O(128) */
{
    u128 res = 0;
    while (b) {
        if (b & 1) res = add_mod(res, a, m);
        a = add_mod(a, a, m);
        b >>= 1;
    }
    return res;
}

static u128 pow_mod(u128 a, u128 d, u128 m)
/* a^d mod m, binary exponentiation */
{
    u128 r = 1;
    while (d) {
        if (d & 1) r = mul_mod(r, a, m);
        a = mul_mod(a, a, m);
        d >>= 1;
    }
    return r;
}

/* ---------- deterministic Miller-Rabin for 0 < n < 2^128 ---------- */

static bool is_prime_u128(u128 n)
{
    /* small primes first */
    static const uint32_t small[] =
        {2,3,5,7,11,13,17,19,23,29,31,37};
    for (size_t i = 0; i < sizeof small / sizeof *small; ++i) {
        if (n == small[i])    return true;
        if (n % small[i] == 0) return false;
    }
    /* n-1 = d·2^s with d odd */
    u128 d = n - 1;
    int  s = 0;
    while ((d & 1) == 0) { d >>= 1; ++s; }

    for (size_t i = 0; i < sizeof small / sizeof *small; ++i) {
        u128 a = small[i];
        if (a >= n) continue;
        u128 x = pow_mod(a, d, n);
        if (x == 1 || x == n - 1) continue;

        bool witness = true;
        for (int r = 1; r < s; ++r) {
            x = mul_mod(x, x, n);
            if (x == n - 1) { witness = false; break; }
        }
        if (witness) return false;
    }
    return true;
}

/* ---------- Goldbach search for 2^k ---------- */

int main(void)
{
    puts("Goldbach decompositions for powers of two (2^k, 1 ≤ k ≤ 127)");
    puts("--------------------------------------------------------------");

    for (int k = 1; k <= 127; ++k) {
        u128 even = (u128)1 << k;

        printf("\n2^%d = ", k);
        print_u128(even);
        putchar('\n');

        if (even < 4) {                       /* 2 = 2^1 is below conjecture */
            puts("  (Goldbach starts at 4)");
            continue;
        }

        u128 limit = even >> 1;               /* search up to n/2 */
        bool found = false;

        /* brutal linear scan: 2,3,5,7,… */
        for (u128 p = 2; p <= limit; p += (p == 2 ? 1 : 2)) {
            if (!is_prime_u128(p)) continue;
            u128 q = even - p;
            if (is_prime_u128(q)) {
                printf("  ");
                print_u128(even);
                printf(" = ");
                print_u128(p);
                printf(" + ");
                print_u128(q);
                putchar('\n');
                found = true;
                break;
            }
        }
        if (!found)
            puts("  **No representation found — Goldbach would fail here!**");
    }
    return 0;
}

