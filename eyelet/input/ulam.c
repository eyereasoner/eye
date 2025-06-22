/*  Ulam Prime Spiral – ASCII Edition
    Inspired by “Prime Patterns”, Mathematica magazine (Lees & Farndon, 2025)
    --------------------------------------------------------------
    • Accepts one odd positive integer N (spiral width and height)
    • Prints an N×N grid centred at (0,0); primes are '#', others '.'
    • Uses a simple deterministic prime test up to N²
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>

/* ---------- fast enough deterministic primality for 32-bit range ---------- */
static bool is_prime(unsigned int n)
{
    if (n < 2)               return false;
    if (n == 2 || n == 3)    return true;
    if (n % 2 == 0)          return false;
    if (n % 3 == 0)          return false;

    /* 6k ± 1 test */
    unsigned int r = (unsigned int) sqrt(n);
    for (unsigned int f = 5; f <= r; f += 6) {
        if (n % f == 0 || n % (f + 2) == 0) return false;
    }
    return true;
}

/* ---------- main ---------------------------------------------------------- */
int main(void)
{
    int N = 49;

    /* allocate flat char buffer (N×N) initialised with '.' */
    char *grid = calloc((size_t)N * N, sizeof(char));
    if (!grid) { perror("calloc"); return EXIT_FAILURE; }
    for (size_t i = 0; i < (size_t)N * N; ++i) grid[i] = '.';

    /* spiral walk starting from the centre */
    int x = N / 2, y = N / 2;          // current coordinates
    unsigned int current = 1;          // current integer to test
    grid[y * N + x] = is_prime(current) ? '#' : '.';  // place 1 (not prime)

    int step_len = 1;                  // length of current leg
    const int dx[4] = {1, 0, -1, 0};   // directions: E, N, W, S
    const int dy[4] = {0, -1, 0, 1};

    while (step_len < N) {
        for (int dir = 0; dir < 4; ++dir) {      // four legs per layer
            int steps_this_leg = (dir < 2) ? step_len : step_len + 1;
            for (int s = 0; s < steps_this_leg; ++s) {
                x += dx[dir];
                y += dy[dir];
                if (x < 0 || x >= N || y < 0 || y >= N) break;

                ++current;
                grid[y * N + x] = is_prime(current) ? '+' : ' ';
            }
        }
        step_len += 2;    // after completing E+N and W+S legs, move to next ring
    }

    /* output the grid */
    for (int row = 0; row < N; ++row) {
        fwrite(grid + row * N, 1, (size_t)N, stdout);
        putchar('\n');
    }

    free(grid);
    return EXIT_SUCCESS;
}

