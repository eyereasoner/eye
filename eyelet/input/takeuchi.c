/* takeuchi.c : memoised Takeuchi function — “return-z” variant     */
/* build:  gcc -O3 -march=native -flto takeuchi.c -o takeuchi       */
#include <stdio.h>
#include <stdint.h>
#include <limits.h>

#define MAXN 50                    /* cache covers 0‥MAXN inclusive */

static int8_t ready[MAXN + 1][MAXN + 1][MAXN + 1];
static long   cache[MAXN + 1][MAXN + 1][MAXN + 1];

#define IN_RANGE(v) ((v) >= 0 && (v) <= MAXN)

static inline long cached(long x, long y, long z)
{
    return (IN_RANGE(x) && IN_RANGE(y) && IN_RANGE(z) && ready[x][y][z])
           ? cache[x][y][z]
           : LONG_MIN;                         /* sentinel: “not cached” */
}

static inline void remember(long x, long y, long z, long v)
{
    if (IN_RANGE(x) && IN_RANGE(y) && IN_RANGE(z)) {
        cache[x][y][z] = v;
        ready[x][y][z] = 1;
    }
}

static long tak(long x, long y, long z)
{
    long v = cached(x, y, z);
    if (v != LONG_MIN)           /* seen before? → reuse */
        return v;

    if (x <= y)                  /* BASE CASE —--> **return z** */
        v = z;
    else
        v = tak(tak(x - 1, y, z),
                 tak(y - 1, z, x),
                 tak(z - 1, x, y));

    remember(x, y, z, v);
    return v;
}

int main(void)                   /* “default arguments” */
{
    const long X = 14, Y = 7, Z = 0;
    printf("tak(%ld,%ld,%ld) = %ld\n", X, Y, Z, tak(X, Y, Z));
    return 0;
}

