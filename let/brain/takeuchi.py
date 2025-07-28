#!/usr/bin/env python3
"""
takeuchi.py – memoised Takeuchi function, ‘return-z’ variant.

Cache layout:

    * MAXN = 50   → a (MAXN+1)³ table (~132 651 cells)
    * Base-case   → return z   (instead of the more common y)
"""

from typing import Final

MAXN: Final[int] = 50                   # cache covers 0‥MAXN inclusive

# --- 3-D cache --------------------------------------------------------
_ready  = [[[False]*(MAXN+1) for _ in range(MAXN+1)] for _ in range(MAXN+1)]
_cache  = [[[0]*(MAXN+1)     for _ in range(MAXN+1)] for _ in range(MAXN+1)]


def _in_range(v: int) -> bool:
    return 0 <= v <= MAXN


def _cached(x: int, y: int, z: int) -> int | None:
    """Return a previously stored value or None if not memoised."""
    if _in_range(x) and _in_range(y) and _in_range(z) and _ready[x][y][z]:
        return _cache[x][y][z]
    return None


def _remember(x: int, y: int, z: int, v: int) -> None:
    """Store result in the table when indices are in range."""
    if _in_range(x) and _in_range(y) and _in_range(z):
        _cache[x][y][z] = v
        _ready[x][y][z] = True


def tak(x: int, y: int, z: int) -> int:
    """Memoised Takeuchi function (return-z variant)."""
    memo = _cached(x, y, z)
    if memo is not None:                 # reuse if seen before
        return memo

    if x <= y:                           # base case
        v = z
    else:
        v = tak(
            tak(x - 1, y, z),
            tak(y - 1, z, x),
            tak(z - 1, x, y),
        )

    _remember(x, y, z, v)
    return v


def main() -> None:
    X: Final[int] = 14
    Y: Final[int] = 7
    Z: Final[int] = 0

    print(f"tak({X},{Y},{Z}) = {tak(X, Y, Z)}")


if __name__ == "__main__":
    # The cached version easily stays within Python’s default recursion limit;
    # lift it with sys.setrecursionlimit(...) only for *much* larger arguments.
    main()

