#!/usr/bin/env python3
"""
Conway’s Game of Life — 32×32 torus, reproducible random demo
Python 3.8+      (June 2025)

Usage:
    python life32_seed.py              # default density 0.25, seed 12345
    python life32_seed.py 0.40         # 40 % density, same seed
    python life32_seed.py 0.30 9999    # 30 %, user-chosen seed
"""
import random
import sys
import time
from typing import List

SIZE               = 32         # board is SIZE × SIZE
DEFAULT_DENSITY    = 0.25       # fraction of live cells at start
DEFAULT_SEED       = 12345      # fixed seed → reproducible run
GENERATIONS        = 100        # how many steps to simulate
FRAME_DELAY_SEC    = 0.0        # pause between frames (seconds)

World = List[List[bool]]        # type alias


# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------
def parse_cli() -> tuple[float, int]:
    """Read optional density and seed from the command line."""
    if len(sys.argv) == 1:
        return DEFAULT_DENSITY, DEFAULT_SEED
    if len(sys.argv) == 2:
        return float(sys.argv[1]), DEFAULT_SEED
    if len(sys.argv) >= 3:
        return float(sys.argv[1]), int(sys.argv[2])
    raise SystemExit("Usage: life32_seed.py [density [seed]]")


def random_world(density: float, seed: int) -> World:
    """Create a SIZE×SIZE world with the given density of live cells."""
    random.seed(seed)
    return [[random.random() < density for _ in range(SIZE)]
            for _ in range(SIZE)]


def count_neighbors(world: World, x: int, y: int) -> int:
    """Toroidal count of live neighbors around (x, y)."""
    cnt = 0
    for dx in (-1, 0, 1):
        for dy in (-1, 0, 1):
            if dx == dy == 0:
                continue
            nx = (x + dx) % SIZE
            ny = (y + dy) % SIZE
            cnt += world[ny][nx]
    return cnt


def next_generation(world: World) -> World:
    """Compute the next world according to standard Life rules."""
    new = [[False] * SIZE for _ in range(SIZE)]
    for y in range(SIZE):
        for x in range(SIZE):
            n = count_neighbors(world, x, y)
            new[y][x] = n == 3 or (world[y][x] and n == 2)
    return new


def print_world(world: World, gen: int) -> None:
    """Pretty-print the board as ASCII."""
    print(f"Generation {gen}")
    for row in world:
        print(''.join('O' if cell else '.' for cell in row))
    print()                       # blank line after each frame


# ----------------------------------------------------------------------
# Main simulation loop
# ----------------------------------------------------------------------
def run(density: float = DEFAULT_DENSITY,
        seed: int = DEFAULT_SEED,
        gens: int = GENERATIONS,
        delay: float = FRAME_DELAY_SEC) -> None:
    """Run the Game of Life for *gens* generations."""
    world = random_world(max(0.0, min(1.0, density)), seed)
    for g in range(gens):
        print_world(world, g)
        time.sleep(delay)
        world = next_generation(world)


if __name__ == "__main__":
    dens, rng_seed = parse_cli()
    run(dens, rng_seed)
