"""
evolve.py – a minimal evolutionary algorithm (EA) in pure Python.
Author: ChatGPT  (June 2025)

Usage:
    python evolve.py
"""

import random
import math
from typing import List, Tuple, Callable

# ---------- Problem definition ---------- #
def sphere(individual: List[float]) -> float:
    """Simple minimisation benchmark: f(x) = sum(x_i^2)."""
    return sum(x * x for x in individual)


# ---------- EA operators ---------- #
def create_individual(dim: int,
                      lower: float,
                      upper: float) -> List[float]:
    """Random individual with each gene ∈ [lower, upper]."""
    return [random.uniform(lower, upper) for _ in range(dim)]


def mutate(individual: List[float],
           rate: float,
           lower: float,
           upper: float,
           sigma: float = 0.1) -> List[float]:
    """Gaussian mutation with per-gene probability *rate*."""
    rng = upper - lower
    for i in range(len(individual)):
        if random.random() < rate:
            individual[i] += random.gauss(0, sigma * rng)
            # keep inside bounds
            individual[i] = max(lower, min(upper, individual[i]))
    return individual


def crossover(parent1: List[float],
              parent2: List[float]) -> Tuple[List[float], List[float]]:
    """Single-point crossover."""
    if len(parent1) != len(parent2):
        raise ValueError("Parents must be same length")
    point = random.randint(1, len(parent1) - 1)
    child1 = parent1[:point] + parent2[point:]
    child2 = parent2[:point] + parent1[point:]
    return child1, child2


def tournament(pop: List[List[float]],
               fitnesses: List[float],
               k: int = 3) -> List[float]:
    """k-way tournament selection (lower fitness is better)."""
    chosen = random.sample(list(zip(pop, fitnesses)), k)
    return min(chosen, key=lambda t: t[1])[0]  # best individual


# ---------- Main EA loop ---------- #
def evolve(pop_size: int = 100,
           dim: int = 5,
           generations: int = 200,
           lower: float = -5.0,
           upper: float = 5.0,
           crossover_rate: float = 0.8,
           mutation_rate: float = 0.1,
           elite: int = 2,
           fitness_fn: Callable[[List[float]], float] = sphere,
           verbose: bool = True) -> Tuple[List[float], float]:
    """Run the EA and return the best individual + its fitness."""
    # initial population
    pop = [create_individual(dim, lower, upper) for _ in range(pop_size)]

    for gen in range(generations):
        fitnesses = [fitness_fn(ind) for ind in pop]

        # ——— Elitism ——— #
        elite_idx = sorted(range(pop_size), key=lambda i: fitnesses[i])[:elite]
        new_pop = [pop[i][:] for i in elite_idx]

        # ——— Variation ——— #
        while len(new_pop) < pop_size:
            p1 = tournament(pop, fitnesses)
            p2 = tournament(pop, fitnesses)
            # Crossover
            if random.random() < crossover_rate:
                c1, c2 = crossover(p1, p2)
            else:
                c1, c2 = p1[:], p2[:]
            # Mutation
            new_pop.extend([
                mutate(c1, mutation_rate, lower, upper),
                mutate(c2, mutation_rate, lower, upper)
            ])

        pop = new_pop[:pop_size]

        # ——— Progress report ——— #
        if verbose and gen % 10 == 0:
            best = min(fitnesses)
            print(f"Gen {gen:4d}: best fitness = {best:.5f}")

    # final best
    fitnesses = [fitness_fn(ind) for ind in pop]
    best_idx = min(range(pop_size), key=lambda i: fitnesses[i])
    return pop[best_idx], fitnesses[best_idx]


# ---------- Script entry-point ---------- #
if __name__ == "__main__":
    random.seed(42)            # remove for nondeterministic runs
    best, best_fit = evolve()
    print("\nBest individual :", ["{:+.3f}".format(x) for x in best])
    print("Best fitness     :", best_fit)

