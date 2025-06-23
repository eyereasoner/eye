#!/usr/bin/env python3
"""
genetic_algorithm.py ― heavily annotated “textbook” GA template
————————————————————————————————————————————————————————————————————
 • Optimises a user-supplied fitness function (minimisation).
 • Representation = list[float]  (continuous genes in [lo, hi]).
 • Operators:
     – k-way tournament selection
     – single-point crossover
     – Gaussian mutation
     – elitism   (best N individuals survive unchanged)

Edit `sphere()` or pass your own `fitness` when you call
`genetic_algorithm()`.
"""

import random
from typing import List, Tuple, Callable

# ════════════════════════════════════════════════════════════════════════
# 1.  PROBLEM DEFINITION – replace this for your own optimisation task
# ════════════════════════════════════════════════════════════════════════

def sphere(ind: List[float]) -> float:
    """
    Classic continuous benchmark:  f(x) = Σ xᵢ²   (global minimum = 0)
    Lower fitness value ⇒ better individual  (we’re *minimising*).
    """
    return sum(x * x for x in ind)


# ════════════════════════════════════════════════════════════════════════
# 2.  GA BUILDING BLOCKS  (selection, crossover, mutation, etc.)
# ════════════════════════════════════════════════════════════════════════

def create_individual(dim: int, lo: float, hi: float) -> List[float]:
    """
    Initialise one random genome consisting of `dim` real-valued genes,
    each sampled uniformly from [lo, hi].
    """
    return [random.uniform(lo, hi) for _ in range(dim)]


def tournament(pop: List[List[float]],
               fits: List[float],
               k: int = 3) -> List[float]:
    """
    k-way tournament selection (no replacement):
    • Pick k distinct individuals at random.
    • Return the one with the *lowest* fitness value (best).
    """
    contenders = random.sample(range(len(pop)), k)
    best_idx   = min(contenders, key=lambda i: fits[i])
    return pop[best_idx]


def crossover(p1: List[float],
              p2: List[float],
              rate: float = 0.9) -> Tuple[List[float], List[float]]:
    """
    One-point crossover:
        With probability `rate` → cut between 1 and len-1,
        swap tails to create two children.
        Else → children are clones of parents.
    Works equally for integer / float genes.
    """
    if random.random() >= rate or len(p1) < 2:       # “no crossover” branch
        return p1[:], p2[:]

    cut = random.randint(1, len(p1) - 1)             # inclusive ends
    return (p1[:cut] + p2[cut:],
            p2[:cut] + p1[cut:])


def mutate(ind: List[float],
           per_gene_rate: float,
           lo: float,
           hi: float,
           sigma: float = 0.1) -> List[float]:
    """
    Gaussian mutation: for each gene, with probability `per_gene_rate`
    add N(0, sigma·range) noise and clamp back into [lo, hi].

    For binary strings you’d flip the bit here instead.
    """
    span = hi - lo
    for i in range(len(ind)):
        if random.random() < per_gene_rate:
            ind[i] += random.gauss(0.0, sigma * span)
            ind[i]  = max(lo, min(hi, ind[i]))        # hard bounds
    return ind


# ════════════════════════════════════════════════════════════════════════
# 3.  MAIN GA DRIVER
# ════════════════════════════════════════════════════════════════════════

def genetic_algorithm(*,
                      fitness: Callable[[List[float]], float],
                      dim: int,
                      pop_size: int = 100,
                      generations: int = 200,
                      lo: float = -5.0,
                      hi: float =  5.0,
                      cx_rate: float = 0.9,
                      mut_rate: float = 0.05,
                      elite: int = 2,
                      verbose: bool = True) -> Tuple[List[float], float]:
    """
    High-level GA loop.

    Parameters
    ----------
    fitness        : function(individual) → score
    dim            : genome length
    pop_size       : number of individuals in the population
    generations    : how many iterations to run
    lo, hi         : lower/upper bounds for every gene
    cx_rate        : probability a mating undergoes crossover
    mut_rate       : per-gene mutation probability
    elite          : # of top individuals copied unchanged to next gen
    verbose        : print progress every 10 gens if True
    """
    # --- 1. initial random population ------------------------------------
    pop = [create_individual(dim, lo, hi) for _ in range(pop_size)]

    # --- 2. evolutionary loop --------------------------------------------
    for g in range(generations):
        fits = [fitness(ind) for ind in pop]

        # (a) ELITISM: keep best N directly
        elite_idx = sorted(range(pop_size), key=lambda i: fits[i])[:elite]
        next_pop  = [pop[i][:] for i in elite_idx]   # shallow copy genomes

        # (b) REPRODUCTION: fill rest of next_pop
        while len(next_pop) < pop_size:
            # selection
            parent1 = tournament(pop, fits, k=3)
            parent2 = tournament(pop, fits, k=3)
            # variation
            child1, child2 = crossover(parent1, parent2, cx_rate)
            next_pop.extend([
                mutate(child1, mut_rate, lo, hi),
                mutate(child2, mut_rate, lo, hi)
            ])

        pop = next_pop[:pop_size]                    # exact pop_size again

        # progress printout
        if verbose and g % 10 == 0:
            print(f"Gen {g:3d} | best = {min(fits):.6f}")

    # --- 3. return very best ---------------------------------------------
    fits = [fitness(ind) for ind in pop]
    best_idx = min(range(pop_size), key=lambda i: fits[i])
    return pop[best_idx], fits[best_idx]


# ════════════════════════════════════════════════════════════════════════
# 4.  DEMO / CLI
# ════════════════════════════════════════════════════════════════════════
if __name__ == "__main__":
    random.seed(2025)      # remove for nondeterministic runs

    best_ind, best_fit = genetic_algorithm(
        fitness=sphere,    # your objective function
        dim=5,
        pop_size=80,
        generations=150,
        lo=-5.0,
        hi=5.0,
        cx_rate=0.9,
        mut_rate=0.05,
        elite=2,
        verbose=True
    )

    print("\nBest individual:", [f"{x:+.3f}" for x in best_ind])
    print("Best fitness    :", best_fit)

