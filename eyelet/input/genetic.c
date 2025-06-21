#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define TARGET       "METHINKS IT IS LIKE A WEASEL"
#define GENESET      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ,!."
#define POP_SIZE     100
#define MUTATION_RATE 0.01
#define MAX_GENERATIONS 10000

#define MAX_LEN 100

typedef struct {
    char genome[MAX_LEN];
    int fitness;
} Individual;

// Random character from geneset
char random_char() {
    size_t len = strlen(GENESET);
    return GENESET[rand() % len];
}

// Generate random genome
void random_genome(char *genome, int len) {
    for (int i = 0; i < len; i++) {
        genome[i] = random_char();
    }
    genome[len] = '\0';
}

// Evaluate fitness (number of characters matching target)
int evaluate_fitness(const char *genome) {
    int score = 0;
    for (int i = 0; TARGET[i] != '\0'; i++) {
        if (genome[i] == TARGET[i]) score++;
    }
    return score;
}

// Crossover: one-point
void crossover(const char *parent1, const char *parent2, char *child, int len) {
    int point = rand() % len;
    for (int i = 0; i < len; i++) {
        child[i] = (i < point) ? parent1[i] : parent2[i];
    }
    child[len] = '\0';
}

// Mutation
void mutate(char *genome, int len) {
    for (int i = 0; i < len; i++) {
        if ((rand() / (double)RAND_MAX) < MUTATION_RATE) {
            genome[i] = random_char();
        }
    }
}

// Tournament selection
Individual* select_parent(Individual *population) {
    Individual *a = &population[rand() % POP_SIZE];
    Individual *b = &population[rand() % POP_SIZE];
    return (a->fitness > b->fitness) ? a : b;
}

int main() {
    srand(2);
    int len = strlen(TARGET);
    Individual population[POP_SIZE];

    // Initialize population
    for (int i = 0; i < POP_SIZE; i++) {
        random_genome(population[i].genome, len);
        population[i].fitness = evaluate_fitness(population[i].genome);
    }

    int generation = 0;
    while (generation < MAX_GENERATIONS) {
        // Check for perfect match
        for (int i = 0; i < POP_SIZE; i++) {
            if (population[i].fitness == len) {
                printf("Found in generation %d: %s\n", generation, population[i].genome);
                return 0;
            }
        }

        // Create next generation
        Individual new_population[POP_SIZE];
        for (int i = 0; i < POP_SIZE; i++) {
            Individual *parent1 = select_parent(population);
            Individual *parent2 = select_parent(population);
            crossover(parent1->genome, parent2->genome, new_population[i].genome, len);
            mutate(new_population[i].genome, len);
            new_population[i].fitness = evaluate_fitness(new_population[i].genome);
        }

        memcpy(population, new_population, sizeof(population));
        generation++;

        // Optional: print best fitness
        int best = 0;
        for (int i = 1; i < POP_SIZE; i++) {
            if (population[i].fitness > population[best].fitness)
                best = i;
        }
        printf("Gen %4d | Best: %s | Fitness: %d\n", generation, population[best].genome, population[best].fitness);
    }

    printf("Target not found in %d generations.\n", MAX_GENERATIONS);
    return 0;
}

