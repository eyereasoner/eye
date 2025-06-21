#include <stdio.h>

/**
 * Recursively solves the Towers of Hanoi problem.
 *
 * @param n     Number of discs to move
 * @param from  The source peg (e.g., 'A')
 * @param to    The target peg (e.g., 'C')
 * @param aux   The auxiliary peg (e.g., 'B')
 * @param step  Pointer to a counter that tracks the step number
 */
void hanoi(int n, char from, char to, char aux, int* step) {
    if (n == 0) return;

    // Move n-1 discs from source to auxiliary
    hanoi(n - 1, from, aux, to, step);

    // Move the nth disc from source to target
    (*step)++;
    printf("Step %4d: Move disc %d from %c to %c\n", *step, n, from, to);

    // Move the n-1 discs from auxiliary to target
    hanoi(n - 1, aux, to, from, step);
}

int main() {
    int discs = 12;
    int step = 0;

    printf("Towers of Hanoi with %d discs:\n", discs);
    printf("--------------------------------\n");

    hanoi(discs, 'A', 'C', 'B', &step);

    printf("\nTotal steps: %d\n", step);
    return 0;
}

