/*
 * Conway's Game of Life — ASCII Edition
 * --------------------------------------
 * Compile with:
 *   gcc -O2 -std=c11 -Wall -o conway conway.c
 *
 * Run with optional arguments:
 *   ./life [rows] [cols] [generations]
 *   (Defaults: rows=25, cols=80, generations=100)
 *
 * The program uses a toroidal (wrap‑around) grid. Living cells are drawn as
 * 'O', dead cells as '.'. The screen is cleared between generations using ANSI
 * escape sequences, so run it from a terminal that understands them.
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define ALIVE 'O'
#define DEAD  '.'

static void initialize(int rows, int cols, char **grid) {
    /* Seed the grid with a 20%% chance of life in each cell. */
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            grid[r][c] = (rand() % 5 == 0) ? ALIVE : DEAD;
        }
    }
}

static int neighbors(int rows, int cols, char **grid, int r, int c) {
    /* Count the 8 neighbors, using wrap‑around addressing. */
    int count = 0;
    for (int dr = -1; dr <= 1; ++dr) {
        for (int dc = -1; dc <= 1; ++dc) {
            if (!dr && !dc) continue; /* skip self */
            int nr = (r + dr + rows) % rows;
            int nc = (c + dc + cols) % cols;
            if (grid[nr][nc] == ALIVE) ++count;
        }
    }
    return count;
}

static void step(int rows, int cols, char **grid, char **next) {
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            int n = neighbors(rows, cols, grid, r, c);
            if (grid[r][c] == ALIVE) {
                next[r][c] = (n == 2 || n == 3) ? ALIVE : DEAD;
            } else {
                next[r][c] = (n == 3) ? ALIVE : DEAD;
            }
        }
    }
}

static void display(int rows, int cols, char **grid) {
    for (int r = 0; r < rows; ++r) {
        fwrite(grid[r], 1, cols, stdout);
        fputc('\n', stdout);
    }
    fflush(stdout);
}

int main(int argc, char *argv[]) {
    int rows = 25, cols = 80, generations = 100;
    if (argc > 1) rows = atoi(argv[1]);
    if (argc > 2) cols = atoi(argv[2]);
    if (argc > 3) generations = atoi(argv[3]);

    /* Allocate two grids for ping‑pong buffering. */
    char **grid = malloc(rows * sizeof *grid);
    char **next = malloc(rows * sizeof *next);
    if (!grid || !next) {
        perror("malloc");
        return EXIT_FAILURE;
    }
    for (int r = 0; r < rows; ++r) {
        grid[r] = malloc(cols);
        next[r] = malloc(cols);
        if (!grid[r] || !next[r]) {
            perror("malloc");
            return EXIT_FAILURE;
        }
    }

    srand(0);
    initialize(rows, cols, grid);

    for (int gen = 0; gen < generations; ++gen) {
        // printf("\033[H\033[J"); /* clear screen */
        printf("Generation %d\n", gen);
        display(rows, cols, grid);
        step(rows, cols, grid, next);

        /* Swap the buffers. */
        char **tmp = grid;
        grid = next;
        next = tmp;

        // usleep(100000); /* 100 ms delay */
    }

    /* Clean up. */
    for (int r = 0; r < rows; ++r) {
        free(grid[r]);
        free(next[r]);
    }
    free(grid);
    free(next);
    return 0;
}

