#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define ROW 25
#define COL 80
#define OBSTACLE_PROB 30  // percentage chance of an obstacle per cell

// Directions: up, right, down, left
int dRow[] = { -1, 0, 1, 0 };
int dCol[] = { 0, 1, 0, -1 };

typedef struct {
    int row, col;
} Point;

int isValid(int grid[ROW][COL], int visited[ROW][COL], int r, int c) {
    return r >= 0 && r < ROW && c >= 0 && c < COL &&
           grid[r][c] == 0 && !visited[r][c];
}

int lee(int grid[ROW][COL], Point src, Point dst) {
    int visited[ROW][COL] = {0};
    int dist[ROW][COL];
    Point prev[ROW][COL];
    for (int i = 0; i < ROW; i++)
        for (int j = 0; j < COL; j++)
            dist[i][j] = -1;

    Point *queue = malloc(ROW * COL * sizeof(Point));
    int front = 0, rear = 0;

    queue[rear++] = src;
    visited[src.row][src.col] = 1;
    dist[src.row][src.col] = 0;
    prev[src.row][src.col] = (Point){-1, -1};

    while (front < rear) {
        Point p = queue[front++];
        if (p.row == dst.row && p.col == dst.col)
            break;
        for (int i = 0; i < 4; i++) {
            int nr = p.row + dRow[i], nc = p.col + dCol[i];
            if (isValid(grid, visited, nr, nc)) {
                queue[rear++] = (Point){nr, nc};
                visited[nr][nc] = 1;
                dist[nr][nc] = dist[p.row][p.col] + 1;
                prev[nr][nc] = p;
            }
        }
    }

    if (!visited[dst.row][dst.col]) {
        free(queue);
        return 0;
    }

    Point cur = dst;
    while (cur.row != src.row || cur.col != src.col) {
        grid[cur.row][cur.col] = 2;
        cur = prev[cur.row][cur.col];
    }
    grid[src.row][src.col] = 3;
    grid[dst.row][dst.col] = 4;
    free(queue);
    return 1;
}

void printGrid(int grid[ROW][COL]) {
    for (int i = 0; i < ROW; i++) {
        for (int j = 0; j < COL; j++) {
            char c;
            switch (grid[i][j]) {
                case -1: c = 'o'; break;
                case  0: c = '.'; break;
                case  2: c = '*'; break;
                case  3: c = 'S'; break;
                case  4: c = 'D'; break;
                default: c = '?';
            }
            putchar(c);
        }
        putchar('\n');
    }
}

int main() {
    int grid[ROW][COL];
    srand(0);
    for (int i = 0; i < ROW; i++) {
        for (int j = 0; j < COL; j++) {
            grid[i][j] = (rand() % 100) < OBSTACLE_PROB ? -1 : 0;
        }
    }

    Point src = {0, 0};
    Point dst = {ROW - 1, COL - 1};
    grid[src.row][src.col] = 0;
    grid[dst.row][dst.col] = 0;

    if (lee(grid, src, dst)) {
        printf("Path found:\n");
        printGrid(grid);
    } else {
        printf("No path exists\n");
    }
    return 0;
}

