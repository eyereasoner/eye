# takuzu.py ─ A compact 6×6 Takuzu / Binairo solver
from typing import List, Optional, Tuple

N     = 6          # board is ALWAYS 6×6
HALF  = N // 2     # = 3
EMPTY = -1         # convenience alias

Grid = List[List[int]]        # -1 = blank, 0/1 = clues / solution

# ───────────────────────────────── Solver ─────────────────────────────────────
def solve(grid: Grid) -> Optional[Grid]:
    """Return a completed 6×6 grid or None if the puzzle is unsatisfiable."""
    r, c = _find_empty(grid)
    if r is None:                               # nothing left → solved
        return [row[:] for row in grid]

    for val in (0, 1):
        grid[r][c] = val
        if _is_locally_valid(grid, r, c):
            res = solve(grid)
            if res:
                return res
        grid[r][c] = EMPTY                      # undo & try the other value
    return None

# ───────────────────────────── Helper functions ──────────────────────────────
def _find_empty(grid: Grid) -> Tuple[Optional[int], Optional[int]]:
    for i in range(N):
        for j in range(N):
            if grid[i][j] == EMPTY:
                return i, j
    return None, None

def _is_locally_valid(g: Grid, r: int, c: int) -> bool:
    return (
        _check_line(g[r])                                # row
        and _check_line([g[i][c] for i in range(N)])     # column
        and _unique_rows(g, r)
        and _unique_cols(g, c)
    )

def _check_line(line: List[int]) -> bool:
    zeros, ones = line.count(0), line.count(1)
    if zeros > HALF or ones > HALF:          # over-filled
        return False
    if EMPTY not in line and zeros != HALF:  # finished but unbalanced
        return False
    # no three consecutive equals
    for i in range(N - 2):
        if line[i] != EMPTY and line[i] == line[i+1] == line[i+2]:
            return False
    return True

def _unique_rows(g: Grid, r: int) -> bool:
    row = g[r]
    if EMPTY in row:
        return True
    for i in range(N):
        if i != r and EMPTY not in g[i] and g[i] == row:
            return False
    return True

def _unique_cols(g: Grid, c: int) -> bool:
    col = [g[i][c] for i in range(N)]
    if EMPTY in col:
        return True
    for j in range(N):
        other = [g[i][j] for i in range(N)]
        if j != c and EMPTY not in other and other == col:
            return False
    return True

# ─────────────────────────────── Demo puzzle ─────────────────────────────────
if __name__ == "__main__":
    puzzle = [
        [EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY],
        [EMPTY, EMPTY,     1, EMPTY, EMPTY, EMPTY],
        [EMPTY, EMPTY, EMPTY, EMPTY, 0,     EMPTY],
        [EMPTY, EMPTY,     0, EMPTY, EMPTY, EMPTY],
        [    0,     0, EMPTY, EMPTY,     1, EMPTY],
        [0,     EMPTY, EMPTY,     0,     1, EMPTY],
    ]

    solution = solve(puzzle)
    if solution:
        print("Solved 6×6 grid:")
        for row in solution:
            print(*row)
    else:
        print("No solution found.")

