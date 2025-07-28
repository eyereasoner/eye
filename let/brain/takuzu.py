# takuzu.py  –  Find *all* solutions of a 6×6 Takuzu / Binairo puzzle
# -------------------------------------------------------------------
#  -1  → blank       (you may also keep the EMPTY alias if you prefer)
#   0  → digit 0
#   1  → digit 1
#
#  The rules enforced are the classic four:
#    1. 3 zeros and 3 ones in every row
#    2. 3 zeros and 3 ones in every column
#    3. no "000" / "111" triples horizontally or vertically
#    4. finished rows and columns are pair-wise unique
#
#  The one public function, `solve_all(grid, limit=None)`, returns a Python
#  list containing *every* valid completed grid.  If `limit` is a positive
#  integer, the search stops once that many solutions have been found
#  (useful when you only need to know whether a puzzle is unique).

from typing import List, Optional, Tuple

N      = 6
HALF   = N // 2       # = 3
EMPTY  = -1
Grid   = List[List[int]]


# ────────────────────────────── core solver ──────────────────────────────
def solve_all(grid: Grid, limit: Optional[int] = None) -> List[Grid]:
    """Return a list with *all* solutions.  If `limit` is given, stop early."""
    solutions: List[Grid] = []

    def backtrack() -> None:
        # Early exit if we've hit the caller-requested limit
        if limit is not None and len(solutions) >= limit:
            return

        r, c = _find_empty(grid)
        if r is None:                         # fully filled → record a solution
            solutions.append([row[:] for row in grid])
            return

        for val in (0, 1):
            grid[r][c] = val
            if _is_locally_valid(grid, r, c):
                backtrack()
            grid[r][c] = EMPTY                # undo

    backtrack()
    return solutions


# ────────────────────────────── utilities ───────────────────────────────
def _find_empty(g: Grid) -> Tuple[Optional[int], Optional[int]]:
    for i in range(N):
        for j in range(N):
            if g[i][j] == EMPTY:
                return i, j
    return None, None


def _is_locally_valid(g: Grid, r: int, c: int) -> bool:
    return (
        _check_line(g[r])                                # row rules
        and _check_line([g[i][c] for i in range(N)])     # col rules
        and _unique_rows(g, r)
        and _unique_cols(g, c)
    )


def _check_line(line: List[int]) -> bool:
    zeros = line.count(0)
    ones  = line.count(1)

    if zeros > HALF or ones > HALF:
        return False
    if EMPTY not in line and zeros != HALF:
        return False

    # no three consecutive identical digits
    for i in range(N - 2):
        if line[i] != EMPTY and line[i] == line[i + 1] == line[i + 2]:
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


# ─────────────────────────────── demo ───────────────────────────────────
if __name__ == "__main__":
    puzzle = [
        [EMPTY, 0,     1,     EMPTY, EMPTY, EMPTY],
        [EMPTY, EMPTY, EMPTY, 0,     EMPTY, 1    ],
        [1,     EMPTY, EMPTY, EMPTY, 0,     EMPTY],
        [EMPTY, 1,     EMPTY, EMPTY, EMPTY, 0    ],
        [EMPTY, EMPTY, 0,     EMPTY, 1,     EMPTY],
        [0,     EMPTY, EMPTY, EMPTY, EMPTY, EMPTY],
    ]

    sols = solve_all(puzzle)        # or solve_all(puzzle, limit=2) etc.
    print(f"{len(sols)} solution(s) found.\n")

    for n, sol in enumerate(sols, 1):
        print(f"Solution #{n}")
        for row in sol:
            print(*row)
        print()

