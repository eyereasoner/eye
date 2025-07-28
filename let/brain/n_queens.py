def solve_n_queens(n):
    """
    Solve the N-Queens problem for an n x n board.

    Returns a list of solutions, where each solution is a list of column indices.
    The index in the list represents the row, and the value at that index is the column.
    """
    solutions = []
    cols = set()            # Columns where queens are already placed
    pos_diagonals = set()   # Positive-slope diagonals (r + c) under attack
    neg_diagonals = set()   # Negative-slope diagonals (r - c) under attack
    board = [-1] * n        # board[r] = c indicates a queen at (r, c)

    def backtrack(row):
        """
        Attempt to place a queen in the given row and recurse for subsequent rows.
        When row == n, all queens are placed and we record the solution.
        """
        # If we've placed queens in all rows, record the solution
        if row == n:
            solutions.append(board.copy())
            return

        # Try placing a queen in each column of the current row
        for col in range(n):
            # Check if the column or diagonals are under attack
            if col in cols or (row + col) in pos_diagonals or (row - col) in neg_diagonals:
                continue  # Skip unsafe positions

            # Place the queen at (row, col)
            cols.add(col)
            pos_diagonals.add(row + col)
            neg_diagonals.add(row - col)
            board[row] = col

            # Move on to place queen in the next row
            backtrack(row + 1)

            # Backtrack: remove the queen and free up column/diagonals
            cols.remove(col)
            pos_diagonals.remove(row + col)
            neg_diagonals.remove(row - col)
            board[row] = -1

    # Start the recursive backtracking from the first row
    backtrack(0)
    return solutions


if __name__ == "__main__":
    n = 8  # Board size (8x8)
    solutions = solve_n_queens(n)
    print(f"Total solutions for {n}x{n} board: {len(solutions)}")
    # Print each solution as a list of column positions
    for idx, solution in enumerate(solutions, start=1):
        print(f"Solution {idx}: {solution}")

