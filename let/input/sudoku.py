"""Simple Sudoku solver using recursive backtracking.

Run this file directly to see a demo with a sample puzzle.

Board representation:
    - A 9×9 Python list of lists (typing alias `Board`).
    - Digits 1‒9 for filled cells, 0 for blanks.

Main entry points:
    * solve(board)  – mutates the board in place; returns True if solved, False otherwise.
    * print_board(board) – pretty‑prints the board to stdout.
"""
from __future__ import annotations
from typing import List, Tuple, Optional

Board = List[List[int]]

def print_board(board: Board) -> None:
    """Pretty‑print a Sudoku board to the console (0 shown as '.')"""
    for i, row in enumerate(board):
        if i % 3 == 0 and i != 0:
            print("-" * 21)
        for j, val in enumerate(row):
            if j % 3 == 0 and j != 0:
                print("|", end=" ")
            print(val if val != 0 else ".", end=" ")
        print()

def find_empty(board: Board) -> Optional[Tuple[int, int]]:
    """Return position (row, col) of next empty cell, or None if solved."""
    for r in range(9):
        for c in range(9):
            if board[r][c] == 0:
                return r, c
    return None

def is_valid(board: Board, num: int, pos: Tuple[int, int]) -> bool:
    """Check whether placing *num* at *pos* keeps the board valid."""
    r, c = pos
    # Row & column
    if any(board[r][i] == num for i in range(9) if i != c):
        return False
    if any(board[i][c] == num for i in range(9) if i != r):
        return False
    # 3×3 sub‑grid
    box_row, box_col = 3 * (r // 3), 3 * (c // 3)
    for i in range(box_row, box_row + 3):
        for j in range(box_col, box_col + 3):
            if (i, j) != pos and board[i][j] == num:
                return False
    return True

def solve(board: Board) -> bool:
    """Solve *board* in place using DFS backtracking. Return True if solved."""
    spot = find_empty(board)
    if spot is None:
        return True  # Board solved
    r, c = spot
    for num in range(1, 10):
        if is_valid(board, num, (r, c)):
            board[r][c] = num
            if solve(board):
                return True
            board[r][c] = 0  # Undo & backtrack
    return False  # Trigger backtracking

if __name__ == "__main__":
    example: Board = [
        [1, 0, 0, 8, 0, 4, 0, 0, 0],
        [0, 2, 0, 0, 0, 0, 4, 5, 6],
        [0, 0, 3, 2, 0, 5, 0, 0, 0],
        [0, 0, 0, 4, 0, 0, 8, 0, 5],
        [7, 8, 9, 0, 5, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 6, 2, 0, 3],
        [8, 0, 1, 0, 0, 0, 7, 0, 0],
        [0, 0, 0, 1, 2, 3, 0, 8, 0],
        [2, 0, 5, 0, 0, 0, 0, 0, 9]
    ]

    print("Puzzle:\n")
    print_board(example)
    if solve(example):
        print("\nSolution:\n")
        print_board(example)
    else:
        print("No solution exists")
