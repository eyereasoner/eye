"""
hamiltonian_paths.py
────────────────────

Python program to find all Hamiltonian paths in a given graph using backtracking.

Definition:
    A Hamiltonian path in a graph is a path that visits each vertex exactly once.

Algorithm:
    - Try each vertex as a starting point.
    - Use backtracking to explore all paths that visit each vertex once.
    - Collect all such paths.

This version prints not only the result but also an explanation of correctness.
"""

from typing import List


def all_hamiltonian_paths(graph: List[List[int]]) -> List[List[int]]:
    """
    Finds all Hamiltonian paths in the graph.

    :param graph: Adjacency matrix of the graph
    :return: List of all Hamiltonian paths found
    """
    n = len(graph)
    path = [-1] * n           # Current path being explored
    all_paths = []            # List to store valid Hamiltonian paths

    def is_safe(v: int, pos: int) -> bool:
        """
        Check if vertex v can be added to the path at position pos.
        """
        if not graph[path[pos - 1]][v]:    # Must be adjacent to previous vertex
            return False
        if v in path[:pos]:                # Must not already be in the path
            return False
        return True

    def backtrack(pos: int):
        """
        Recursively build paths and collect all Hamiltonian paths.
        """
        if pos == n:
            all_paths.append(path.copy())
            return
        for v in range(n):
            if is_safe(v, pos):
                path[pos] = v
                backtrack(pos + 1)
                path[pos] = -1

    # Try each vertex as the starting point
    for start in range(n):
        path[0] = start
        backtrack(1)
        path[0] = -1

    return all_paths


def explain_results(graph: List[List[int]], paths: List[List[int]]):
    """
    Print summary explanation and reasoning of results.
    """
    n = len(graph)

    print(f"\nGraph with {n} vertices (0 to {n - 1})")
    print("Adjacency Matrix:")
    for row in graph:
        print("  ", row)

    print(f"\nFound {len(paths)} Hamiltonian path(s):")
    for i, p in enumerate(paths, 1):
        print(f"  Path {i}: {p}")

    if not paths:
        print("\nNo Hamiltonian path exists in the graph.")
        return

    print("\nExplanation:")
    print("─────────────")
    print("A Hamiltonian path must:")
    print("  • Visit each vertex exactly once.")
    print("  • Traverse only along edges in the graph.")
    print("The algorithm checks all such paths using backtracking.")
    print("Each returned path satisfies these properties.")
    print("Therefore, the output is correct and complete under exhaustive search.")


# ───────────────────────────────────────────────────────────────
# Main
# ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    # Example: graph with 6 vertices
    graph_example = [
        [0, 1, 1, 0, 0, 0],  # 0
        [1, 0, 1, 1, 0, 0],  # 1
        [1, 1, 0, 1, 1, 0],  # 2
        [0, 1, 1, 0, 1, 1],  # 3
        [0, 0, 1, 1, 0, 1],  # 4
        [0, 0, 0, 1, 1, 0],  # 5
    ]

    results = all_hamiltonian_paths(graph_example)
    explain_results(graph_example, results)

