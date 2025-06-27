'''Python program to find all Hamiltonian Paths in a given graph using backtracking.'''

def all_hamiltonian_paths(graph):
    """
    Finds all Hamiltonian paths in the graph.
    :param graph: adjacency matrix representation of the graph
    :return: a list of lists, each representing a Hamiltonian path
    """
    n = len(graph)
    path = [-1] * n
    all_paths = []

    def is_safe(v, pos):
        # Adjacent to previous vertex
        if not graph[path[pos - 1]][v]:
            return False
        # Not already in path
        if v in path:
            return False
        return True

    def backtrack(pos):
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

if __name__ == "__main__":
    # Example: complex graph with 6 vertices
    graph_example = [
        [0, 1, 1, 0, 0, 0],  # 0
        [1, 0, 1, 1, 0, 0],  # 1
        [1, 1, 0, 1, 1, 0],  # 2
        [0, 1, 1, 0, 1, 1],  # 3
        [0, 0, 1, 1, 0, 1],  # 4
        [0, 0, 0, 1, 1, 0],  # 5
    ]

    results = all_hamiltonian_paths(graph_example)
    if results:
        print(f"Found {len(results)} Hamiltonian path(s):")
        for p in results:
            print(p)
    else:
        print("No Hamiltonian Path exists in the graph.")

