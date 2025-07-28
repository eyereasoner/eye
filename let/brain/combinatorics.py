import itertools
import math

def advanced_combinatorics(elements):
    n = len(elements)
    print(f"Elements: {elements}")
    print(f"Total number of subsets: 2^{n} = {2**n}\n")
    
    # List all subsets (the power set)
    print("All subsets:")
    for r in range(n + 1):
        for subset in itertools.combinations(elements, r):
            print(subset)
    print("\n---\n")
    
    # Combinations and permutations for various k
    for k in range(1, n + 1):
        c = math.comb(n, k)
        p = math.perm(n, k)
        print(f"k = {k}:")
        print(f"  Number of combinations C({n},{k}) = {c}")
        print(f"  All combinations of size {k}: {list(itertools.combinations(elements, k))}")
        print(f"  Number of permutations P({n},{k}) = {p}")
        print(f"  All permutations of length {k}: {list(itertools.permutations(elements, k))}\n")
    
    # Combinations with repetition (multiset combinations)
    k = 2
    cwr = math.comb(n + k - 1, k)
    print(f"Combinations with repetition: choosing {k} from {n} with repetition allowed")
    print(f"  Number of multiset combinations: C({n + k - 1},{k}) = {cwr}")
    print(f"  List: {list(itertools.combinations_with_replacement(elements, k))}")

if __name__ == "__main__":
    sample_elements = ['A', 'B', 'C', 'D', 'E']
    advanced_combinatorics(sample_elements)

