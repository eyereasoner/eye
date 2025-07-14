"""
SEND + MORE = MONEY solver (Pure Python)

This script solves the classic cryptarithm where each letter represents a unique digit (0–9),
and the arithmetic equation must hold:

    SEND
  + MORE
  -------
   MONEY

Constraints:
- Each letter maps to a unique digit.
- S and M cannot be zero (numbers can't start with 0).
- The equation SEND + MORE == MONEY must be satisfied.

This version uses brute-force search (2560 permutations) and includes
deterministic output with a human-readable proof of correctness.
"""

from itertools import permutations

def solve_send_more_money():
    # The distinct letters in the puzzle
    letters = ['S', 'E', 'N', 'D', 'M', 'O', 'R', 'Y']

    # All digits 0–9 available for assignment to letters
    digits = range(10)

    # Try all possible permutations of 8 different digits for the 8 letters
    for perm in permutations(digits, len(letters)):
        # Map each letter to its assigned digit
        mapping = dict(zip(letters, perm))

        # Skip if S or M is assigned 0 (numbers can't start with zero)
        if mapping['S'] == 0 or mapping['M'] == 0:
            continue

        # Construct the numerical values:
        # SEND = 1000*S + 100*E + 10*N + D
        send = 1000 * mapping['S'] + 100 * mapping['E'] + 10 * mapping['N'] + mapping['D']
        # MORE = 1000*M + 100*O + 10*R + E
        more = 1000 * mapping['M'] + 100 * mapping['O'] + 10 * mapping['R'] + mapping['E']
        # MONEY = 10000*M + 1000*O + 100*N + 10*E + Y
        money = 10000 * mapping['M'] + 1000 * mapping['O'] + 100 * mapping['N'] + 10 * mapping['E'] + mapping['Y']

        # Check if SEND + MORE == MONEY
        if send + more == money:
            # Found a valid solution — print result and explanation
            print("Solution (deterministic, pure Python):")
            for letter in sorted(mapping):
                print(f"  {letter} = {mapping[letter]}")

            print(f"\nSEND  = {send}")
            print(f"MORE  = {more}")
            print(f"MONEY = {money}")
            print(f"\nVerified: {send} + {more} = {send + more} == {money}")

            # Detailed proof
            print("\nProof (explaining the logic):")
            print("  Each letter was assigned a unique digit.")
            print("  None of the numbers start with zero (S = {}, M = {}).".format(mapping['S'], mapping['M']))
            print("  Final equation: {} (SEND) + {} (MORE) = {} (MONEY) is satisfied.".format(send, more, money))
            print("  No digit is reused, and all conditions are respected.")
            return  # Only show the first (and only) solution

    # If no solution found (shouldn't happen in this puzzle)
    print("No solution found.")

# Run the solver
if __name__ == "__main__":
    solve_send_more_money()

