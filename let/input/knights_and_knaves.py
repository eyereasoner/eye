from itertools import product  # Used to generate all possible Knight/Knave combinations

def knights_and_knaves():
    # Define names of the 8 individuals
    names = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']

    # Define the statement each person makes.
    # Each function takes a dictionary `assignment` and returns True/False depending on whether
    # the content of the statement is logically true under that assignment.

    def A_statement(assignment):  # A says: "B is a Knave."
        return not assignment['B']

    def B_statement(assignment):  # B says: "C is a Knight."
        return assignment['C']

    def C_statement(assignment):  # C says: "D is a Knave."
        return not assignment['D']

    def D_statement(assignment):  # D says: "E and F are of the same type."
        return assignment['E'] == assignment['F']

    def E_statement(assignment):  # E says: "G is a Knave."
        return not assignment['G']

    def F_statement(assignment):  # F says: "I am a Knight."
        return assignment['F']  # Just checks whether F is a Knight (self-referential)

    def G_statement(assignment):  # G says: "H is a Knave."
        return not assignment['H']

    def H_statement(assignment):  # H says: "A is a Knight."
        return assignment['A']

    # Collect all statement functions in a list for easy iteration
    statements = [
        A_statement,
        B_statement,
        C_statement,
        D_statement,
        E_statement,
        F_statement,
        G_statement,
        H_statement
    ]

    # Try all possible combinations of Knight (True) or Knave (False) for each person
    # 2^8 = 256 possibilities — small enough for brute-force
    for truth_values in product([True, False], repeat=8):
        # Create a mapping like {'A': True, 'B': False, ...}
        assignment = dict(zip(names, truth_values))

        # Assume the assignment is valid unless proven otherwise
        valid = True

        # Check each person's statement against their role
        for i, name in enumerate(names):
            is_knight = assignment[name]              # True if the person is a Knight
            says_true = statements[i](assignment)     # Does their statement evaluate to True?

            # Knights must tell the truth; Knaves must lie
            if is_knight and not says_true:
                valid = False  # Knight made a false statement → contradiction
                break
            elif not is_knight and says_true:
                valid = False  # Knave made a true statement → contradiction
                break

        # If a valid assignment is found, print it and the logic behind it
        if valid:
            print("Solution (deterministic, pure Python):")
            for name in names:
                print(f"  {name} is a {'Knight' if assignment[name] else 'Knave'}")

            print("\nProof (explaining each statement logically):")

            # English versions of each statement for explanation
            explanations = {
                'A': "B is a Knave.",
                'B': "C is a Knight.",
                'C': "D is a Knave.",
                'D': "E and F are of the same type.",
                'E': "G is a Knave.",
                'F': "I am a Knight.",
                'G': "H is a Knave.",
                'H': "A is a Knight."
            }

            # Print logical explanation for each person
            for name in names:
                role = 'Knight' if assignment[name] else 'Knave'
                print(f"\n{name} is a {role} and says: \"{explanations[name]}\"")

                # Explain logic depending on the person
                if name == 'A':
                    print(f"  A is a {role}, and says B is a Knave → So B is {'a Knave' if not assignment['B'] else 'a Knight'}")
                elif name == 'B':
                    print(f"  B is a {role}, and says C is a Knight → So C is {'a Knight' if assignment['C'] else 'a Knave'}")
                elif name == 'C':
                    print(f"  C is a {role}, and says D is a Knave → So D is {'a Knave' if not assignment['D'] else 'a Knight'}")
                elif name == 'D':
                    same = assignment['E'] == assignment['F']
                    e_type = 'Knight' if assignment['E'] else 'Knave'
                    f_type = 'Knight' if assignment['F'] else 'Knave'
                    print(f"  D is a {role}, and says E and F are the same → E is {e_type}, F is {f_type} → {'same' if same else 'different'}")
                elif name == 'E':
                    print(f"  E is a {role}, and says G is a Knave → G is {'a Knave' if not assignment['G'] else 'a Knight'}")
                elif name == 'F':
                    print(f"  F is a {role}, and says they are a Knight → Statement is {'true' if assignment['F'] else 'false'}")
                elif name == 'G':
                    print(f"  G is a {role}, and says H is a Knave → H is {'a Knave' if not assignment['H'] else 'a Knight'}")
                elif name == 'H':
                    print(f"  H is a {role}, and says A is a Knight → A is {'a Knight' if assignment['A'] else 'a Knave'}")
            return  # Stop after the first valid solution is found

    # If no assignment satisfies the puzzle, report it
    print("No solution exists.")

# Run the puzzle solver
if __name__ == "__main__":
    knights_and_knaves()

