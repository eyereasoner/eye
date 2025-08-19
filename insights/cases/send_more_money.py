"""
SEND + MORE = MONEY (pruned, deterministic, pure Python)

Key improvements vs. naive brute force:
- Use column-wise constraints to prune the search early:
  * The result has 5 digits ⇒ M = 1
  * Thousands column forces O = 0 and S = 9 - c3
  * Carry relations tie digits tightly, slashing the search space
- Deterministic iteration order (no set iteration order surprises).
- Prints output in three sections: "Answer", "Reason why", "Check (harness)".
"""

def solve_send_more_money():
    M, O = 1, 0  # From reasoning: 5-digit sum ⇒ M=1; thousands column forces O=0
    # Deterministic loops over digits
    for E in range(10):
        if E in (M, O):
            continue

        for c1 in (0, 1):  # carry from units
            for D in range(10):
                if D in (M, O, E):
                    continue
                Y = (D + E) % 10
                if (D + E) // 10 != c1:
                    continue
                if Y in (M, O, E, D):
                    continue

                for N in range(10):
                    if N in (M, O, E, D, Y):
                        continue

                    for R in range(10):
                        if R in (M, O, E, D, Y, N):
                            continue

                        # Tens column: N + R + c1 ends in E, carry c2
                        if (N + R + c1) % 10 != E:
                            continue
                        c2 = (N + R + c1) // 10

                        # Hundreds column (O=0): E + 0 + c2 ends in N, carry c3
                        if (E + O + c2) % 10 != N:
                            continue
                        c3 = (E + O + c2) // 10

                        # Thousands column: S + M + c3 = 10*c4 + O (with O=0)
                        # Since the sum is 5 digits, c4 must be 1 ⇒ S + 1 + c3 = 10 ⇒ S = 9 - c3
                        S = 10 - (M + c3)
                        if S == 0 or S in (M, O, E, D, Y, N, R):
                            continue

                        # Build numbers
                        send  = 1000*S + 100*E + 10*N + D
                        more  = 1000*M + 100*O + 10*R + E
                        money = 10000*M + 1000*O + 100*N + 10*E + Y

                        if send + more == money:
                            return dict(S=S, E=E, N=N, D=D, M=M, O=O, R=R, Y=Y), send, more, money

    raise ValueError("No solution found (unexpected for this puzzle).")


def print_solution():
    mapping, send, more, money = solve_send_more_money()

    # Compute carries for the explanation
    c1 = (mapping['D'] + mapping['E']) // 10
    c2 = (mapping['N'] + mapping['R'] + c1) // 10
    c3 = (mapping['E'] + mapping['O'] + c2) // 10
    c4 = 1  # 5-digit result ⇒ the final carry is 1 (and equals M)

    # ---- Answer ----
    print("Answer")
    print("------")
    print("Mapping:")
    for k in sorted(mapping):
        print(f"  {k} = {mapping[k]}")
    print()
    print("SEND + MORE = MONEY")
    print(f" {mapping['S']}{mapping['E']}{mapping['N']}{mapping['D']}")
    print(f"+{mapping['M']}{mapping['O']}{mapping['R']}{mapping['E']}")
    print("-----")
    print(f"{mapping['M']}{mapping['O']}{mapping['N']}{mapping['E']}{mapping['Y']}")

    # ---- Reason why ----
    print("\nReason why")
    print("----------")
    print("1) The sum has 5 digits, so M = 1.")
    print("2) Thousands column forces O = 0 and S + M + c3 = 10 ⇒ S = 9 - c3.")
    print(f"3) Column carries (right→left): c1={c1}, c2={c2}, c3={c3}, c4={c4}.")
    print("4) Column equations satisfied:")
    print(f"   • D + E = 10*c1 + Y  → {mapping['D']} + {mapping['E']} = {10*c1 + mapping['Y']}")
    print(f"   • N + R + c1 = 10*c2 + E → {mapping['N']} + {mapping['R']} + {c1} = {10*c2 + mapping['E']}")
    print(f"   • E + O + c2 = 10*c3 + N → {mapping['E']} + {mapping['O']} + {c2} = {10*c3 + mapping['N']}")
    print(f"   • S + M + c3 = 10*c4 + O → {mapping['S']} + {mapping['M']} + {c3} = {10*c4 + mapping['O']}")
    print("5) All letters map to distinct digits; no leading letter is zero.")

    # ---- Check (harness) ----
    print("\nCheck (harness)")
    print("----------------")
    unique = len(set(mapping.values())) == 8
    no_leading_zero = (mapping['S'] != 0 and mapping['M'] != 0)
    equation_ok = (send + more == money)
    print(f"Unique digits: {unique}")
    print(f"No leading zeros: {no_leading_zero}")
    print(f"Equation holds: {send} + {more} = {money} → {equation_ok}")


if __name__ == "__main__":
    print_solution()

