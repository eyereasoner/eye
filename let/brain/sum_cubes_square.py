# ============================================
# Explain-and-Check: Induction (Sum of Cubes)
# Claim: For all n ≥ 1,  1^3 + 2^3 + ... + n^3 = ( n(n+1)/2 )^2
# ============================================
# What you'll see:
#  • A clean proof by induction (base case + inductive step with explicit algebra).
#  • A short intuition note tying it to triangular numbers.
#  • A proof harness that checks the identity for many n using integer arithmetic only.
#
# No imports. No user input.

# -----------------------------
# Program output: the “reason why”
# -----------------------------

print("============================================")
print("Induction Example — Sum of cubes equals a square")
print("============================================\n")

print("Claim:")
print("  For every n ≥ 1,  1^3 + 2^3 + ··· + n^3 = ( n(n+1)/2 )^2.\n")

print("Proof (by induction on n):")
print("  Base case (n=1):")
print("    LHS = 1^3 = 1,   RHS = (1·2/2)^2 = 1.  True.\n")

print("  Inductive step:")
print("    Assume for some n ≥ 1 that  S(n) := 1^3 + 2^3 + ··· + n^3 = [ n(n+1)/2 ]^2.")
print("    We must show  S(n+1) = S(n) + (n+1)^3 = [ (n+1)(n+2)/2 ]^2.\n")

print("    Start from the hypothesis and add (n+1)^3:")
print("      S(n+1) = [ n(n+1)/2 ]^2 + (n+1)^3")
print("              = (n+1)^2 · ( n^2/4 + (n+1) )")
print("              = (n+1)^2 · ( (n^2 + 4n + 4) / 4 )")
print("              = (n+1)^2 · ( (n+2)^2 / 4 )")
print("              = [ (n+1)(n+2) / 2 ]^2.")
print("    Thus S(n) ⇒ S(n+1). By induction, the identity holds for all n ≥ 1.  □\n")

print("Intuition (why it feels right):")
print("  The triangular number T_n = 1 + 2 + ··· + n equals n(n+1)/2.")
print("  The claim says the sum of the first n cubes equals T_n^2 — the square of a triangle of dots.")
print("  Visual proofs arrange n^3 blocks into growing square patterns; each step adds a 'thickened L'.\n")

# -----------------------------
# Proof harness (integer checks)
# -----------------------------
# We verify for n = 1..MAX_N in two ways:
#   (1) Incremental sum of cubes
#   (2) Closed form with triangular numbers
# and assert equality at every step. Also check the telescoping step:
#   S(n+1) - S(n) = (n+1)^3.

def harness(MAX_N=200_000):
    sum_cubes = 0
    prev_sum  = 0
    for n in range(1, MAX_N + 1):
        # Incremental left-hand side
        sum_cubes += n*n*n

        # Right-hand side via triangular number squared
        Tn = n * (n + 1) // 2
        rhs = Tn * Tn

        # Equality check
        assert sum_cubes == rhs, f"Fail at n={n}: sum={sum_cubes}, rhs={rhs}"

        # Difference check equals (n)^3 when going from n-1 to n
        if n > 1:
            assert sum_cubes - prev_sum == n*n*n, \
                f"Telescoping fail at n={n}: Δ={sum_cubes - prev_sum}, expected {n*n*n}"

        prev_sum = sum_cubes
    return MAX_N

if __name__ == "__main__":
    tested = harness()
    print(f"Proof harness: verified for n = 1..{tested}. All checks passed. ✓")

