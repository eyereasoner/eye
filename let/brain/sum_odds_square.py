# ============================================
# Explain-and-Check: Induction Example
# Claim: 1 + 3 + 5 + ... + (2n-1) = n^2  for all n ≥ 1
# ============================================
# What you'll see:
#   • A clear "reason why" proof by induction (base case + inductive step).
#   • A geometric intuition (adding an L-shaped border to grow squares).
#   • A proof harness that verifies the identity for many n, quickly, with integers only.
#
# No imports. No user input.

# -----------------------------
# Program output: the “reason why”
# -----------------------------

print("============================================")
print("Induction Example — Sum of first n odds = n^2")
print("============================================\n")

print("Claim:")
print("  For every n ≥ 1,  1 + 3 + 5 + ... + (2n−1) = n².\n")

print("Proof (by induction on n):")
print("  Base case (n=1):")
print("    Left side = 1,  Right side = 1² = 1.  True.\n")

print("  Inductive step:")
print("    Assume for some n ≥ 1 that  1 + 3 + ... + (2n−1) = n².  (Induction hypothesis)")
print("    Then for n+1 we have:")
print("      1 + 3 + ... + (2n−1) + (2(n+1)−1)")
print("      = n² + (2n + 1)                 (by the hypothesis)")
print("      = (n + 1)².                     (algebra)")
print("    So the statement holds for n+1.  By induction, it holds for all n ≥ 1.  □\n")

print("Geometric intuition (why it *feels* true):")
print("  Arrange dots as an n×n square (n² dots). To grow to (n+1)²,")
print("  you add an L-shaped border of exactly 2n+1 dots — the next odd number.")
print("  Repeating this from 1² adds 1, then 3, then 5, ... building perfect squares at each step.\n")

# -----------------------------
# Proof harness (checks many n)
# -----------------------------
# We verify the identity for n = 1..MAX_N by accumulating the odd numbers
# incrementally. This keeps the check O(MAX_N) with simple integer arithmetic.

def harness(MAX_N=200_000):
    """
    Incrementally check that sum_{k=1..n} (2k-1) == n^2 for many n.
    No imports, integers only, linear time.
    """
    odd_sum = 0
    for n in range(1, MAX_N + 1):
        odd_sum += (2*n - 1)
        # Identity to check: odd_sum == n*n
        assert odd_sum == n*n, f"Failed at n={n}: sum={odd_sum}, n^2={n*n}"
    return MAX_N

if __name__ == "__main__":
    tested = harness()  # fast: ~200k integer steps
    print(f"Proof harness: verified the identity for n = 1..{tested}. ✓")

