#!/usr/bin/env python3
"""
Gray code lists (no imports) — "Reason why" + "Check (harness)"
================================================================

What this file does
-------------------
• Mimics:
      list(GrayCode(2).generate_gray())
      list(GrayCode(3).generate_gray())
      list(GrayCode(4).generate_gray())

• Prints, for each case:
  - Reason why: short derivation of the binary-reflected Gray code (BRGC) and why
    consecutive codes differ by exactly one bit (and the sequence is cyclic).
  - Check (harness): independent verification:
      1) length is 2^n and all codes are unique;
      2) every code is an n-bit '0'/'1' string;
      3) consecutive codes (and last→first) have Hamming distance 1;
      4) sequence equals the closed-form Gray mapping g_i = i ^ (i>>1).

Background: binary-reflected Gray code (BRGC)
---------------------------------------------
Seed: G(1) = ["0", "1"].
Induction: Given G(n-1), build G(n) by
    G(n) = ["0"+w for w in G(n-1)]  +  ["1"+w for w in reversed(G(n-1))].
This guarantees:
• Within each half, neighbors remain Hamming-1 (prefixing does not change distances).
• At the "middle" junction, "...0 + last(G(n-1))" and "1 + last(G(n-1))" differ only
  in the first bit.
• The sequence is cyclic: first = "0"*n, last = "1" + "0"*(n-1) ⇒ they differ in
  exactly one bit.

Closed form: the i-th n-bit Gray code equals the n-bit binary of g_i = i ^ (i>>1).

"""

# --------------------------- Gray code core ---------------------------

def gray_reflected(n):
    """Binary-reflected Gray code as list of n-bit strings."""
    if n <= 0:
        return [""]
    g = ["0", "1"]
    if n == 1:
        return g[:]
    for bits in range(2, n+1):
        left  = ["0"+w for w in g]
        right = ["1"+w for w in reversed(g)]
        g = left + right
    return g

def gray_formula(n):
    """Closed-form list using g_i = i ^ (i>>1), rendered as n-bit strings."""
    N = 1 << n
    out = []
    for i in range(N):
        g = i ^ (i >> 1)
        out.append(format(g, f"0{n}b"))
    return out

# --------------------------- Harness utilities ---------------------------

def hamming(a, b):
    return sum(1 for x, y in zip(a, b) if x != y)

def is_power_of_two(x):
    return x > 0 and (x & (x - 1)) == 0

def check_gray_list(n, lst):
    """Return a list of human-readable check lines."""
    N = 1 << n
    lines = []
    # 1) size and uniqueness
    lines.append(f"length = {len(lst)} (expected {N}), unique = {len(set(lst)) == len(lst)}")
    # 2) shape of each word
    ok_words = all(len(w) == n and set(w) <= {'0','1'} for w in lst)
    lines.append(f"all words are {n}-bit 0/1 strings = {ok_words}")
    # 3) neighbor Hamming-1 (incl. wrap-around)
    ham_ok = True
    worst = 0
    for i in range(N):
        d = hamming(lst[i], lst[(i+1) % N])
        worst = max(worst, d)
        if d != 1:
            ham_ok = False
    lines.append(f"adjacent Hamming distance = 1 (incl. wrap) → {ham_ok} (max seen {worst})")
    # also via integer XOR = power of two
    ints = [int(w, 2) for w in lst]
    xor_ok = all(is_power_of_two(ints[(i+1)%N] ^ ints[i]) for i in range(N))
    lines.append(f"adjacent XOR is power of two → {xor_ok}")
    # 4) equality with closed form
    cf = gray_formula(n)
    lines.append(f"equals i^(i>>1) closed-form sequence → {lst == cf}")
    return lines

# --------------------------- Simple parser for the case strings ---------------------------

def parse_gray_case(s):
    """
    Extract n from a string like: "list(GrayCode(3).generate_gray())"
    (no eval)
    """
    key = "GrayCode("
    i = s.find(key)
    if i == -1:
        raise ValueError("bad case string")
    j = s.find(")", i)
    if j == -1:
        raise ValueError("bad case string")
    n_str = s[i+len(key):j].strip()
    return int(n_str)

# --------------------------- Driver ---------------------------

if __name__ == "__main__":
    cases = [
        "list(GrayCode(2).generate_gray())",
        "list(GrayCode(3).generate_gray())",
        "list(GrayCode(4).generate_gray())",
    ]

    for c in cases:
        n = parse_gray_case(c)
        seq = gray_reflected(n)

        print("=" * 72)
        print(f"{c} = {seq}")

        print("Reason why:")
        print("  We use the binary-reflected Gray code (reflect-and-prefix):")
        print("    G(1) = ['0','1'],  G(n) = ['0'+G(n-1)] + ['1'+reverse(G(n-1))].")
        print("  Prefixing preserves Hamming distances within halves,")
        print("  the middle junction flips only the new leading bit,")
        print("  and last = '1' + '0'*(n-1) differs from first = '0'*n in 1 bit ⇒ cyclic.")

        print("Check (harness):")
        for line in check_gray_list(n, seq):
            print(" ", line)

