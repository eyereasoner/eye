"""
8-point FFT (no imports) with "explain-and-check"
=================================================

What this file does
-------------------
• Accepts inputs written as strings like:
      "fft([0, 1, 2, 3, 4, 5, 6, 7])"
  (matching your SymPy-call style) — no eval, no imports.
• Implements an in-place radix-2 **Cooley–Tukey FFT** for N = 8 using only
  built-in complex numbers and a **precomputed W_8 twiddle table**
  (values are multiples of π/4, so they equal {±1, 0, ±√2/2} combinations).
• Prints the FFT result in the same style as the original.
• Runs a proof harness:
    1) Cross-check against a **naïve O(N²) DFT** (same twiddle table, exact).
    2) **Inverse FFT** via conjugation: IFFT(FFT(x)) == x (max error).
    3) **Parseval** (forward unscaled, inverse 1/N): Σ|x|² = (1/N)Σ|X|².
    4) **Conjugate symmetry** (for real inputs): X[N-k] = conj(X[k]).

Conventions
-----------
• DFT sign:  X[k] = Σ_{n=0}^{N-1} x[n] * exp(-2πi * k n / N).  (Same as SymPy’s `fft`.)
• Normalization: forward **unscaled**; inverse has the 1/N factor.
• N is fixed to 8 for this script (you can extend by adding twiddle tables for
  other power-of-two sizes, or by a small sin/cos routine if you prefer).

Why this is correct (sketch)
----------------------------
• Cooley–Tukey factors the N-point DFT into log2(N) butterfly stages that reuse
  sub-DFTs and twiddle multiplications; with exact W_8 values we avoid trig
  round-off entirely here.
• The checks cover algebraic equality (naïve DFT), inversion property (IFFT),
  energy preservation (Parseval), and structure (conjugate symmetry for real x).

"""

# --------------------------- Twiddles & utilities ---------------------------

def twiddle_8_table():
    """
    Return [W_8^m] for m=0..7 where W_8 = exp(-2πi/8) = exp(-iπ/4) = (√2/2) - i(√2/2).
    """
    s = (2 ** 0.5) / 2.0  # √2/2
    return [
        1 + 0j,         # m=0
        s - 1j*s,       # m=1
        0 - 1j,         # m=2
        -s - 1j*s,      # m=3
        -1 + 0j,        # m=4
        -s + 1j*s,      # m=5
        0 + 1j,         # m=6
        s + 1j*s,       # m=7
    ]

W8 = twiddle_8_table()

def bit_reverse_reorder_inplace(a):
    """In-place bit-reversal for N=8 (indices 0..7)."""
    # Precomputed 3-bit reversals: 0->0,1->4,2->2,3->6,4->1,5->5,6->3,7->7
    order = [0, 4, 2, 6, 1, 5, 3, 7]
    for i in range(8):
        j = order[i]
        if j > i:
            a[i], a[j] = a[j], a[i]

def fft8(x):
    """
    In-place radix-2 decimation-in-time FFT for N=8, forward (unscaled).
    x: list of 8 numbers (real or complex). Returns a new list (does not modify input).
    """
    if len(x) != 8:
        raise ValueError("This demo only handles N=8.")
    a = [complex(v) for v in x]  # copy as complex
    bit_reverse_reorder_inplace(a)

    # stages: m = 2, 4, 8
    m = 2
    N = 8
    while m <= N:
        half = m // 2
        stride = N // m  # twiddle step in W8
        for k in range(0, N, m):
            for j in range(half):
                w = W8[(j * stride) % 8]
                t = w * a[k + j + half]
                u = a[k + j]
                a[k + j] = u + t
                a[k + j + half] = u - t
        m *= 2
    return a

def ifft8(X):
    """Inverse FFT via conjugation trick: IFFT(X) = conj( FFT(conj(X)) ) / N."""
    conjX = [z.conjugate() for z in X]
    y = fft8(conjX)
    N = 8
    return [z.conjugate() / N for z in y]

def dft8_naive(x):
    """Naïve O(N²) DFT using the same W8 table for exactness."""
    N = 8
    X = []
    for k in range(N):
        acc = 0+0j
        for n in range(N):
            acc += complex(x[n]) * W8[(k*n) % 8]
        X.append(acc)
    return X

def parse_fft_case(case_str):
    """
    Parse "fft([a, b, c, ...])" into a Python list of numbers (ints).
    No eval. Accepts spaces.
    """
    s = case_str.strip()
    head = "fft("
    tail = ")"
    a = s.find(head)
    b = s.rfind(tail)
    if a == -1 or b == -1 or b <= a:
        raise ValueError("Bad case string: " + case_str)
    inside = s[a + len(head): b].strip()
    if not (inside.startswith("[") and inside.endswith("]")):
        raise ValueError("Expected a list in: " + case_str)
    inner = inside[1:-1].strip()
    if inner == "":
        return []
    parts = [p.strip() for p in inner.split(",")]
    out = []
    for p in parts:
        # integers are enough for the provided cases; support floats too.
        if any(ch in p for ch in ".eE"):
            out.append(float(p))
        else:
            out.append(int(p))
    return out

def fmt_c(z, eps=1e-12):
    """
    Pretty complex printer:
    • near-zero imag -> real
    • near-integer real/imag -> show as ints
    """
    a = z.real
    b = z.imag
    # snap near-ints
    ra = int(round(a))
    rb = int(round(b))
    if abs(a - ra) < eps: a = ra
    if abs(b - rb) < eps: b = rb
    if abs(b) < eps:
        # pure real
        if isinstance(a, int) or (abs(a - int(a)) < eps):
            return str(int(round(a)))
        return f"{a:.12g}"
    # general complex a+bi
    # show as Python-like "(a+bj)"
    if isinstance(a, int) or abs(a - int(a)) < eps:
        a_str = str(int(round(a)))
    else:
        a_str = f"{a:.12g}"
    if isinstance(b, int) or abs(b - int(b)) < eps:
        b_str = str(int(round(b)))
    else:
        b_str = f"{b:.12g}"
    return f"({a_str}{'+' if b>=0 else ''}{b_str}j)"

def fmt_list(lst):
    return "[" + ", ".join(fmt_c(z) for z in lst) + "]"

def max_abs_diff(a, b):
    return max(abs(a[i] - b[i]) for i in range(len(a)))

def energy(v):
    return sum((abs(complex(x))**2 for x in v))

# --------------------------- Driver ---------------------------

if __name__ == "__main__":
    # Show our twiddle table once (explanation)
    print("Using W_8^m = exp(-2πi m / 8) table:")
    for m, val in enumerate(W8):
        print(f"  W_8^{m} = {fmt_c(val)}")

    cases = [
        "fft([0, 1, 2, 3, 4, 5, 6, 7])",
        "fft([0, 1, 2, 3, 0, 1, 2, 3])",
        "fft([0, 1, 0, 1, 0, 1, 0, 1])",
        "fft([0, 0, 0, 0, 0, 0, 0, 0])",
    ]

    for c in cases:
        print("=" * 72)
        x = parse_fft_case(c)

        # Explain: quick summary for the pattern
        print("Case:", c)
        print("  Input length N=8, forward DFT convention X[k] = Σ x[n] W_8^{kn}.")
        if all(isinstance(v, (int, float)) for v in x):
            if all(v == 0 for v in x):
                print("  Explain: All zeros -> all spectral bins are zero.")
            elif all(x[i] == x[i % 4] for i in range(8)):
                print("  Explain: 4-sample pattern repeated twice -> strong energy at k multiples of 2.")
            elif all(x[i] == x[i % 2] for i in range(8)):
                print("  Explain: 2-sample pattern repeated -> energy concentrated at DC and k=4 (Nyquist).")
            elif x == list(range(8)):
                print("  Explain: Ramp 0..7 -> large DC (sum=28) and alternating complex bins elsewhere.")

        # FFT
        X = fft8(x)
        print(f"{c} = {fmt_list(X)}")

        # --- Proof harness ---
        print("\n  Checks:")
        # 1) Cross-check with naïve DFT
        X_naive = dft8_naive(x)
        print("    Naïve DFT max |Δ|  :", f"{max_abs_diff(X, X_naive):.3g}")

        # 2) Inverse FFT round-trip
        xr = ifft8(X)
        max_rt = max(abs(complex(x[i]) - xr[i]) for i in range(8))
        print("    IFFT(FFT(x)) max |Δ|:", f"{max_rt:.3g}")

        # 3) Parseval (forward unscaled, inverse 1/N)
        Ex = energy(x)
        EX = energy(X)
        parseval_err = abs(Ex - EX/8)
        print("    Parseval check     :", f"Σ|x|^2={Ex:.12g}, (1/N)Σ|X|^2={EX/8:.12g}, |Δ|={parseval_err:.3g}")

        # 4) Conjugate symmetry (only meaningful for real inputs)
        if all(abs(v.imag if isinstance(v, complex) else 0) < 1e-15 for v in x):
            N = 8
            symm_max = 0.0
            for k in range(1, N):
                symm_max = max(symm_max, abs(X[N-k] - X[k].conjugate()))
            print("    Conjugate symmetry :", f"max |X[N-k]-conj(X[k])| = {symm_max:.3g}")
        else:
            print("    Conjugate symmetry : (skipped; input not purely real)")

