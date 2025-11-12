"""
Matrix basics (no imports) with "explain-and-check"
===================================================

What this file does
-------------------
Reproduces the following calls WITHOUT imports:

  1) Matrix([[1, 3], [-2, 3]])*Matrix([[0, 3], [0, 7]])
  2) Matrix([[1, 3], [-2, 3]])**2
  3) Matrix([[1, 3], [-2, 3]])**-1
  4) Matrix([[1, 0, 1], [2, -1, 3], [4, 3, 2]]).det()
  5) Matrix([[3, -2, 4, -2], [5, 3, -3, -2], [5, -2, 2, -2], [5, -2, -3, 3]]).eigenvals()

For each, it prints the result in the same string form you used and then runs a
small "proof harness" to verify correctness (algebraic identities, determinants,
Cayley–Hamilton, etc.).

Implementation notes
--------------------
• Pure Python only (no imports). Small helpers implement:
  - integer/fraction formatting (we print rationals like "p/q"),
  - 2×2 inverse via adjugate/determinant,
  - det(3×3) via Sarrus and cross-check by Laplace,
  - eigenvalues for the 4×4 via the Faddeev–LeVerrier method to build the
    characteristic polynomial χ_A(λ) = det(λI−A), then integer-root factorization.
• Proof harness for the 4×4 also checks Cayley–Hamilton (χ_A(A)=0), and that
  trace(A) equals the sum of eigenvalues and det(A) equals their product.
"""

# --------------------------- Tiny fraction utils ---------------------------

def _gcd(a, b):
    a, b = abs(a), abs(b)
    while b:
        a, b = b, a % b
    return a

def make_frac(num, den):
    """Return a reduced (num, den) with den>0; num, den integers."""
    if den == 0:
        raise ZeroDivisionError("denominator 0")
    if den < 0:
        num, den = -num, -den
    g = _gcd(num, den)
    return (num // g, den // g)

def is_frac(x):
    return isinstance(x, tuple) and len(x) == 2 and all(isinstance(t, int) for t in x)

def to_frac(x):
    """Promote int -> frac; leave frac as-is."""
    if is_frac(x): return x
    return (int(x), 1)

def add_frac(a, b):
    a = to_frac(a); b = to_frac(b)
    return make_frac(a[0]*b[1] + b[0]*a[1], a[1]*b[1])

def sub_frac(a, b):
    a = to_frac(a); b = to_frac(b)
    return make_frac(a[0]*b[1] - b[0]*a[1], a[1]*b[1])

def mul_frac(a, b):
    a = to_frac(a); b = to_frac(b)
    return make_frac(a[0]*b[0], a[1]*b[1])

def div_frac(a, b):
    a = to_frac(a); b = to_frac(b)
    return make_frac(a[0]*b[1], a[1]*b[0])

def eq_frac(a, b):
    a = to_frac(a); b = to_frac(b)
    return a == b

def frac_str(x):
    x = to_frac(x)
    n, d = x
    if d == 1: return str(n)
    return f"{n}/{d}"

# --------------------------- Matrix helpers ---------------------------

def mat_str(M):
    """Show Matrix literal; entries can be ints or fracs."""
    rows = []
    for r in M:
        rows.append("[" + ", ".join(frac_str(x) for x in r) + "]")
    return "Matrix([" + ", ".join(rows) + "])"

def mat_shape(A):
    return (len(A), len(A[0]) if A else 0)

def mat_mul(A, B):
    n, m = mat_shape(A)
    m2, p = mat_shape(B)
    if m != m2:
        raise ValueError("shape mismatch")
    C = [[(0,1) for _ in range(p)] for __ in range(n)]
    for i in range(n):
        for j in range(p):
            s = (0,1)
            for k in range(m):
                s = add_frac(s, mul_frac(A[i][k], B[k][j]))
            C[i][j] = s
    return C

def mat_add(A, B):
    n, m = mat_shape(A)
    C = [[add_frac(A[i][j], B[i][j]) for j in range(m)] for i in range(n)]
    return C

def mat_sub(A, B):
    n, m = mat_shape(A)
    C = [[sub_frac(A[i][j], B[i][j]) for j in range(m)] for i in range(n)]
    return C

def mat_scalar(A, s):
    s = to_frac(s)
    n, m = mat_shape(A)
    return [[mul_frac(A[i][j], s) for j in range(m)] for i in range(n)]

def mat_eq(A, B):
    n, m = mat_shape(A)
    for i in range(n):
        for j in range(m):
            if not eq_frac(A[i][j], B[i][j]):
                return False
    return True

def eye(n):
    return [[(1,1) if i==j else (0,1) for j in range(n)] for i in range(n)]

def zeros(n, m):
    return [[(0,1) for _ in range(m)] for __ in range(n)]

def mat_pow(A, e):
    if e < 0:
        raise ValueError("negative power: use inverse for 2x2 below")
    n, m = mat_shape(A)
    if n != m:
        raise ValueError("power needs square matrix")
    R = eye(n)
    B = [row[:] for row in A]
    k = e
    while k:
        if k & 1:
            R = mat_mul(R, B)
        B = mat_mul(B, B)
        k >>= 1
    return R

def det2(A):
    return sub_frac(mul_frac(A[0][0], A[1][1]), mul_frac(A[0][1], A[1][0]))

def inv2(A):
    """Inverse of 2×2 [[a,b],[c,d]] = (1/det)*[[d,-b],[-c,a]]."""
    a, b = A[0][0], A[0][1]
    c, d = A[1][0], A[1][1]
    det = det2(A)
    if eq_frac(det, (0,1)):
        raise ZeroDivisionError("singular")
    adj = [[d, make_frac(-to_frac(b)[0], 1)],
           [make_frac(-to_frac(c)[0], 1), a]]
    inv = mat_scalar(adj, div_frac((1,1), det))
    return inv, det, adj

def det3(M):
    a,b,c = M[0]
    d,e,f = M[1]
    g,h,i = M[2]
    term1 = mul_frac(a, sub_frac(mul_frac(e,i), mul_frac(f,h)))
    term2 = mul_frac(b, sub_frac(mul_frac(d,i), mul_frac(f,g)))
    term3 = mul_frac(c, sub_frac(mul_frac(d,h), mul_frac(e,g)))
    return add_frac(sub_frac(term1, term2), term3)

def det3_sarrus_terms(M):
    # For explanation only (all ints here, but we keep frac ops to be general).
    a,b,c = M[0]; d,e,f = M[1]; g,h,i = M[2]
    pos = [mul_frac(a, mul_frac(e, i)),
           mul_frac(b, mul_frac(f, g)),
           mul_frac(c, mul_frac(d, h))]
    neg = [mul_frac(c, mul_frac(e, g)),
           mul_frac(a, mul_frac(f, h)),
           mul_frac(b, mul_frac(d, i))]
    return pos, neg

# --------------------------- Parsing ---------------------------

def parse_matrix_literal(s):
    """Parse 'Matrix([[...], [...]])' into a list of list of fractions."""
    # pull out the [[...]] part
    a = s.find("Matrix(")
    if a == -1:
        raise ValueError("no Matrix(")
    # find the matching ')'
    depth = 0
    b = a
    while b < len(s):
        if s[b] == '(':
            depth += 1
        elif s[b] == ')':
            depth -= 1
            if depth == 0:
                break
        b += 1
    inner = s[a+len("Matrix("):b]  # expects something like [[...],[...]]
    # simple bracket/number parser
    stack = []
    cur = None
    i = 0
    while i < len(inner):
        ch = inner[i]
        if ch == '[':
            stack.append([])
            i += 1
        elif ch == ']':
            last = stack.pop()
            if stack:
                stack[-1].append(last)
            else:
                cur = last
            i += 1
        elif ch == ',' or ch.isspace():
            i += 1
        else:
            # parse integer (allow leading '-')
            j = i
            if inner[j] == '-':
                j += 1
            while j < len(inner) and inner[j].isdigit():
                j += 1
            num = int(inner[i:j])
            stack[-1].append((num,1))
            i = j
    # cur should be a list of rows
    return cur

# --------------------------- Poly / eigen helpers ---------------------------

def mat_int(A):
    """Convert entries (frac) to int if denominator 1; raise otherwise."""
    n, m = mat_shape(A)
    B = [[0]*m for _ in range(n)]
    for i in range(n):
        for j in range(m):
            num, den = to_frac(A[i][j])
            if den != 1:
                raise ValueError("expected integer matrix")
            B[i][j] = num
    return B

def matmul_int(A, B):
    n = len(A); m = len(A[0]); p = len(B[0])
    C = [[0]*p for _ in range(n)]
    for i in range(n):
        for j in range(p):
            s = 0
            for k in range(m):
                s += A[i][k]*B[k][j]
            C[i][j] = s
    return C

def trace_int(A):
    return sum(A[i][i] for i in range(len(A)))

def eye_int(n):
    return [[1 if i==j else 0 for j in range(n)] for i in range(n)]

def add_int(A,B):
    n=len(A); m=len(A[0])
    return [[A[i][j]+B[i][j] for j in range(m)] for i in range(n)]

def scal_int(A, s):
    return [[A[i][j]*s for j in range(len(A))] for i in range(len(A))]

def le_verrier_charpoly_int(A):
    """
    Return integer coefficients [1, c1, c2, ..., cn] for χ_A(λ)=λ^n + c1 λ^{n-1} + ... + cn.
    Works over Z for integer matrices (division by k is exact each step).
    """
    n = len(A)
    I = eye_int(n)
    B = [row[:] for row in A]     # B1 = A
    coeff = [0]*(n+1)
    coeff[0] = 1
    for k in range(1, n+1):
        tr = trace_int(B)
        if tr % k != 0:
            raise ValueError("non-integer leverrier step (unexpected)")
        ck = -tr // k
        coeff[k] = ck
        # B_{k+1} = A * (B_k + c_k I)
        BI = add_int(B, scal_int(I, ck))
        B = matmul_int(A, BI)
    return coeff  # length n+1

def eval_poly_int_desc(coeffs, x):
    """Evaluate p(x) where coeffs are descending power integers."""
    v = 0
    for c in coeffs:
        v = v*x + c
    return v

def synthetic_div_int(coeffs, r):
    """Divide integer poly by (x - r). Return quotient coeffs (ints)."""
    out = [coeffs[0]]
    carry = coeffs[0]
    for c in coeffs[1:]:
        carry = c + carry*r
        out.append(carry)
    rem = out.pop()
    if rem != 0:
        raise ValueError("not a root")
    return out

def divisors(n):
    n = abs(n)
    ds = []
    for d in range(1, n+1):
        if n % d == 0:
            ds.append(d)
    return ds + [-d for d in ds]

def poly_to_string_desc(coeffs):
    """e.g., [1,-11,29,35,-150] -> λ^4 - 11 λ^3 + 29 λ^2 + 35 λ - 150"""
    n = len(coeffs)-1
    parts = []
    for i,c in enumerate(coeffs):
        p = n - i
        if c == 0: continue
        sign = "+" if c > 0 else "-"
        a = abs(c)
        if p == 0:
            term = f"{a}"
        elif p == 1:
            term = ("" if a == 1 else f"{a} ") + "λ"
        else:
            term = ("" if a == 1 else f"{a} ") + f"λ^{p}"
        parts.append((sign, term))
    if not parts:
        return "0"
    # first sign: drop '+' if positive
    s0, t0 = parts[0]
    s = ("" if s0 == "+" else "-") + t0
    for sgn, term in parts[1:]:
        s += f" {sgn} {term}"
    return s

def mat_poly_eval(A, coeffs):
    """Compute coeffs[0]*A^n + ... + coeffs[n] I (descending, degree n)."""
    n = len(A)
    deg = len(coeffs)-1
    # Horner with matrices: (((I*c0 + A*c1) + A*c2) + ...)
    # Easier: build powers and accumulate
    P = eye(n)
    # We'll use Horner from highest degree:
    M = zeros(n, n)
    for c in coeffs:
        # M = M*A + c*I
        M = mat_mul(M, A)
        M = mat_add(M, mat_scalar(eye(n), (c,1)))
    return M

# --------------------------- Cases & harness ---------------------------

if __name__ == "__main__":
    cases = [
        "Matrix([[1, 3], [-2, 3]])*Matrix([[0, 3], [0, 7]])",
        "Matrix([[1, 3], [-2, 3]])**2",
        "Matrix([[1, 3], [-2, 3]])**-1",
        "Matrix([[1, 0, 1], [2, -1, 3], [4, 3, 2]]).det()",
        "Matrix([[3, -2,  4, -2], [5,  3, -3, -2], [5, -2,  2, -2], [5, -2, -3,  3]]).eigenvals()",
    ]

    for c in cases:
        print("="*72)
        print("Case:", c)

        # 1) 2x2 * 2x2
        if ")*Matrix(" in c and c.startswith("Matrix("):
            left = c[:c.find(")*Matrix(")+1]  # "Matrix([...])"
            right = c[c.find(")*Matrix(")+2:] # "Matrix([...])"
            A = parse_matrix_literal(left)
            B = parse_matrix_literal(right)
            C = mat_mul(A, B)
            print(f"{c} = {mat_str(C)}")

            # Explain-and-check
            print("\nExplain:")
            print("  Entry (i,j) is the dot product of row i of the left matrix,")
            print("  and column j of the right matrix. Determinant identity holds: det(AB)=det(A)det(B).")
            print("Check:")
            # entrywise check from definition
            ok = True
            for i in range(2):
                for j in range(2):
                    lhs = (0,1)
                    for k in range(2):
                        lhs = add_frac(lhs, mul_frac(A[i][k], B[k][j]))
                    ok = ok and eq_frac(lhs, C[i][j])
                    print(f"  C[{i},{j}] = Σ A[{i},k]*B[k,{j}] = {frac_str(C[i][j])}")
            detA = det2(A)
            detB = det2(B)
            detC = det2(C)
            print("  det(A) =", frac_str(detA), " det(B) =", frac_str(detB),
                  " det(AB) =", frac_str(detC),
                  " -> det(AB)=det(A)det(B)?", eq_frac(detC, mul_frac(detA, detB)))

        # 2) 2x2 ** 2
        elif c.endswith(")**2"):
            A = parse_matrix_literal(c[:c.find(")**2")+1])
            A2 = mat_pow(A, 2)
            print(f"{c} = {mat_str(A2)}")

            print("\nExplain:")
            print("  A**2 means A⋅A (standard matrix product).")
            print("Check:")
            # recompute directly and compare
            AA = mat_mul(A, A)
            print("  A·A equals printed A**2? ->", mat_eq(AA, A2))
            # Cayley–Hamilton for 2x2: A^2 - (tr A)A + (det A)I = 0
            tr = add_frac(A[0][0], A[1][1])
            detA = det2(A)
            left = mat_sub(mat_sub(A2, mat_scalar(A, tr)), mat_scalar(eye(2), mul_frac(detA, (-1,1))))
            # Wait: formula is A^2 - (tr A)A + (det A)I = 0, so A^2 - tr*A + det*I
            left = mat_add(mat_sub(A2, mat_scalar(A, tr)), mat_scalar(eye(2), detA))
            zero = mat_eq(left, zeros(2,2))
            print("  Cayley–Hamilton (2×2): A^2 - (tr A)A + (det A)I = 0 ? ->", zero)

        # 3) 2x2 ** -1
        elif c.endswith(")**-1"):
            A = parse_matrix_literal(c[:c.find(")**-1")+1])
            invA, detA, adjA = inv2(A)
            print(f"{c} = {mat_str(invA)}")

            print("\nExplain:")
            print("  For 2×2 [[a,b],[c,d]], A^{-1} = (1/(ad-bc)) [[d,-b],[-c,a]] (adjugate over determinant).")
            print("Check:")
            print("  det(A) =", frac_str(detA), " adj(A) =", mat_str(adjA))
            print("  A·A^{-1} = I ? ->", mat_eq(mat_mul(A, invA), eye(2)))
            print("  A^{-1}·A = I ? ->", mat_eq(mat_mul(invA, A), eye(2)))

        # 4) 3x3 det
        elif c.endswith("]).det()"):
            A = parse_matrix_literal(c[:c.find("]).det()")+1])
            pos, neg = det3_sarrus_terms(A)
            D = det3(A)
            print(f"{c} = {frac_str(D)}")

            print("\nExplain (Sarrus):")
            print("  det = (aei + bfg + cdh) - (ceg + afh + bdi)")
            print("  + terms:", " + ".join(frac_str(t) for t in pos))
            print("  - terms:", " + ".join(frac_str(t) for t in neg))
            # Cross-check by Laplace along first row
            a,b,c = A[0]; d,e,f = A[1]; g,h,i = A[2]
            C11 = sub_frac(mul_frac(e,i), mul_frac(f,h))
            C12 = make_frac(-to_frac(sub_frac(mul_frac(d,i), mul_frac(f,g)))[0], 1)
            C13 = sub_frac(mul_frac(d,h), mul_frac(e,g))
            D_lap = add_frac(add_frac(mul_frac(a, C11), mul_frac(b, C12)), mul_frac(c, C13))
            print("Check:")
            print("  Laplace along row 1 gives:", frac_str(D_lap), " -> matches?", eq_frac(D, D_lap))

        # 5) 4x4 eigenvals
        elif c.endswith("]).eigenvals()"):
            A = parse_matrix_literal(c[:c.find("]).eigenvals()")+1])
            Aint = mat_int(A)
            coeff = le_verrier_charpoly_int(Aint)  # [1, c1, c2, c3, c4]
            print("Explain: characteristic polynomial via Faddeev–LeVerrier")
            print("  χ_A(λ) = λ^4 + c1 λ^3 + c2 λ^2 + c3 λ + c4")
            print("  Coefficients:", coeff)
            print("  =>", poly_to_string_desc(coeff))

            # Factor by integer roots
            const = coeff[-1]
            cand = divisors(const)
            roots = []
            poly = coeff[:]
            for r in cand:
                if eval_poly_int_desc(poly, r) == 0:
                    roots.append(r)
            # remove duplicates properly via synthetic division
            fact = []
            tmp = poly[:]
            for r in roots:
                while True:
                    if eval_poly_int_desc(tmp, r) == 0:
                        tmp = synthetic_div_int(tmp, r)
                        fact.append(r)
                    else:
                        break
            # If quadratic remains, factor it
            if len(tmp) == 3:  # ax^2+bx+c
                a,b,cq = tmp
                # discriminant
                disc = b*b - 4*a*cq
                # we already expect integer roots here
                r1 = (-b + int(disc**0.5)) // (2*a)
                r2 = (-b - int(disc**0.5)) // (2*a)
                fact.extend([r1, r2])

            # multiplicities
            mult = {}
            for r in fact:
                mult[r] = mult.get(r, 0) + 1

            # Print in dict style
            items = ", ".join(f"{k}: {v}" for k, v in sorted(mult.items()))
            print(f"{c} = "+"{"+items+"}")

            # Proof harness
            print("\nCheck:")
            # (a) Cayley–Hamilton: χ_A(A) = 0
            # Convert coeffs to descending, evaluate on A (as fractions)
            # coeff already descending with degree 4
            Afrac = A  # already fracs
            CA = mat_poly_eval(Afrac, coeff)
            print("  Cayley–Hamilton χ_A(A) = 0 ? ->", mat_eq(CA, zeros(4,4)))
            # (b) Trace equals sum of eigenvalues
            tr = sum(Aint[i][i] for i in range(4))
            sumeigs = sum(k*v for k,v in mult.items())
            print("  trace(A) =", tr, " sum(eigs) =", sumeigs, " -> equal?", tr == sumeigs)
            # (c) det equals product of eigenvalues
            prod = 1
            for k,v in mult.items():
                for _ in range(v):
                    prod *= k
            # det(A) is constant term of χ_A (here degree even, so det(A)=c4)
            detA = coeff[-1]
            print("  det(A) =", detA, " ∏eigs =", prod, " -> equal?", detA == prod)

        else:
            raise ValueError("unrecognized case")


