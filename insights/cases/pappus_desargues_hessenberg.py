#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
================================================================================
Hessenberg (1905):  Pappus  ⇒  Desargues   — Explanation + Randomized Check
================================================================================

WHAT THIS PROGRAM DOES
----------------------
This script prints three sections:

  1) Answer
     A precise statement of Hessenberg's theorem:
       In a projective plane, if Pappus' theorem holds for all configurations,
       then Desargues' theorem holds for all configurations.

  2) Reason
     (a) A modern algebraic explanation in 4 lines:
           Pappus ⇒ commutative coordinatization (a field) ⇒
           projective plane over a division ring ⇒ Desargues.
         (Pappus forces commutativity; planes over any division ring are
          Desarguesian.)
     (b) A compact *synthetic* reconstruction (outline) of Hessenberg's
         original 1905 argument that avoids coordinates, using only
         incidence/projectivity and the behavior of central collineations.
         This mirrors the spirit of Hessenberg’s paper: derive Desargues
         directly from Pappus without building a full algebra first.

  3) Check (harness)
     A randomized, computational “sanity check” over small finite projective
     planes PG(2, p) for small primes p. In such planes (coordinatized by
     finite fields), BOTH Pappus and Desargues hold. The harness repeatedly
     builds random Pappus and Desargues configurations and tests the required
     collinearities using homogeneous coordinates and exact integer arithmetic
     modulo p. We expect 100% passes (modulo occasional sampling degeneracies,
     which are filtered out).

WHY THIS IS RELEVANT TO HESSENBERG
----------------------------------
Hessenberg proved (1905) that Pappus ⇒ Desargues *synthetically*. Later,
coordinatization theory (Hilbert; Veblen–Young; Artin; Hall) showed that
Pappus characterizes *commutative* coordinatization (fields), while Desargues
holds already over any division ring (possibly noncommutative). Hence the
modern algebraic chain:
      Pappus ⇒ field structure ⇒ Desargues.

This program:
  • Encodes Pappus/Desargues configurations exactly in PG(2, p).
  • Verifies the collinearities with no floating-point issues.
  • Prints an outline of Hessenberg’s synthetic route (no coordinates used).

MATHEMATICAL CONVENTIONS USED IN THE CODE
-----------------------------------------
We work in homogeneous coordinates over GF(p):
  • Points are 3-vectors P = (x,y,z), not all zero, up to nonzero scalar.
  • Lines are also 3-vectors ℓ = (a,b,c), up to nonzero scalar.
  • Incidence is “dot(ℓ, P) = 0 (mod p)”.
  • The join of two points is ℓ = P × Q (cross product).
  • The meet of two lines is P = ℓ × m.
  • Collinearity of P, Q, R means (P × Q) · R = 0.

PAPPUS CONFIGURATION TESTED
---------------------------
Two distinct lines carry triples A,B,C and A',B',C'. Define:
  X = (AB') ∩ (A'B),  Y = (AC') ∩ (A'C),  Z = (BC') ∩ (B'C).
Pappus says X, Y, Z are collinear.

DESARGUES CONFIGURATION TESTED
------------------------------
Two triangles ABC and A'B'C' are in perspective from a point O: the lines
AA', BB', CC' concur at O (the “center”). Define:
  P = (AB) ∩ (A'B'),  Q = (AC) ∩ (A'C'),  R = (BC) ∩ (B'C').
Desargues says P, Q, R are collinear (the “axis”).

LIMITATIONS
-----------
The harness is a *demonstration*, not a proof. It checks many random
instances over small fields. Hessenberg’s actual proof is synthetic; we
include an outline of that proof in text form below (printed by the program).

================================================================================
"""

from random import randrange, seed
seed(0)  # deterministic randomness for reproducible runs

# ------------------------------------------------------------------------------
# Basic finite projective geometry utilities over GF(p)
# ------------------------------------------------------------------------------

def inv_mod(a, p):
    """
    Multiplicative inverse in GF(p), for prime p and a ≠ 0.
    Uses Fermat's little theorem: a^(p-2) ≡ a^{-1} (mod p).
    """
    a %= p
    if a == 0:
        raise ZeroDivisionError("No inverse for 0 in a field.")
    return pow(a, p - 2, p)

def norm_point(P, p):
    """
    Normalize a homogeneous point (or line) to a canonical representative:
    divide by the first nonzero coordinate so equality is testable by tuples.
    """
    x, y, z = P
    x %= p; y %= p; z %= p
    if (x, y, z) == (0, 0, 0):
        return (0, 0, 0)
    for v in (x, y, z):
        if v % p != 0:
            inv = inv_mod(v, p)
            return ((x * inv) % p, (y * inv) % p, (z * inv) % p)
    # mathematically unreachable
    return (0, 0, 0)

def projectively_equal(P, Q, p):
    """Check equality up to nonzero scalar via normalized reps."""
    return norm_point(P, p) == norm_point(Q, p)

def cross(u, v, p):
    """
    Projective join/meet via cross product mod p:
      • If u,v are points, u×v is the line through them.
      • If u,v are lines, u×v is their intersection point.
    """
    (x1, y1, z1) = u
    (x2, y2, z2) = v
    return ((y1 * z2 - z1 * y2) % p,
            (z1 * x2 - x1 * z2) % p,
            (x1 * y2 - y1 * x2) % p)

def dot(l, P, p):
    """Incidence test: l·P == 0 (mod p) means P lies on line l."""
    a, b, c = l
    x, y, z = P
    return (a * x + b * y + c * z) % p

def line_through(P, Q, p):
    """Return the normalized line through two points P,Q."""
    L = cross(P, Q, p)
    return norm_point(L, p)

def intersect(L1, L2, p):
    """Return the normalized intersection point of lines L1,L2."""
    P = cross(L1, L2, p)
    return norm_point(P, p)

def collinear(P, Q, R, p):
    """True iff P, Q, R are collinear: (P×Q)·R == 0."""
    return dot(cross(P, Q, p), R, p) == 0

def random_affine_point(p):
    """Random point with z=1 (i.e., not on the line at infinity)."""
    return (randrange(p), randrange(p), 1)

def random_point(p):
    """Random homogeneous point (not all coords 0)."""
    while True:
        x, y, z = randrange(p), randrange(p), randrange(p)
        if (x, y, z) != (0, 0, 0):
            return (x, y, z)

def random_distinct_points(n, p):
    """
    Return n distinct (up to projective equality) affine points.
    Avoids coincidences by normalizing and comparing representatives.
    """
    pts = []
    while len(pts) < n:
        P = random_affine_point(p)
        if all(not projectively_equal(P, Q, p) for Q in pts):
            pts.append(P)
    return pts

def point_on_span(U, V, p):
    """
    Sample a random nonzero linear combination αU+βV (homogeneous).
    If U,V are on a line L, this returns another point on L with high prob.
    """
    while True:
        a, b = randrange(p), randrange(p)
        if (a, b) != (0, 0):
            P = ((a * U[0] + b * V[0]) % p,
                 (a * U[1] + b * V[1]) % p,
                 (a * U[2] + b * V[2]) % p)
            if P != (0, 0, 0):
                return P

def random_line_and_span_points(p):
    """
    Construct a random line by choosing two random points U,V and returning:
      (L, U, V)  where  L is the line through U and V.
    """
    U, V = random_distinct_points(2, p)
    L = line_through(U, V, p)
    return L, U, V

def noncollinear_triple(p):
    """Return A,B,C (affine) that are not collinear."""
    while True:
        A, B, C = random_distinct_points(3, p)
        if not collinear(A, B, C, p):
            return A, B, C

# ------------------------------------------------------------------------------
# Theorem checkers (random configuration sampling)
# ------------------------------------------------------------------------------

def sample_pappus_trial(p, max_attempts=250):
    """
    Build a random Pappus configuration on two distinct lines and test the
    collinearity of the three “opposite-side” intersection points.

    Returns: (result, data)
      result ∈ {True, False, None}
        • True/False: test executed and passed/failed
        • None: degenerate sample; no verdict
      data: a tuple with the points used (for potential debugging/inspection).
    """
    for _ in range(max_attempts):
        # First line with three distinct points A,B,C
        L, U, V = random_line_and_span_points(p)
        A, B = U, V
        C = None
        for __ in range(30):
            C = point_on_span(U, V, p)
            if not projectively_equal(C, A, p) and not projectively_equal(C, B, p):
                break

        # Second, distinct line with three distinct points A',B',C'
        while True:
            M, U2, V2 = random_line_and_span_points(p)
            if M != L:
                break

        A2, B2 = U2, V2
        C2 = None
        for __ in range(30):
            C2 = point_on_span(U2, V2, p)
            if not projectively_equal(C2, A2, p) and not projectively_equal(C2, B2, p):
                break

        # Lines for opposite sides
        L1 = line_through(A,  B2, p); L2 = line_through(A2, B,  p)
        L3 = line_through(A,  C2, p); L4 = line_through(A2, C,  p)
        L5 = line_through(B,  C2, p); L6 = line_through(B2, C,  p)

        # Filter degeneracies (coincident opposite sides)
        if L1 == L2 or L3 == L4 or L5 == L6:
            continue

        # Intersections X,Y,Z
        X = intersect(L1, L2, p)
        Y = intersect(L3, L4, p)
        Z = intersect(L5, L6, p)
        if X == (0, 0, 0) or Y == (0, 0, 0) or Z == (0, 0, 0):
            continue

        return collinear(X, Y, Z, p), (A, B, C, A2, B2, C2, X, Y, Z)
    return None, None

def sample_desargues_trial(p, max_attempts=250):
    """
    Build a random Desargues configuration from a random concurrency point O,
    with triangles ABC and A'B'C' chosen so that AA', BB', CC' concur at O.
    Test that the intersections P,Q,R of corresponding sides are collinear.

    Returns: (result, data) with same semantics as sample_pappus_trial.
    """
    for _ in range(max_attempts):
        O = random_affine_point(p)
        A, B, C = noncollinear_triple(p)

        def on_OP_not_O_or_P(O, P):
            # Randomly pick a point on line OP but distinct from O and P.
            for __ in range(60):
                a = randrange(1, p)  # both nonzero increases chance of distinctness
                b = randrange(1, p)
                Q = ((a * O[0] + b * P[0]) % p,
                     (a * O[1] + b * P[1]) % p,
                     (a * O[2] + b * P[2]) % p)
                if not projectively_equal(Q, O, p) and not projectively_equal(Q, P, p):
                    # Collinearity with O,P is automatic from the linear form; keep for clarity
                    if collinear(Q, O, P, p):
                        return Q
            return None

        Ap = on_OP_not_O_or_P(O, A)
        Bp = on_OP_not_O_or_P(O, B)
        Cp = on_OP_not_O_or_P(O, C)
        if None in (Ap, Bp, Cp):
            continue

        # Intersections of corresponding sides
        L_AB,   L_ApBp = line_through(A,  B,  p), line_through(Ap, Bp, p)
        L_AC,   L_ApCp = line_through(A,  C,  p), line_through(Ap, Cp, p)
        L_BC,   L_BpCp = line_through(B,  C,  p), line_through(Bp, Cp, p)

        # Filter degeneracies (coincident lines leading to undefined P,Q,R)
        if L_AB == L_ApBp or L_AC == L_ApCp or L_BC == L_BpCp:
            continue

        Ppt = intersect(L_AB, L_ApBp, p)
        Qpt = intersect(L_AC, L_ApCp, p)
        Rpt = intersect(L_BC, L_BpCp, p)
        if Ppt == (0, 0, 0) or Qpt == (0, 0, 0) or Rpt == (0, 0, 0):
            continue

        return collinear(Ppt, Qpt, Rpt, p), (A, B, C, Ap, Bp, Cp, O, Ppt, Qpt, Rpt)
    return None, None

# ------------------------------------------------------------------------------
# Harness
# ------------------------------------------------------------------------------

def run_harness(primes=(3, 5, 7, 11), trials=120):
    """
    For each prime p, run 'trials' independent random Pappus and Desargues
    configurations in PG(2, p). Count how many pass (True) out of nondegenerate
    attempts. In a field plane we expect near 100% passes.
    """
    lines = []
    header = "p | Pappus (ok/total) | Desargues (ok/total)"
    lines.append(header)
    lines.append("-" * len(header))

    for p in primes:
        pap_ok = des_ok = 0
        pap_total = des_total = 0

        for _ in range(trials):
            r1, _ = sample_pappus_trial(p)
            if r1 is not None:
                pap_total += 1
                pap_ok += int(r1)

            r2, _ = sample_desargues_trial(p)
            if r2 is not None:
                des_total += 1
                des_ok += int(r2)

        lines.append(f"{p} | {pap_ok}/{pap_total}           | {des_ok}/{des_total}")

    lines.append("")
    lines.append("Note: Over GF(p) both theorems are true, so we expect 100% success.")
    return "\n".join(lines)

# ------------------------------------------------------------------------------
# Printed text (Answer + Reason, including Hessenberg reconstruction outline)
# ------------------------------------------------------------------------------

ANSWER = (
    "Hessenberg’s theorem (1905): In any projective plane, if Pappus’ theorem "
    "holds for all appropriate configurations (i.e., the plane is Pappian), "
    "then Desargues’ theorem also holds for all configurations (i.e., the plane is Desarguesian)."
)

REASON_ALGEBRAIC = (
    "Modern algebraic reason (4 lines):\n"
    "  (1) A projective plane with Pappus can be coordinatized by a commutative division ring (a field).\n"
    "  (2) Projective planes over any division ring (commutative or not) satisfy Desargues.\n"
    "  (3) Every field is a division ring.\n"
    "  (4) Therefore Pappus ⇒ Desargues."
)

REASON_HESSENBERG_OUTLINE = (
    "Hessenberg’s synthetic reconstruction (outline, no coordinates):\n"
    "  • Setup: Work in a projective plane where Pappus holds. Study central collineations\n"
    "    (elations/homologies: collineations fixing an 'axis' line pointwise and a 'center' point).\n"
    "  • Translations and shears: Using Pappus, show that the composition of two elations with\n"
    "    parallel axes is again an elation and that such elations commute. This yields an abelian\n"
    "    'translation group' acting freely on an affine patch (remove a line at infinity).\n"
    "  • Parallelogram law: The commuting translations induce a commutative, associative 'addition'\n"
    "    on each affine line (constructible purely by incidence). Pappus is precisely the geometric\n"
    "    statement ensuring commutativity of this addition.\n"
    "  • Homotheties and products: By combining a fixed triangle and central perspectivities,\n"
    "    define 'dilations'; their composition rules are controlled by Pappus-configurations.\n"
    "    This gives a commutative 'scalar' multiplication compatible with the additive structure.\n"
    "  • Projectivities from a point: With this structure in hand, compare any two triangles that\n"
    "    are perspective from a point O. One can map one triangle to the other by a composition of\n"
    "    elations/dilations whose axis is a fixed line. The fixed line becomes the 'axis of\n"
    "    perspectivity', and the three intersections of corresponding sides lie on it—this is\n"
    "    exactly Desargues’ theorem. Thus Desargues follows from the Pappus-driven behavior of\n"
    "    central collineations, without coordinatization.\n"
    "  • Moral: Pappus forces the plane’s collineation group to behave like the affine/projective\n"
    "    groups over a field; Desargues is a consequent property of such geometry."
)

def main():
    print("Answer:")
    print(ANSWER)
    print()

    print("Reason:")
    print(REASON_ALGEBRAIC)
    print()
    print(REASON_HESSENBERG_OUTLINE)
    print()

    print("Check (harness):")
    print(run_harness(primes=(3, 5, 7, 11), trials=120))

if __name__ == "__main__":
    main()

