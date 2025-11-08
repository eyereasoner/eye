#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
===============================================================================
Group Theory in Plain Python
===============================================================================

This script demonstrates *finite group theory* using only standard Python.
It aims to be didactic: **lots** of comments connect the code to the math.

What we build
-------------
1) A lightweight "group interface" `FiniteGroup` with:
   - elements()   → finite list of elements of G
   - op(a,b)      → the group operation (written multiplicatively; Z/nZ uses +)
   - e()          → identity element
   - inv(a)       → inverse of a
   Plus: pow(a, n) for fast repeated products (binary exponentiation).

2) Concrete groups:
   - Z/nZ (integers modulo n under addition). We'll use n = 12.
     • Abelian; order 12; every element x has finite order dividing 12.

   - S_n (symmetric group on n symbols). We'll use n = 4.
     • Non-abelian; order 4! = 24; elements are permutations.
     • Operation is **composition** of permutations (right-to-left).

   - GL(2, Z_p): invertible 2×2 matrices over the finite field Z_p (p prime).
     We'll use p = 5:
     • Non-abelian; order |GL(2, 5)| = (5² − 1)(5² − 5) = 24 × 20 = 480.
     • An element is quadruple (a,b,c,d) representing matrix [[a,b],[c,d]] mod p.
     • Inverse uses adjugate/determinant with modular inverse.

3) Homomorphisms:
   - φ : Z/12Z → Z/4Z given by reduction mod 4.
     • We check φ(a+b) = φ(a) + φ(b)  (a homomorphism of additive groups).
     • We verify First Isomorphism Theorem numerically:
         |Z/12Z| / |ker φ| == |im φ|.

   - sgn : S₄ → C₂ (Z/2Z), the permutation parity map.
     • sgn(σ∘τ) = sgn(σ) + sgn(τ) mod 2.

4) Computations & theorems we *illustrate by code*:
   - Associativity, identity, inverses, closure (group axioms).
     • Exhaustive for Z/12Z and S₄; sampled for GL(2,5) (to keep it snappy).
   - Non-commutativity witnesses for S₄ and GL(2,5).
   - Element orders and quick checks motivated by **Cauchy's theorem**:
       For each prime dividing |G|, we find an element whose order is that prime.
   - **Lagrange's theorem** sanity: For each group, every element's order divides |G|.

Why plain Python?
-----------------
In dependently typed settings, shape or algebraic laws can live in types and be
statically checked. Here we enforce structure at runtime with simple functions
and `assert`-like checks. The upside is simplicity and approachability.

Output format
-------------
The script prints:
- "Answer"        : high-level results and example computations,
- "Reason why"    : short explanation connecting the results to the theory,
- "Check (harness)": verification of axioms and homomorphism laws, plus sanity tests.

===============================================================================
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import List, Tuple, Dict, Callable, TypeVar, Generic, Optional
import itertools
import random

# A type variable for elements of a (finite) group
T = TypeVar("T")

# Type aliases to make signatures more readable in comments
Perm = Tuple[int, ...]              # A permutation stored as a tuple p with p[i] = image of i
Mat2 = Tuple[int, int, int, int]    # A 2×2 matrix [[a,b],[c,d]] represented as (a,b,c,d)


# =============================================================================
# Generic finite-group shell
# =============================================================================

class FiniteGroup(Generic[T]):
    """
    Minimal interface for a finite group.

    Methods any concrete group must implement:
      - elements() : List[T]     The finite set underlying the group G.
      - op(a,b)    : T           The group operation (associative).
      - e()        : T           The identity element.
      - inv(a)     : T           The inverse of a (so op(a, inv(a)) == e()).

    We also provide:
      - pow(a, n)  : T           "Repeated product" a^n with binary exponentiation;
                                  when the group is written additively (like Z/nZ),
                                  this still works (it uses repeated op), it’s just
                                  computing n·a in that case.
    """
    def elements(self) -> List[T]:
        raise NotImplementedError

    def op(self, a: T, b: T) -> T:
        """Group operation. In Z/nZ this is addition mod n; elsewhere multiplication-like."""
        raise NotImplementedError

    def e(self) -> T:
        """Identity element (e.g., 0 in Z/nZ, identity permutation, identity matrix)."""
        raise NotImplementedError

    def inv(self, a: T) -> T:
        """Inverse of a (−a in Z/nZ, permutation inverse, matrix inverse)."""
        raise NotImplementedError

    def name(self) -> str:
        """Human-friendly name for printing."""
        return self.__class__.__name__

    def pow(self, a: T, n: int) -> T:
        """
        Fast exponentiation (a^n) using square-and-multiply.

        Math connection:
        - In multiplicative notation: a^n = a · a · ... · a (n times).
        - In additive notation (Z/nZ), `pow(a,n)` computes n·a.

        Precondition: n >= 0 (we skip negative exponents for simplicity).
        """
        if n < 0:
            raise ValueError("Exponent must be non-negative.")
        r = self.e()  # a^0 = e
        b = a
        k = n
        while k:
            if k & 1:            # if current bit is 1, multiply in the base
                r = self.op(r, b)
            b = self.op(b, b)    # square the base
            k >>= 1              # shift to next bit
        return r


# =============================================================================
# Concrete groups
# =============================================================================

@dataclass
class Zmod(FiniteGroup[int]):
    """
    Z/nZ = integers modulo n under addition.
    - Underlying set: {0, 1, ..., n-1}
    - Operation: (a + b) mod n
    - Identity: 0
    - Inverse:  (-a) mod n
    """
    n: int

    def elements(self) -> List[int]:
        return list(range(self.n))

    def op(self, a: int, b: int) -> int:
        return (a + b) % self.n

    def e(self) -> int:
        return 0

    def inv(self, a: int) -> int:
        return (-a) % self.n

    def name(self) -> str:
        return f"Z/{self.n}Z"


@dataclass
class SymmetricGroup(FiniteGroup[Perm]):
    """
    S_n, the symmetric group on n symbols.
    - Element representation: a permutation p as a tuple with p[i] = image of i.
    - Operation: composition (apply the right permutation first!)
                 (p ∘ q)(i) = p(q(i)) → implemented as tuple(p[q[i]] for i in ...)
    - Identity: identity permutation id(i) = i.
    - Inverse: computed by inverting the mapping positions.
    """
    n: int
    _elems: Optional[List[Perm]] = None

    def elements(self) -> List[Perm]:
        # There are n! permutations; for n=4, that's 24 — small enough to enumerate.
        if self._elems is None:
            self._elems = list(itertools.permutations(range(self.n)))
        return self._elems

    def op(self, p: Perm, q: Perm) -> Perm:
        # Composition: apply q, then p. That is, i ↦ p[q[i]].
        # NOTE: composition order matters; S_n is non-abelian for n ≥ 3.
        return tuple(p[q[i]] for i in range(self.n))

    def e(self) -> Perm:
        # Identity permutation (0,1,...,n-1)
        return tuple(range(self.n))

    def inv(self, p: Perm) -> Perm:
        # The inverse permutation p^{-1} satisfies p^{-1}[p[i]] = i.
        inv = [0] * self.n
        for i, img in enumerate(p):
            inv[img] = i
        return tuple(inv)

    def name(self) -> str:
        return f"S_{self.n}"


@dataclass
class GL2Zp(FiniteGroup[Mat2]):
    """
    GL(2, Z_p): group of invertible 2×2 matrices over the finite field Z_p.

    Element representation:
        M = (A, B, C, D) encodes the matrix [[A, B], [C, D]] with entries mod p.

    Group operation:
        Matrix multiplication modulo p.

    Identity:
        I = (1, 0, 0, 1).

    Inverse:
        For M = [[A, B],[C, D]], inverse is det(M)^{-1} * adj(M),
        where det = A*D - B*C (mod p), and adj(M) = [[ D,-B],[-C, A]] (classical adjugate).
        We compute det^{-1} using extended Euclid (since Z_p is a field).
    """
    p: int
    _elems: Optional[List[Mat2]] = None

    # --- small helpers for modular arithmetic ---
    def _mod(self, x: int) -> int:
        return x % self.p

    def _det(self, M: Mat2) -> int:
        A, B, C, D = M
        return self._mod(A * D - B * C)

    def _inv_mod(self, a: int) -> int:
        """
        Compute a^{-1} mod p via Extended Euclidean Algorithm.
        Precondition: gcd(a, p) = 1 (i.e., a ≠ 0 in the field Z_p).
        """
        a %= self.p
        if a == 0:
            raise ZeroDivisionError("no inverse mod p")
        t, newt = 0, 1
        r, newr = self.p, a
        # Classic extended GCD loop.
        while newr:
            q = r // newr
            t, newt = newt, t - q * newt
            r, newr = newr, r - q * newr
        if r != 1:
            # Should not happen for nonzero a in a field.
            raise ZeroDivisionError("not invertible mod p")
        return t % self.p

    def elements(self) -> List[Mat2]:
        """
        Enumerate all invertible matrices in M_2(Z_p) by filtering on nonzero determinant.
        There are |GL(2, p)| = (p^2 - 1)(p^2 - p) such matrices.
        For p=5 this is 480 — small enough to precompute.
        """
        if self._elems is None:
            p = self.p
            elems: List[Mat2] = []
            for A in range(p):
                for B in range(p):
                    for C in range(p):
                        for D in range(p):
                            if (A * D - B * C) % p != 0:  # det ≠ 0 ⇒ invertible
                                elems.append((A, B, C, D))
            self._elems = elems
        return self._elems

    def op(self, M: Mat2, N: Mat2) -> Mat2:
        # (2×2) · (2×2) modulo p
        p = self.p
        A, B, C, D = M
        E, F, G, H = N
        return ((A * E + B * G) % p, (A * F + B * H) % p,
                (C * E + D * G) % p, (C * F + D * H) % p)

    def e(self) -> Mat2:
        # Identity matrix
        return (1, 0, 0, 1)

    def inv(self, M: Mat2) -> Mat2:
        # M^{-1} = det(M)^{-1} * adj(M) over Z_p
        A, B, C, D = M
        det = self._det(M)
        inv_det = self._inv_mod(det)
        return (( D * inv_det) % self.p, (-B * inv_det) % self.p,
                (-C * inv_det) % self.p, ( A * inv_det) % self.p)

    def name(self) -> str:
        return f"GL(2, Z_{self.p})"


# =============================================================================
# Utilities & checks
# =============================================================================

def check_group_axioms(
    G: FiniteGroup[T], *, exhaustive_assoc: bool = False, assoc_samples: int = 5000
) -> Dict[str, bool]:
    """
    Verify the group axioms for G, returning a dict of booleans.

    Axioms:
      (Assoc)   (a*b)*c == a*(b*c)               for all a,b,c
      (Ident)   e*a == a == a*e                  for all a
      (Inv)     a*a^{-1} == e == a^{-1}*a        for all a
      (Closure) a*b is in elements(G)            for all a,b

    For large groups, checking associativity over all triples is |G|^3 operations.
    We therefore allow a *sampled* check (default) and enable *exhaustive* for small groups.
    """
    elems = G.elements()
    e = G.e()

    # Associativity
    ok_assoc = True
    if exhaustive_assoc:
        # Exhaustive cubic check — okay for |G| <= ~30
        for a in elems:
            for b in elems:
                for c in elems:
                    if G.op(G.op(a, b), c) != G.op(a, G.op(b, c)):
                        ok_assoc = False
                        break
                if not ok_assoc:
                    break
            if not ok_assoc:
                break
    else:
        # Random sampling for speed: find a counterexample if it exists with high probability
        for _ in range(assoc_samples):
            a, b, c = random.choice(elems), random.choice(elems), random.choice(elems)
            if G.op(G.op(a, b), c) != G.op(a, G.op(b, c)):
                ok_assoc = False
                break

    # Identity: must be neutral on both sides
    ok_ident = all(G.op(e, a) == a and G.op(a, e) == a for a in elems)

    # Inverses: both left and right inverse equal identity
    ok_inv = all(G.op(a, G.inv(a)) == e and G.op(G.inv(a), a) == e for a in elems)

    # Closure: operation stays in the set (we spot-check to avoid |G|^2 cost on huge groups)
    limit = min(1000, len(elems) ** 2)
    ok_closure = True
    for _ in range(limit):
        a, b = random.choice(elems), random.choice(elems)
        if G.op(a, b) not in elems:
            ok_closure = False
            break

    return {"associativity": ok_assoc, "identity": ok_ident, "inverses": ok_inv, "closure": ok_closure}


def permutation_parity(p: Perm) -> int:
    """
    Return 0 for even permutations, 1 for odd permutations.

    Theory: parity equals (# inversions) mod 2.
    An inversion is a pair (i, j) with i<j but p[i] > p[j].
    """
    inv_count = 0
    n = len(p)
    for i in range(n):
        for j in range(i + 1, n):
            if p[i] > p[j]:
                inv_count += 1
    return inv_count & 1


def check_homomorphism(
    G: FiniteGroup[T], H: FiniteGroup, phi: Callable[[T], T], *, exhaustive: bool = False, samples: int = 5000
) -> bool:
    """
    Check the homomorphism law: φ(a*b) == φ(a) · φ(b).

    - If `exhaustive` or |G| is small, check all pairs (a,b).
    - Otherwise randomly sample `samples` pairs.
    """
    elemsG = G.elements()
    if exhaustive or len(elemsG) <= 200:
        for a in elemsG:
            for b in elemsG:
                if phi(G.op(a, b)) != H.op(phi(a), phi(b)):
                    return False
        return True
    else:
        for _ in range(samples):
            a, b = random.choice(elemsG), random.choice(elemsG)
            if phi(G.op(a, b)) != H.op(phi(a), phi(b)):
                return False
        return True


def element_order(G: FiniteGroup[T], a: T) -> int:
    """
    The (finite) order of a in G: smallest k > 0 such that a^k = e.
    Brute-force by repeated multiplication (works fine for our small groups).
    """
    e = G.e()
    acc = a
    k = 1
    # Multiply until we cycle back to identity. A safety cap avoids infinite loops.
    while acc != e:
        acc = G.op(acc, a)
        k += 1
        if k > 10000:
            return -1  # should never hit for finite groups used here
    return k


def find_noncommuting_pair(G: FiniteGroup[T]):
    """
    If G is non-abelian, return an explicit (a, b) with a*b != b*a.
    If abelian, return None.
    """
    elems = G.elements()
    for a in elems:
        for b in elems:
            if G.op(a, b) != G.op(b, a):
                return a, b
    return None


def all_orders_divide_group_order(G: FiniteGroup[T]) -> bool:
    """
    Lagrange sanity: for each element g in G, order(g) divides |G|.
    For finite groups this always holds (Lagrange's theorem).
    """
    n = len(G.elements())
    for g in G.elements():
        k = element_order(G, g)
        if n % k != 0:
            return False
    return True


# =============================================================================
# Demo: Answer / Reason / Harness
# =============================================================================

def main():
    random.seed(0xC0FFEE)  # deterministic sampling for reproducibility

    # --- Construct our three groups ---------------------------------------------------------
    Z12  = Zmod(12)           # additive group of integers modulo 12
    S4   = SymmetricGroup(4)  # symmetric group on 4 points
    GL25 = GL2Zp(5)           # invertible 2×2 matrices over Z_5 (order 480)

    # --- A few concrete elements and sample operations --------------------------------------
    # Z/12Z examples (remember: op is addition mod 12, inv is additive inverse)
    aZ, bZ = 7, 10
    abZ    = Z12.op(aZ, bZ)       # 7 + 10 ≡ 5 (mod 12)
    invZ   = Z12.inv(aZ)          # -7 ≡ 5 (mod 12) since 7 + 5 ≡ 0

    # S4 examples: permutations as tuples (images of 0,1,2,3).
    # p = (1 0)(2)(3)      i.e., swaps 0 and 1.
    # q = (0 2)(1)(3)      i.e., swaps 0 and 2.
    p: Perm = (1, 0, 2, 3)
    q: Perm = (2, 1, 0, 3)
    pq      = S4.op(p, q)         # composition: apply q, then p
    invp    = S4.inv(p)
    noncomm_pair_S4 = find_noncommuting_pair(S4)

    # GL(2,5) examples: matrices are (a,b,c,d) for [[a,b],[c,d]] mod 5.
    M: Mat2 = (2, 1, 0, 1)   # det = 2 (invertible)
    N: Mat2 = (1, 1, 1, 2)   # det = 1
    MN      = GL25.op(M, N)  # matrix multiply mod 5
    invM    = GL25.inv(M)    # matrix inverse mod 5
    noncomm_pair_GL = find_noncommuting_pair(GL25)

    # --- Homomorphisms ----------------------------------------------------------------------
    # φ: Z/12Z → Z/4Z, φ([x]) = [x mod 4]
    Z4 = Zmod(4)
    φ = lambda x: x % 4
    φ_is_hom = check_homomorphism(Z12, Z4, φ, exhaustive=True)
    ker_φ = [x for x in Z12.elements() if φ(x) == Z4.e()]  # kernel: elements mapping to 0 mod 4
    im_φ  = sorted({φ(x) for x in Z12.elements()})         # image: all residues mod 4
    # First Iso Theorem: Z12 / ker(φ) ≅ im(φ). We check the size relation numerically.
    fit_ok = (len(Z12.elements()) // len(ker_φ)) == len(im_φ)

    # sgn: S4 → C2 (Z/2Z), parity of a permutation (even→0, odd→1)
    C2 = Zmod(2)
    sgn = lambda perm: permutation_parity(perm)
    sgn_is_hom = check_homomorphism(S4, C2, sgn, exhaustive=True)
    ker_sgn = [σ for σ in S4.elements() if sgn(σ) == 0]    # kernel is A4 (even permutations)
    index_A4 = len(S4.elements()) // len(ker_sgn)          # [S4 : A4] = 2

    # --- Orders & prime-order witnesses (Cauchy’s theorem motivation) -----------------------
    # For primes dividing |G|, there exists an element of that prime order.
    orders_Z12 = {x: element_order(Z12, x) for x in Z12.elements()}
    elem_order2_S4 = next((g for g in S4.elements()  if element_order(S4, g)  == 2), None)
    elem_order3_S4 = next((g for g in S4.elements()  if element_order(S4, g)  == 3), None)
    elem_order2_GL = next((g for g in GL25.elements() if element_order(GL25, g) == 2), None)
    elem_order3_GL = next((g for g in GL25.elements() if element_order(GL25, g) == 3), None)
    elem_order5_GL = next((g for g in GL25.elements() if element_order(GL25, g) == 5), None)

    # --- Lagrange sanity: order of each element divides group order -------------------------
    lagrange_Z12  = all_orders_divide_group_order(Z12)
    lagrange_S4   = all_orders_divide_group_order(S4)
    lagrange_GL25 = all_orders_divide_group_order(GL25)

    # =============================================================================
    #                               Answer
    # =============================================================================
    print("Answer")
    print("------")
    print(f"Groups: {Z12.name()} (|G|={len(Z12.elements())}), "
          f"{S4.name()} (|G|={len(S4.elements())}), "
          f"{GL25.name()} (|G|={len(GL25.elements())})")

    print("\nSample operations:")
    print(f"Z/12Z: {aZ} + {bZ} ≡ {abZ} (mod 12); inverse({aZ}) = {invZ}")
    print(f"S4: compose p={p} after q={q} → p∘q={pq}; inv(p)={invp}")
    print(f"GL(2,5): M={M}, N={N}, M·N={MN}; M⁻¹={invM}")
    if noncomm_pair_S4:
        a, b = noncomm_pair_S4
        print(f"Non-abelian witness for S4: a*b ≠ b*a for a={a}, b={b}")
    if noncomm_pair_GL:
        a, b = noncomm_pair_GL
        print(f"Non-abelian witness for GL(2,5): a*b ≠ b*a for a={a}, b={b}")

    print("\nHomomorphisms:")
    print(f"φ: Z/12Z → Z/4Z (mod 4) is a homomorphism? {φ_is_hom}")
    print(f"  ker φ = {ker_φ} (size {len(ker_φ)}),  im φ = {im_φ} (size {len(im_φ)})")
    print(f"  First Iso Theorem size check: |Z12|/|ker φ| == |im φ| → {fit_ok}")
    print(f"sgn: S4 → C2 is a homomorphism? {sgn_is_hom}")
    print(f"  |ker sgn| = {len(ker_sgn)} (A4),  index [S4:A4] = {index_A4} (expected 2)")

    print("\nElement orders (Cauchy checks):")
    print(f"Z/12Z orders: {orders_Z12}")
    print(f"S4 has elements of order 2? {elem_order2_S4 is not None}; order 3? {elem_order3_S4 is not None}")
    print(f"GL(2,5) has elements of order 2? {elem_order2_GL is not None}; 3? {elem_order3_GL is not None}; 5? {elem_order5_GL is not None}")

    print("\nLagrange sanity (order(g) divides |G| for all g):")
    print(f"Z/12Z: {lagrange_Z12},  S4: {lagrange_S4},  GL(2,5): {lagrange_GL25}")

    # =============================================================================
    #                               Reason why
    # =============================================================================
    print("\nReason why")
    print("----------")
    print("We model groups directly: a finite set, an operation, identity, and inverses.")
    print("Each concrete group implements these and we verify the axioms in code.")
    print("Homomorphisms preserve structure (φ(a*b)=φ(a)·φ(b)); we check this for:")
    print("  • φ: Z/12Z→Z/4Z (reduction mod 4), and  • sgn: S4→C2 (parity).")
    print("The First Isomorphism Theorem claims Z12/ker(φ) ≅ im(φ); we validate the size relation.")
    print("Cauchy’s theorem predicts elements with prime orders dividing |G|; we find examples.")
    print("Lagrange’s theorem predicts order(g) | |G|; we confirm this holds for all g in each group.")

    # =============================================================================
    #                               Check (harness)
    # =============================================================================
    print("\nCheck (harness)")
    print("----------------")
    print("Axiom checks (associativity exhaustive for small groups; sampled for GL(2,5)):")
    res_Z12 = check_group_axioms(Z12, exhaustive_assoc=True)
    print(f"  {Z12.name()}: {res_Z12}")
    res_S4 = check_group_axioms(S4, exhaustive_assoc=True)
    print(f"  {S4.name()}: {res_S4}")
    # For GL(2,5), associativity is sampled with many random triples.
    res_GL25 = check_group_axioms(GL25, exhaustive_assoc=False, assoc_samples=8000)
    print(f"  {GL25.name()}: {res_GL25}  (associativity via 8000 random triples)")

    # Commutativity (abelian?) checks
    def is_abelian(G: FiniteGroup) -> bool:
        elems = G.elements()
        for a in elems:
            for b in elems:
                if G.op(a, b) != G.op(b, a):
                    return False
        return True

    print(f"\nAbelian?")
    print(f"  {Z12.name()}: {is_abelian(Z12)}")
    print(f"  {S4.name()}:  {is_abelian(S4)}")
    print(f"  {GL25.name()}: {is_abelian(GL25)}")

    # Homomorphism re-checks (exhaustive)
    ok_phi = check_homomorphism(Z12, Z4, φ, exhaustive=True)
    ok_sgn = check_homomorphism(S4, C2, sgn, exhaustive=True)
    print(f"\nHomomorphism laws hold?  φ: {ok_phi},  sgn: {ok_sgn}")

    # Extra: sanity on inverses (a * a^{-1} = e) in GL(2,5) by random spot-checks
    trials = 10
    ok_inv_samples = True
    for _ in range(trials):
        g = random.choice(GL25.elements())
        if GL25.op(g, GL25.inv(g)) != GL25.e() or GL25.op(GL25.inv(g), g) != GL25.e():
            ok_inv_samples = False
            break
    print(f"GL(2,5) random inverse sanity ({trials} samples): {ok_inv_samples}")

if __name__ == "__main__":
    main()

