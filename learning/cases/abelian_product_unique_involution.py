#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
EYE Learning — Self-contained, self-checking script (group theory)
Case: Product of all elements in a finite abelian group

Puzzle / Claim
--------------
Let G be a finite abelian group, written multiplicatively. Then
the product of all elements of G equals

    • e   if G has 0 or ≥2 elements of order 2;
    • t   if G has a unique element t of order 2 (i.e., t^2 = e, t ≠ e).

Equivalently, in additive notation the sum of all elements is 0 unless
the 2-torsion has size 2, in which case it is the unique nonzero 2-torsion.

Contract (EYE-learning style)
-----------------------------
- Answer: explicit formula above.
- Reason Why: pair every g with g^{-1}; only self-inverse elements remain.
  Those form the 2-torsion subgroup T (an elementary abelian 2-group).
  The product over T is the identity unless |T|=2, in which case it’s the
  unique non-identity element.
- Check (harness): verify on many abelian groups Z_n and their products
  Z_{n1} × ... × Z_{nk} (no external packages).
"""

from itertools import product
from typing import List, Tuple

# ----------------------------
# Finite abelian groups as products of cyclic Z_n (additive notation)
# ----------------------------

class AbelianGroup:
    """
    G = Z_{n1} × ... × Z_{nk} under component-wise addition mod n_i.
    We'll still say "product" but it's the group operation (addition).
    """
    def __init__(self, moduli: List[int], name: str = None):
        assert all(n >= 1 for n in moduli), "Moduli must be ≥ 1"
        self.n = tuple(moduli)
        self.k = len(self.n)
        self.size = 1
        for m in self.n:
            self.size *= m
        self.id = tuple(0 for _ in self.n)
        self.name = name or "×".join(f"Z{m}" for m in self.n)

    def op(self, a: Tuple[int, ...], b: Tuple[int, ...]) -> Tuple[int, ...]:
        return tuple((ai + bi) % ni for ai, bi, ni in zip(a, b, self.n))

    def inv(self, a: Tuple[int, ...]) -> Tuple[int, ...]:
        return tuple((-ai) % ni for ai, ni in zip(a, self.n))

    def iter_elements(self):
        ranges = [range(m) for m in self.n]
        for tup in product(*ranges):
            yield tup

    # --- helpers specific to the statement

    def is_order_two(self, a: Tuple[int, ...]) -> bool:
        # a ≠ 0 and 2a = 0
        if a == self.id:
            return False
        two_a = self.op(a, a)
        return two_a == self.id

    def all_order_two(self) -> List[Tuple[int, ...]]:
        return [a for a in self.iter_elements() if self.is_order_two(a)]

    def product_of_all(self) -> Tuple[int, ...]:
        # Fold the group operation over all elements (additive “sum”)
        acc = self.id
        for a in self.iter_elements():
            acc = self.op(acc, a)
        return acc

# ----------------------------
# Answer logic as a predicate
# ----------------------------

def expected_product(G: AbelianGroup) -> Tuple[int, ...]:
    order2 = G.all_order_two()
    if len(order2) == 1:
        # unique t with 2t=0
        return order2[0]
    else:
        # none or many → identity
        return G.id

# ----------------------------
# Reason (proof sketch)
# ----------------------------

def build_reason() -> str:
    lines = []
    lines.append("Pair-with-inverse argument in an abelian group G.")
    lines.append("• Partition G \\ T into pairs {g, g^{-1}} with g ≠ g^{-1}.")
    lines.append("  Each pair multiplies to e, so they contribute e to the total product.")
    lines.append("• What remains is the product over T = {x ∈ G : x = x^{-1}} = {x : 2x = 0},")
    lines.append("  the 2-torsion subgroup. T is elementary abelian (every element has order 1 or 2).")
    lines.append("• If |T| ≥ 4 (or |T|=1), elements of T can be paired to multiply to e; the total")
    lines.append("  product over T is e. If |T|=2, then T={e,t} and the product over T is t.")
    lines.append("Therefore the product over all of G is e unless there is a unique t of order 2,")
    lines.append("in which case the product is t.")
    lines.append("")
    lines.append("Concrete decomposition view.")
    lines.append("Write G ≅ Z_{n1}×…×Z_{nk}. Then the 2-torsion size is 2^r, where r is the number")
    lines.append("of even n_i (each contributes a coordinate 0 or n_i/2). Hence there is a unique")
    lines.append("non-identity 2-torsion element iff exactly one n_i is even; in that case the product")
    lines.append("of all elements equals that element (0,…,n_i/2,…,0).")
    return "\n".join(lines)

# ----------------------------
# Harness
# ----------------------------

def print_header(title: str):
    print("="*72)
    print(title)
    print("="*72)

def check_group(G: AbelianGroup) -> None:
    got = G.product_of_all()
    want = expected_product(G)
    assert got == want, f"{G.name}: product {got} ≠ expected {want}"

def small_groups_suite() -> list:
    res = []
    # A) Single cyclic Z_n, n=1..40
    for n in range(1, 41):
        G = AbelianGroup([n], name=f"Z_{n}")
        check_group(G)
    res.append("✓ Z_n for 1 ≤ n ≤ 40")

    # B) Products Z_m × Z_n with moderate sizes (avoid huge groups)
    for m in range(1, 21):
        for n in range(1, 21):
            if m * n > 4000:  # keep it snappy
                continue
            G = AbelianGroup([m, n], name=f"Z_{m}×Z_{n}")
            check_group(G)
    res.append("✓ Z_m × Z_n for m,n ≤ 20 with |G| ≤ 4000")

    # C) Triples Z_a × Z_b × Z_c, smallish
    triples = [(2,2,2), (2,4,3), (3,3,3), (4,5,6), (8,3,5), (10,6,9)]
    for a,b,c in triples:
        if a*b*c <= 6000:
            G = AbelianGroup([a,b,c], name=f"Z_{a}×Z_{b}×Z_{c}")
            check_group(G)
    res.append("✓ Selected triples Z_a × Z_b × Z_c (|G| ≤ 6000)")

    # D) A few randomized mixes
    import random
    random.seed(7)
    for _ in range(40):
        k = random.choice([1,2,3])
        mods = []
        size = 1
        while len(mods) < k:
            m = random.randint(2, 16)
            if size * m > 6000:
                break
            mods.append(m)
            size *= m
        if not mods:
            continue
        G = AbelianGroup(mods, name="×".join(f"Z_{m}" for m in mods))
        check_group(G)
    res.append("✓ Random abelian products (bounded size)")
    return res

def main():
    print_header("ANSWER")
    print("Finite abelian G: the product of all elements equals e unless G has a unique")
    print("element t of order 2, in which case the product equals t.")

    print_header("REASON WHY")
    print(build_reason())

    print_header("CHECK (HARNESS)")
    try:
        for line in small_groups_suite():
            print(line)
        print("All checks PASSED ✅")
    except AssertionError as e:
        print(f"Check FAILED ❌: {e}")
        raise

if __name__ == "__main__":
    main()

