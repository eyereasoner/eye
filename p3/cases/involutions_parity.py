#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P3 — Self-contained, self-checking script (group theory)
Case: Parity of involutions

Puzzle / Claim
--------------
Let G be a finite group. Show that the number of solutions of x^2 = e in G
(i.e., the number of self-inverse elements, including e) has the same parity
as |G|. Equivalently:

    # { x in G : x^2 = e }  ≡  |G|   (mod 2).

In particular, a finite group has odd order  ⇔  the only solution to x^2 = e
is x = e.

Contract (P3 style)
-----------------------------
- Answer: statement above (parity congruence and corollary for odd order).
- Reason Why: pair elements with their inverses; only self-inverse elements
  remain unpaired → parity conclusion.
- Check (harness): verify on many concrete groups (cyclic Z_n, dihedral D_n,
  symmetric S_n for n ≤ 6, quaternion Q8, and various direct products).

No external packages. Pure Python 3.
"""

from itertools import product, permutations

# ----------------------------
# Tiny group framework
# ----------------------------

class FiniteGroup:
    def __init__(self, elements, op, id_elem, inv=None, name="G"):
        self.E = list(elements)
        self.op = op
        self.id = id_elem
        self.name = name
        if inv is None:
            self.inv = self._compute_inverse_map()
        else:
            self.inv = inv

    def _compute_inverse_map(self):
        inv = {}
        for a in self.E:
            for b in self.E:
                if self.op(a, b) == self.id and self.op(b, a) == self.id:
                    inv[a] = b
                    break
        return inv

    def is_self_inverse(self, a):
        return self.op(a, a) == self.id

    def count_self_inverse(self):
        return sum(1 for a in self.E if self.is_self_inverse(a))

    def order(self):
        return len(self.E)

# ----------------------------
# Concrete groups
# ----------------------------

def cyclic_group(n):
    # (Z_n, +)
    E = list(range(n))
    op = lambda a,b: (a + b) % n
    inv = {a: (-a) % n for a in E}
    return FiniteGroup(E, op, 0, inv=inv, name=f"Z_{n}")

def direct_product(G, H, name=None):
    E = list(product(G.E, H.E))
    def op(a,b):
        return (G.op(a[0], b[0]), H.op(a[1], b[1]))
    id_elem = (G.id, H.id)
    inv = {(g,h):(G.inv[g], H.inv[h]) for g,h in E}
    return FiniteGroup(E, op, id_elem, inv=inv, name=(name or f"{G.name}×{H.name}"))

def dihedral_group(n):
    """
    D_n = < r,s | r^n = e, s^2 = e, s r s = r^{-1} >, order 2n.
    Represent elements as (k, f) with k in 0..n-1, f in {0,1} meaning r^k s^f.
    Multiplication: (a,b)*(c,d) = ( a + (-1)^b c  mod n,  b⊕d ).
    """
    E = [(k, f) for k in range(n) for f in (0,1)]
    def op(x,y):
        a,b = x; c,d = y
        k = (a + (c if b==0 else (-c)%n)) % n
        return (k, b ^ d)
    id_elem = (0,0)
    # inverse: (k,0)^{-1} = (-k,0), (k,1)^{-1} = (k,1)
    inv = {}
    for k,f in E:
        if f == 0:
            inv[(k,f)] = ((-k) % n, 0)
        else:
            inv[(k,f)] = (k,1)
    return FiniteGroup(E, op, id_elem, inv=inv, name=f"D_{n}")

def quaternion_group_Q8():
    """
    Q8 = {±1, ±i, ±j, ±k}, with i^2=j^2=k^2=ijk=-1.
    Encode: 0:1, 1:-1, 2:i, 3:-i, 4:j, 5:-j, 6:k, 7:-k.
    """
    names = ["1","-1","i","-i","j","-j","k","-k"]
    # multiplication table (by index)
    # We'll build using rules: i*j=k, j*k=i, k*i=j and anti-commuting signs.
    def mul_sym(a, b):
        # map index -> (unit, sign), units in {1,i,j,k} as 0,2,4,6; sign ∈ {+1,-1}
        units = {0:(0,1), 1:(0,-1), 2:(2,1), 3:(2,-1), 4:(4,1), 5:(4,-1), 6:(6,1), 7:(6,-1)}
        ua, sa = units[a]; ub, sb = units[b]
        # unit multiplication table for 1,i,j,k (indices 0,2,4,6)
        # 1 is 0; i=2; j=4; k=6
        table = {
            (0,0):(0, 1), (0,2):(2, 1), (0,4):(4, 1), (0,6):(6, 1),
            (2,0):(2, 1), (2,2):(0,-1), (2,4):(6, 1), (2,6):(4,-1),
            (4,0):(4, 1), (4,2):(6,-1), (4,4):(0,-1), (4,6):(2, 1),
            (6,0):(6, 1), (6,2):(4, 1), (6,4):(2,-1), (6,6):(0,-1),
        }
        u, s = table[(ua, ub)]
        s *= sa * sb
        # map back to index
        back = {(0,1):0,(0,-1):1,(2,1):2,(2,-1):3,(4,1):4,(4,-1):5,(6,1):6,(6,-1):7}
        return back[(u,s)]
    E = list(range(8))
    op = mul_sym
    id_elem = 0
    inv = {}
    for a in E:
        for b in E:
            if op(a,b)==id_elem and op(b,a)==id_elem:
                inv[a]=b; break
    G = FiniteGroup(E, op, id_elem, inv=inv, name="Q8")
    G.pretty = lambda a: names[a]
    return G

def symmetric_group(n):
    """
    S_n as permutations of {0,...,n-1}, composed left-to-right: (p∘q)(i)=p[q[i]].
    """
    E = list(permutations(range(n)))
    def op(p,q):  # compose p after q
        return tuple(p[i] for i in q)
    id_elem = tuple(range(n))
    # inverse quickly
    def inv_perm(p):
        n = len(p)
        q = [None]*n
        for i,pi in enumerate(p):
            q[pi] = i
        return tuple(q)
    inv = {p: inv_perm(p) for p in E}
    return FiniteGroup(E, op, id_elem, inv=inv, name=f"S_{n}")

# ----------------------------
# Reason (proof sketch)
# ----------------------------

def build_reason() -> str:
    lines = []
    lines.append("Pairing-by-inverse argument.")
    lines.append("For any finite group G, partition G into {x, x^{-1}} pairs.")
    lines.append("• If x ≠ x^{-1}, then {x, x^{-1}} contributes 2 elements — an even number.")
    lines.append("• The only unpaired elements are those with x = x^{-1}, i.e., x^2 = e.")
    lines.append("Therefore the parity of |G| equals the parity of the number of solutions")
    lines.append("to x^2 = e. In symbols,  # {x : x^2=e} ≡ |G| (mod 2).")
    lines.append("")
    lines.append("Corollary (odd order).")
    lines.append("If |G| is odd, the congruence forces exactly one solution, namely x=e.")
    lines.append("Conversely, if x=e is the unique solution, then the number of solutions")
    lines.append("is 1, which is odd, hence |G| is odd.")
    return "\n".join(lines)

# ----------------------------
# Harness
# ----------------------------

def print_header(title: str):
    print("="*72)
    print(title)
    print("="*72)

def parity_ok(G: FiniteGroup) -> bool:
    return (G.count_self_inverse() - G.order()) % 2 == 0

def only_e_for_odd_order(G: FiniteGroup) -> bool:
    if G.order() % 2 == 1:
        return G.count_self_inverse() == 1
    return True  # not required

def run_checks():
    results = []

    # A) Cyclic groups Z_n for n=1..35
    for n in range(1, 36):
        G = cyclic_group(n)
        assert parity_ok(G), f"Parity failed for {G.name}"
        assert only_e_for_odd_order(G), f"Odd-order corollary failed for {G.name}"
    results.append("✓ Cyclic groups Z_n up to n=35.")

    # B) Dihedral groups D_n for n=3..20
    for n in range(3, 21):
        G = dihedral_group(n)
        assert parity_ok(G), f"Parity failed for {G.name}"
    results.append("✓ Dihedral groups D_n, 3 ≤ n ≤ 20.")

    # C) Symmetric groups S_n for n=2..6
    for n in range(2, 7):
        G = symmetric_group(n)
        assert parity_ok(G), f"Parity failed for {G.name}"
    results.append("✓ Symmetric groups S_n, 2 ≤ n ≤ 6.")

    # D) Quaternion group Q8
    Q8 = quaternion_group_Q8()
    assert parity_ok(Q8), "Parity failed for Q8"
    results.append("✓ Quaternion group Q8.")

    # E) Some direct products (mixing odd/even orders)
    pairs = [
        (3, 4), (5, 6), (7, 8), (9,10), (5,5), (8,9)
    ]
    for a,b in pairs:
        G = direct_product(cyclic_group(a), cyclic_group(b))
        assert parity_ok(G), f"Parity failed for {G.name}"
        assert only_e_for_odd_order(G), f"Odd-order corollary failed for {G.name}"
    results.append("✓ Direct products Z_a × Z_b for several (a,b).")

    return results

# ----------------------------
# Presentation
# ----------------------------

def main():
    print_header("ANSWER")
    print("In any finite group G, the number of solutions to x^2 = e has the same")
    print("parity as |G|. In particular, |G| is odd  ⇔  the only solution is x = e.")

    print_header("REASON WHY")
    print(build_reason())

    print_header("CHECK (HARNESS)")
    try:
        for line in run_checks():
            print(line)
        print("All checks PASSED ✅")
    except AssertionError as e:
        print(f"Check FAILED ❌: {e}")
        raise

if __name__ == "__main__":
    main()

