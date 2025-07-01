#!/usr/bin/env python3
"""
four_numbers_backward.py
Find {x1,x2,x3,x4} such that

    Σ xi      =  26
    Σ xi²     =  994
    Σ xi³     = 15086
    Π xi      = −9100

and show a step-by-step proof.
"""

from itertools import product
from math import prod

# 0.  Given power sums
S1   = 26
S2   = 994
S3   = 15086
P    = -9100                           # σ₄ (product)

print("\n=== Newton-identity step ===")
# 1.  σ₂ from Σ xi² = S1² − 2σ₂
σ2 = (S1**2 - S2) // 2
print(f"σ₂ = (S1² − S2)/2 = ({S1**2} − {S2})/2 = {σ2}")

# 2.  σ₃ from Σ xi³ = S1³ − 3S1σ₂ + 3σ₃
σ3 = (S3 - (S1**3 - 3*S1*σ2)) // 3
print(f"σ₃ = (S3 − (S1³ − 3S1σ₂))/3 = {σ3}")

# 3.  Characteristic polynomial
#     x⁴ − S1 x³ + σ₂ x² − σ₃ x + P = 0
coeffs = [1, -S1, σ2, -σ3, P]
print("\nCharacteristic polynomial:")
print(f"x⁴ {coeffs[1]:+} x³ {coeffs[2]:+} x² {coeffs[3]:+} x {coeffs[4]:+}")

# 4.  Trial integer roots: divisors of |P|
def divisors(n):
    n = abs(n)
    out=set()
    for d in range(1,int(n**0.5)+1):
        if n%d==0:
            out.add(d); out.add(-d); out.add(n//d); out.add(-(n//d))
    return out

candidate_roots = sorted(divisors(P))
print(f"\nTrial integer roots (± divisors of {P}): {candidate_roots[:10]} …")

# 5.  Factor by synthetic division
roots=[]
poly=coeffs[:]                     # mutable copy

def eval_poly(poly,x):
    """Evaluate polynomial given by coeff list at x (Horner)."""
    val=0
    for c in poly:
        val=val*x+c
    return val

for r in candidate_roots:
    while eval_poly(poly,r)==0 and len(poly)>1:
        roots.append(r)
        # synthetic division
        new=[poly[0]]
        for c in poly[1:]:
            new.append(new[-1]*r + c)
        poly=new[:-1]              # last coef is zero
        if len(roots)==4:
            break
    if len(roots)==4:
        break

print("\nDiscovered roots:", roots)

# 6.  Verify every constraint
print("\n=== Verification ===")
print("Sum        =", sum(roots))
print("Squares    =", sum(x*x for x in roots))
print("Cubes      =", sum(x**3 for x in roots))
print("Product    =", prod(roots))

