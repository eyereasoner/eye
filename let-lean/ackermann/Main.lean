/-
  Fast Ackermann demo for the specific test set

  Tricks:
  * closed-form formulas for m ≤ 4
  * use bit-shift for 2ⁿ
-/

-- 2ⁿ using a single GMP `mpz_mul_2exp` call
@[inline] def pow2 (e : Nat) : Nat :=
  (1 <<< e)

-- Closed-form + fallback for m ≥ 5 (rarely reached here)
partial def ackFast : Nat → Nat → Nat
| 0, n => n + 1
| 1, n => n + 2
| 2, n => 2 * n + 3
| 3, n => pow2 (n + 3) - 3
| 4, 0 => 13                                   -- 2↑3 − 3
| 4, 1 => pow2 16      - 3                     -- 2↑4 − 3
| 4, 2 => pow2 65536   - 3                     -- 2↑5 − 3
| (m+1), 0   => ackFast m 1
| (m+1), n+1 => ackFast m (ackFast (m+1) n)

def pretty (m n : Nat) : String :=
  toString (ackFast m n)

def main : IO Unit := do
  let tests :=
    [ (0,6), (1,2), (1,7), (2,2), (2,9)
    , (3,4), (3,14), (4,0), (4,1), (4,2), (5,0) ]
  for (m,n) in tests do
    IO.println s!"ackermann({m}, {n}) = {pretty m n}"
