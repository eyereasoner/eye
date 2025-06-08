/-
  Fast Fibonacci with fast-doubling.
  Works for every non-negative `n` that fits into a Lean `Nat`
  (i.e. until you run out of memory).
-/

@[inline]                    -- keep the tight inner loop inlined
partial def fibPair : Nat → Nat × Nat
| 0 => (0, 1)                -- (F₀ , F₁)
| n =>
  let (a, b) := fibPair (n / 2)      -- Fₖ , Fₖ₊₁  where k = ⌊n/2⌋
  let c      := a * (2 * b - a)      -- F₂ₖ
  let d      := a * a + b * b        -- F₂ₖ₊₁
  if n % 2 == 0 then (c, d) else (d, c + d)

@[inline] def fib (n : Nat) : Nat :=
  (fibPair n).fst

/-- Pretty-printer: for “large” results also show the digit count. -/
def pretty (n : Nat) : IO String := do
  let f := fib n
  pure <|
    if n > 200 then
      s!"{f}\n  (digits: {Nat.toDigits 10 f |>.length})"
    else
      toString f

def main : IO Unit := do
  let tests : List Nat := [0, 1, 91, 283, 3674]
  for n in tests do
    IO.println s!"F({n}) =\n  {← pretty n}\n"
