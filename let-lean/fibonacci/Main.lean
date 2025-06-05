import Std

/-
  An efficient, linear-time implementation of Fibonacci using tail recursion.
-/
def fib (n : Nat) : Nat :=
  go n 0 1
where
  go : Nat → Nat → Nat → Nat
    | 0,     a, _   => a
    | k + 1, a, b   => go k b (a + b)

/-
  The `main` entry point computes and prints `fib n`
  for the list of inputs #[0, 1, 91, 283, 3674].
-/
def main : IO Unit := do
  let inputs := #[0, 1, 91, 283, 3674]
  for n in inputs do
    let val := fib n
    -- Print in the format: "fib <n> = <value>"
    IO.println s!"fib {n} = {val}"
