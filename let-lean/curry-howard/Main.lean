/-
swapData is an ordinary function on tuples.
swapProof is **the same λ-term**, but its type says
           P ∧ Q → Q ∧ P.

The key takeaway is: exactly one λ-expression acts as
both executable code and constructive proof.
Lean merely checks it against two different types
living in different universes (Type vs Prop).
-/

def swapData {α β : Type} : α × β → β × α :=
  fun ⟨a, b⟩ => (b, a)

def swapProof {P Q : Prop} : P ∧ Q → Q ∧ P :=
  fun ⟨hP, hQ⟩ => And.intro hQ hP

/-- Demo: print a data example and show that swapProof runs. -/
def main : IO Unit := do
  let input : String × Nat := ("hello", 3)
  IO.println "— data side —"
  IO.println s!"  input   = {input}"
  IO.println s!"  swapped = {swapData input}"

  IO.println "\n— proof side —"
  -- trivial proof of True ∧ True …
  let trivialProof : True ∧ True := And.intro trivial trivial
  -- … transformed by the very same λ-term:
  let _ : True ∧ True := swapProof trivialProof
  IO.println "  swapProof succeeded on (True ∧ True) → (True ∧ True)"
  IO.println "  (proof objects are erased at runtime)"
