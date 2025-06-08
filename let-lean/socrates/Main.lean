-- Define entities
inductive Entity where
  | socrates : Entity
  deriving Repr, DecidableEq

-- Define kinds
inductive Kind where
  | man : Kind
  | mortal : Kind
  deriving Repr, DecidableEq

open Entity Kind

-- Define the type predicate
inductive type : Entity → Kind → Prop where
  | socratesIsMan : type socrates man
  | manIsMortal : ∀ (e : Entity), type e man → type e mortal

open type

-- Build all facts derivable from the rules
def inferredFacts : List (Entity × Kind) :=
  let e := socrates
  let p₁ := type.socratesIsMan
  let p₂ := type.manIsMortal e p₁
  [(e, man), (e, mortal)]

-- Main function
def main : IO Unit := do
  IO.println "Inferred facts:"
  for (e, k) in inferredFacts do
    IO.println s!"type({repr e}, {repr k})"
