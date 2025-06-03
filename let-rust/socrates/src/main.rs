// ------------------------------------------------------------
//  Socrates is a mortal – Prolog → Rust
// ------------------------------------------------------------
use std::collections::HashSet;

/// A proven `type/2` fact.  In Prolog this is the pair `type(X,Y)`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct TypeFact(&'static str, &'static str);

/// Hard-coded **extensional database**: the asserted facts.
static FACTS: &[TypeFact] = &[TypeFact("Socrates", "Man")];

/// Derive all facts that follow from the rule
///
///     type(X, 'Mortal') :- type(X, 'Man').
fn infer_all() -> HashSet<TypeFact> {
    let mut derived: HashSet<TypeFact> = FACTS.iter().cloned().collect();

    // One forward-chaining pass is enough here because the rule
    // only derives a *new* fact from an existing base fact.
    for TypeFact(x, class) in FACTS {
        if *class == "Man" {
            derived.insert(TypeFact(x, "Mortal"));
        }
    }
    derived
}

fn main() {
    // Prolog query:  ?- type(_, _).
    let all_facts = infer_all();

    for fact in &all_facts {
        println!("type({:?}, {:?})", fact.0, fact.1);
    }
}
