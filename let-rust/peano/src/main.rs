// -------------------------------------------------------------------------
// % Peano arithmetic
// % See https://en.wikipedia.org/wiki/Peano_axioms
//
// The Prolog source represents natural numbers with 0 and nested s/1 functors:
//     0           → 0
//     s(0)        → 1
//     s(s(0))     → 2
// etc.
//
// In Rust we mirror that with an ordinary `usize` *and* helper functions
// that convert back and forth to a Peano-style text so the output looks
// just like the Prolog terms in the original query.
// -------------------------------------------------------------------------

/// ---------- helper: convert usize → Peano term as a String -------------
fn to_peano(n: usize) -> String {
    if n == 0 {
        "0".into()
    } else {
        format!("s({})", to_peano(n - 1))
    }
}

/// ---------- add --------------------------------------------------------
/// % add
/// add(A, 0, A).
/// add(A, s(B), s(C)) :- add(A, B, C).
fn add(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        // Prolog: add(A,B,C) ⇒ add(A,B-1,C1), C = s(C1)
        add(a, b - 1) + 1
    }
}

/// ---------- multiply ---------------------------------------------------
/// % multiply
/// multiply(_, 0, 0).
/// multiply(A, s(B), C) :-
///     multiply(A, B, D),
///     add(A, D, C).
fn multiply(a: usize, b: usize) -> usize {
    if b == 0 {
        0
    } else {
        let d = multiply(a, b - 1);
        add(a, d)
    }
}

/// ---------- factorial (wrapper + worker) ------------------------------
/// % factorial
/// factorial(A, B) :- fac(A, s(0), B).
///
/// fac(0, A, A).
/// fac(s(A), B, C) :-
///     multiply(B, s(A), D),
///     fac(A, D, C).
fn factorial(n: usize) -> usize {
    /// worker that mirrors `fac/3`
    fn fac(a: usize, acc: usize) -> usize {
        if a == 0 {
            acc
        } else {
            let d = multiply(acc, a);
            fac(a - 1, d)
        }
    }
    fac(n, 1) // s(0) = 1
}

// -------------------------------------------------------------------------
// % query
// ?-
//     multiply(s(0), s(s(0)), A),
//     add(A, s(s(s(0))), B),
//     factorial(B, _).
//
// Rust: perform the same sequence and print the intermediate terms.
// -------------------------------------------------------------------------
fn main() {
    // multiply(s(0), s(s(0)), A)        → 1 * 2 = 2
    let a = multiply(1, 2);

    // add(A, s(s(s(0))), B)             → 2 + 3 = 5
    let b = add(a, 3);

    // factorial(B, _)                   → 5! = 120
    let fact_b = factorial(b);

    println!("A  = {}   // {}", to_peano(a), a);
    println!("B  = {}   // {}", to_peano(b), b);
    println!("B! = {}   // {}", to_peano(fact_b), fact_b);
}
