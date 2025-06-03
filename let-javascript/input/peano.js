// -------------------------------------------------------------------------
// % Peano arithmetic
// % See https://en.wikipedia.org/wiki/Peano_axioms
//
// Numbers are represented as 0 and nested s/1 functors:
//     0         → 0
//     s(0)      → 1
//     s(s(0))   → 2
// etc.
//
// In JavaScript we perform the calculations with plain integers for speed
// and provide helper functions that render / parse Peano terms so the
// printed output matches the Prolog style.
// -------------------------------------------------------------------------

// ---------- helper: int  → Peano string ---------------------------------
function toPeano(n) {
  return n === 0 ? "0" : `s(${toPeano(n - 1)})`;
}

// ---------- add ---------------------------------------------------------
// % add
// add(A, 0, A).
// add(A, s(B), s(C)) :-
//     add(A, B, C).
function add(a, b) {
  return b === 0 ? a : 1 + add(a, b - 1);
}

// ---------- multiply ----------------------------------------------------
// % multiply
// multiply(_, 0, 0).
// multiply(A, s(B), C) :-
//     multiply(A, B, D),
//     add(A, D, C).
function multiply(a, b) {
  return b === 0 ? 0 : add(a, multiply(a, b - 1));
}

// ---------- factorial (wrapper + worker) -------------------------------
// % factorial
// factorial(A, B) :-
//     fac(A, s(0), B).
//
// fac(0, A, A).
// fac(s(A), B, C) :-
//     multiply(B, s(A), D),
//     fac(A, D, C).
function factorial(n) {
  function fac(a, acc) {
    return a === 0 ? acc : fac(a - 1, multiply(acc, a));
  }
  return fac(n, 1); // s(0) = 1
}

// -------------------------------------------------------------------------
// % query
// ?-
//     multiply(s(0), s(s(0)), A),
//     add(A, s(s(s(0))), B),
//     factorial(B, _).
//
// JavaScript: perform the same steps and print Peano + decimal values.
// -------------------------------------------------------------------------
(function runQuery() {
  // multiply(s(0), s(s(0)), A)   → 1 * 2 = 2
  const A = multiply(1, 2);

  // add(A, s(s(s(0))), B)        → 2 + 3 = 5
  const B = add(A, 3);

  // factorial(B, _)              → 5! = 120
  const factB = factorial(B);

  console.log(`A  = ${toPeano(A)}   // ${A}`);
  console.log(`B  = ${toPeano(B)}   // ${B}`);
  console.log(`B! = ${toPeano(factB)}   // ${factB}`);
})();
