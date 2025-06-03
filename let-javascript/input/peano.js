// ------------------------------------------------------------
// Peano arithmetic in Node.js – direct translation from Prolog
// ------------------------------------------------------------

// add/3  —  add(A,B,C)  ⇔  C = A + B
function add(a, b) {
  if (b === 0)            // add(A, 0, A).
    return a;

  // add(A, s(B), s(C)) :- add(A, B, C).
  const c = add(a, b - 1);
  return c + 1;           // s(C)
}

// multiply/3  —  multiply(A,B,C)  ⇔  C = A × B
function multiply(a, b) {
  if (b === 0)            // multiply(_, 0, 0).
    return 0;

  // multiply(A, s(B), C) :- multiply(A,B,D), add(A,D,C).
  const d = multiply(a, b - 1);
  return add(a, d);
}

// factorial/2  —  factorial(N,F)  ⇔  F = N!
function factorial(n) {
  return fac(n, 1);       // fac/3 helper mirroring the Prolog version
}

function fac(n, acc) {
  if (n === 0)            // fac(0, A, A).
    return acc;

  // fac(s(A), B, C) :- multiply(B, s(A), D), fac(A, D, C).
  const d = multiply(acc, n); // s(A)  ≙  n
  return fac(n - 1, d);
}

// ------------------------------------------------------------
// Demo: exact counterpart of the original Prolog query
// ------------------------------------------------------------
// ?- multiply(s(0), s(s(0)), A),
//    add(A, s(s(s(0))), B),
//    factorial(B, _).

const A = multiply(1, 2);  // s(0) × s(s(0))  = 1 × 2  = 2
const B = add(A, 3);       // 2 + s(s(s(0))) = 2 + 3 = 5
const F = factorial(B);    // 5! = 120

console.log('A =', A);     // → 2
console.log('B =', B);     // → 5
console.log('B! =', F);    // → 120
