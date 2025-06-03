// ---------------------------------------------------------
// Peano arithmetic in TypeScript
// ---------------------------------------------------------
export type Peano = number;

export function add(a: Peano, b: Peano): Peano {
  if (b === 0) return a;
  const c = add(a, b - 1);
  return c + 1;
}

export function multiply(a: Peano, b: Peano): Peano {
  if (b === 0) return 0;
  const d = multiply(a, b - 1);
  return add(a, d);
}

export function factorial(n: Peano): Peano {
  return fac(n, 1);
}

function fac(n: Peano, acc: Peano): Peano {
  if (n === 0) return acc;
  const d = multiply(acc, n);
  return fac(n - 1, d);
}

// ---- Demo ------------------------------------------------
const A = multiply(1, 2);
const B = add(A, 3);
const F = factorial(B);

console.log(`A = ${A}`); // 2
console.log(`B = ${B}`); // 5
console.log(`B! = ${F}`); // 120
