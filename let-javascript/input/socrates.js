// -------------------------------------------------------------------------
// % Socrates is a mortal
//
// type('Socrates', 'Man').
//
// type(X, 'Mortal') :-
//     type(X, 'Man').
//
// % query
// ?- type(_, _).
// -------------------------------------------------------------------------

// ----- extensional facts (the Prolog database) --------------------------
const FACTS = [
  ['Socrates', 'Man']
];

// ----- forward-chaining rule -------------------------------------------
// JS version of  type(X,'Mortal') :- type(X,'Man').
function inferAll(facts) {
  const derived = new Set(facts.map(f => JSON.stringify(f))); // avoid duplicates

  for (const [x, cls] of facts) {
    if (cls === 'Man') {
      derived.add(JSON.stringify([x, 'Mortal']));
    }
  }
  return [...derived].map(JSON.parse);   // back to plain arrays
}

// ----- “query”  type(_, _)  --------------------------------------------
function main() {
  const allFacts = inferAll(FACTS);
  for (const [x, y] of allFacts) {
    console.log(`type(${x}, ${y})`);
  }
}

main();
