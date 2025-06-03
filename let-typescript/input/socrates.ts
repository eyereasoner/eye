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

// ---------- Type aliases for clarity ------------------------------------
type Entity = string;
type Class  = string;
type Fact   = [Entity, Class];

// ---------- extensional database: asserted facts ------------------------
const FACTS: Fact[] = [
  ['Socrates', 'Man']
];

// ---------- forward-chaining rule ---------------------------------------
// TypeScript version of
//     type(X,'Mortal') :- type(X,'Man').
function inferAll(facts: Fact[]): Fact[] {
  const seen = new Map<string, Fact>();      // deduplication helper

  // copy base facts
  for (const fact of facts) {
    seen.set(fact.join('|'), fact);
  }

  // derive new ones
  for (const [x, cls] of facts) {
    if (cls === 'Man') {
      const newFact: Fact = [x, 'Mortal'];
      seen.set(newFact.join('|'), newFact);
    }
  }

  return Array.from(seen.values());
}

// ---------- “query”  type(_, _)  ----------------------------------------
function main(): void {
  const allFacts = inferAll(FACTS);
  allFacts.forEach(([x, y]) => console.log(`type(${x}, ${y})`));
}

main();
