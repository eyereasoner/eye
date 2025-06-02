// inference.js
// A tiny forward-chaining inference engine for the predicate type/2
// Run with:  node inference.js

// ---------------------------------------------------------------------------
// knowledge base (set of [subject, category] tuples)
// ---------------------------------------------------------------------------
const facts = new Set();

/**
 * Serialises a tuple so it can live in a Set.
 */
function key(subject, category) {
  return `${subject}::${category}`;
}

/**
 * Assert an explicit fact  type(S, C).
 */
function assertType(subject, category) {
  facts.add(key(subject, category));
}

/**
 * Apply the rule  type(X,'Man') ⇒ type(X,'Mortal')  until no new facts arise.
 */
function infer() {
  let changed = true;
  while (changed) {
    changed = false;

    for (const k of Array.from(facts)) {
      const [subject, category] = k.split("::");
      if (category === "Man") {
        const mortalKey = key(subject, "Mortal");
        if (!facts.has(mortalKey)) {
          facts.add(mortalKey);
          changed = true;
        }
      }
    }
  }
}

/**
 * Equivalent to the Prolog query  ?- type(_, _).
 * Returns an array of [Subject, Category] pairs, sorted for stable output.
 */
function queryType() {
  infer();
  return Array.from(facts)
    .map(k => k.split("::"))
    .sort(([a1, b1], [a2, b2]) =>
      a1 === a2 ? b1.localeCompare(b2) : a1.localeCompare(a2)
    );
}

// ---------------------------------------------------------------------------
// Seed the knowledge base and run the “?- type(_, _).” query
// ---------------------------------------------------------------------------
assertType("Socrates", "Man");

if (require.main === module) {
  for (const [subject, category] of queryType()) {
    console.log(`type('${subject}', '${category}')`);
  }
}
