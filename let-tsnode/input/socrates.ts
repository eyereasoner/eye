#!/usr/bin/env -S ts-node
/**
 * A tiny forward-chaining inference engine for the predicate `type/2`
 * Original idea (Prolog): Jos De Roo
 * JS → TS adaptation: this file
 */

/* ──────────────────────────────────────────────────────────
   Domain model & knowledge base
   ────────────────────────────────────────────────────────── */

type Subject  = string;
type Category = string;

/** A Set key serialising a fact `type(S,C)` */
type Key = `${string}::${string}`;

/** The knowledge base: all asserted *or inferred* facts. */
const facts: Set<Key> = new Set();

/** Serialise a tuple so it can live in the `Set`. */
function key(subject: Subject, category: Category): Key {
  return `${subject}::${category}`;
}

/** Assert an explicit fact `type(S, C)`. */
export function assertType(subject: Subject, category: Category): void {
  facts.add(key(subject, category));
}

/**
 * Forward-chain the single rule
 *     type(X,'Man') ⇒ type(X,'Mortal')
 * until no new facts arise.
 */
function infer(): void {
  let changed = true;
  while (changed) {
    changed = false;

    for (const k of Array.from(facts)) {
      const [subject, category] = k.split('::') as [Subject, Category];
      if (category === 'Man') {
        const mortalKey = key(subject, 'Mortal');
        if (!facts.has(mortalKey)) {
          facts.add(mortalKey);
          changed = true;
        }
      }
    }
  }
}

/**
 * Equivalent to the Prolog query `?- type(_, _).`
 * Returns all facts as `[Subject, Category]` pairs,
 * deterministically ordered.
 */
export function queryType(): [Subject, Category][] {
  infer();
  return [...facts]
    .map(k => k.split('::') as [Subject, Category])
    .sort(([s1, c1], [s2, c2]) =>
      s1 === s2 ? c1.localeCompare(c2) : s1.localeCompare(s2)
    );
}

/* ──────────────────────────────────────────────────────────
   Demo (mirrors the original Prolog query “?- type(_, _).”)
   ────────────────────────────────────────────────────────── */

assertType('Socrates', 'Man');

if (typeof require !== 'undefined' && require.main === module) {
  for (const [subject, category] of queryType()) {
    console.log(`type('${subject}', '${category}')`);
  }
}
