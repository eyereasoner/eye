#!/usr/bin/env python3
"""
zkp.py  –  toy zero-knowledge proof (ZKP) with backward chaining
───────────────────────────────────────────────────────────────────────────
Scenario
--------
* The prover knows a secret S but never publishes it.
* Instead, the prover publishes its hash H  =  hash(S).
* The verifier sends a random nonce  N.
* The prover answers with         R  =  hash( H ‖ N ).

If (H, N, R) satisfies the public hash-concatenation predicate
`log:hashConcat`, the verifier is convinced the prover knows H (and hence S)
but learns nothing else about S.

We capture that with one rule:

    { :commit :hash ?H .
      :nonce  :value ?N .
      :response :value ?R .
      (?H ?N ?R) log:hashConcat true. }
    => { :verifier :convinced true } .

The script proves the goal `:verifier :convinced true` *backwards* and
prints an indented, numbered proof trace.

NOTE
----
• All hashes are dummy strings (H8C6, R7B5, …) – we do **not** compute
  real cryptographic hashes; we just assert the needed relations.
• The focus is reasoning, not cryptography.

Why this is “zero knowledge”:

• The secret itself never appears—only its hash H8C6.
• The verifier learns nothing beyond the fact that the prover knew a
  pre-image of H8C6, because the rule relies solely on the hash
  relation (hashConcat) which is already in the knowledge base.
"""

from itertools import count

# ──────────────────────────────
# 1.  Ground facts
# ──────────────────────────────
facts = {
    # Public commitments and values
    (":commit",   ":hash",   "H8C6"),
    (":nonce",    ":value",  "N123"),
    (":response", ":value",  "R7B5"),

    # Public statement that H‖N hashes to R
    (("H8C6", "N123", "R7B5"), "log:hashConcat", "true"),
}

# ──────────────────────────────
# 2.  Single rule
# ──────────────────────────────
rule = {
    "id": "R-convince",
    "head": (":verifier", ":convinced", "true"),
    "body": [
        (":commit",   ":hash",   "?H"),
        (":nonce",    ":value",  "?N"),
        (":response", ":value",  "?R"),
        (("?H", "?N", "?R"), "log:hashConcat", "true"),
    ],
}

# ──────────────────────────────
# 3.  Unification helpers
# ──────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pat, fact, θ=None):
    """
    Recursively unify pattern `pat` with ground `fact` under environment θ.
    Returns extended θ or None if unification fails.
    """
    θ = dict(θ or {})

    # both atoms
    if not isinstance(pat, tuple) and not isinstance(fact, tuple):
        if is_var(pat):
            if pat in θ and θ[pat] != fact:
                return None
            θ[pat] = fact
            return θ
        return θ if pat == fact else None

    # one is tuple, other not -> impossible
    if isinstance(pat, tuple) ^ isinstance(fact, tuple):
        return None

    # both tuples
    if len(pat) != len(fact):
        return None
    for p_elem, f_elem in zip(pat, fact):
        θ = unify(p_elem, f_elem, θ)
        if θ is None:
            return None
    return θ

def subst(term, θ):
    """Apply substitution θ to atom or tuple."""
    if isinstance(term, tuple):
        return tuple(subst(t, θ) for t in term)
    return θ.get(term, term)

# ──────────────────────────────
# 4.  Backward-chaining prover
# ──────────────────────────────
step = count(1)

def bc(goal, θ, depth):
    """Generator yielding substitutions that satisfy goal."""
    g = subst(goal, θ)
    print("  " * depth + f"Step {next(step):02}: prove {g}")

    # 1.  Ground fact matching (deterministic order)
    for f in sorted(facts, key=str):
        θ2 = unify(g, f, θ)
        if θ2 is not None:
            print("  " * depth + f"✓ fact {f}")
            yield θ2                         # keep searching for alt proofs

    # 2.  Rule application (only one rule here)
    θhead = unify(rule["head"], g, θ)
    if θhead is None:
        return
    print("  " * depth + f"→ via {rule['id']}")

    def prove_body(i, θcur):
        if i == len(rule["body"]):
            yield θcur
        else:
            for θn in bc(rule["body"][i], θcur, depth + 1):
                yield from prove_body(i + 1, θn)

    yield from prove_body(0, θhead)

# ──────────────────────────────
# 5.  Run the query
# ──────────────────────────────
goal = (":verifier", ":convinced", "true")
print(f"\n=== Proving {goal} ===\n")

gen = bc(goal, {}, 0)
proved = next(gen, None) is not None
print("\n✔ PROVED" if proved else "\n✗ NOT PROVED")

