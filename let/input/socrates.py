# --- simple inference engine -----------------------------------------------

# storage for asserted facts
_facts: set[tuple[str, str]] = set()

def assert_type(subject: str, category: str) -> None:
    """Add a fact type(S, C) to the knowledge base."""
    _facts.add((subject, category))

def _infer() -> None:
    """Apply the rule   type(X,'Man') → type(X,'Mortal')   until saturation."""
    changed = True
    while changed:                      # keep looping until no new facts
        changed = False
        new_facts = {
            (s, "Mortal")
            for (s, c) in _facts
            if c == "Man" and (s, "Mortal") not in _facts
        }
        if new_facts:                   # at least one new fact was derived
            _facts.update(new_facts)
            changed = True

def query_type() -> list[tuple[str, str]]:
    """Return every known (explicit + inferred) type/2 fact."""
    _infer()
    return sorted(_facts)
# ---------------------------------------------------------------------------

# seed the knowledge base with the fact from your program
assert_type("Socrates", "Man")

if __name__ == "__main__":
    # equivalent to the Prolog query: ?- type(_, _).
    for fact in query_type():
        print(f"type{fact}")
