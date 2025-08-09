# ============================================
# EXPLAIN-AND-CHECK (LOGIC): paths in a tiny graph
# Claim (concrete): path(paris, nantes)  — and more generally which cities reach nantes
# ============================================
# Vocabulary:
#   oneway(u,v): there is a directed edge u → v  (a fact)
#   path(u,v):   there is a (nonempty) directed path u → … → v
#
# Inference rules (natural deduction reading):
#   (P1) ∀u∀v ( oneway(u,v) → path(u,v) )              # base case
#   (P2) ∀u∀v∀z ( (oneway(u,z) ∧ path(z,v)) → path(u,v) )   # recursive step
#
# Facts (edges):
EDGES = [
    ("paris",     "orleans"),
    ("paris",     "chartres"),
    ("paris",     "amiens"),
    ("orleans",   "blois"),
    ("orleans",   "bourges"),
    ("blois",     "tours"),
    ("chartres",  "lemans"),
    ("lemans",    "angers"),
    ("lemans",    "tours"),
    ("angers",    "nantes"),
]
GOAL = "nantes"

# -----------------------------
# Program output: the “reason why”
# -----------------------------

print("============================================")
print("Reason why: path(paris, nantes)")
print("============================================\n")

print("Rules:")
print("  (P1) ∀u∀v ( oneway(u,v) → path(u,v) )     # base case")
print("  (P2) ∀u∀v∀z ( (oneway(u,z) ∧ path(z,v)) → path(u,v) )  # recursive step\n")

print("Facts (some edges we will use):")
print("  oneway(angers, nantes)")
print("  oneway(lemans, angers)")
print("  oneway(chartres, lemans)")
print("  oneway(paris, chartres)\n")

print("Derivation (natural-deduction style):")
print("  1) oneway(angers, nantes)                                 (fact)")
print("  2) path(angers, nantes)                                   (from 1 by P1, UI + →-Elim)")
print("  3) oneway(lemans, angers)                                  (fact)")
print("  4) path(lemans, nantes)                                    (from 3 & 2 by P2, UI + ∧-Intro + →-Elim)")
print("  5) oneway(chartres, lemans)                                 (fact)")
print("  6) path(chartres, nantes)                                   (from 5 & 4 by P2)")
print("  7) oneway(paris, chartres)                                  (fact)")
print("  8) path(paris, nantes)                                      (from 7 & 6 by P2)\n")

print("Conclusion:")
print("  Therefore, path(paris, nantes).  (Chains facts via P1/P2)\n")

print("Extra note:")
print("  The *same* pattern shows path(lemans, nantes), path(chartres, nantes), and path(angers, nantes).")
print("  Other outgoing routes (e.g., via orleans→blois→tours) do not reach nantes in this graph.\n")

# -----------------------------
# Silent CHECK: forward-chaining closure under P1/P2
# -----------------------------
# We compute the least set PATH such that:
#   • if oneway(u,v) then (u,v) ∈ PATH           (P1)
#   • if oneway(u,z) and (z,v) ∈ PATH then (u,v) ∈ PATH   (P2)
# Then we read off all sources u with (u, nantes) ∈ PATH.

# Build quick adjacency and node set (no imports)
NODES = set()
SUCCS = {}
for u,v in EDGES:
    NODES.add(u); NODES.add(v)
    s = SUCCS.get(u)
    if s is None:
        s = set(); SUCCS[u] = s
    s.add(v)

# Forward-chaining fixpoint
PATH = set()
# Seed with P1
for u,v in EDGES:
    PATH.add((u,v))
# Apply P2 to closure
changed = True
while changed:
    changed = False
    for u in SUCCS:
        for z in SUCCS[u]:
            # for every existing path z→v, add u→v
            for (p,q) in list(PATH):
                if p == z:
                    pair = (u,q)
                    if pair not in PATH:
                        PATH.add(pair)
                        changed = True

# Expected sources that reach nantes (by inspection/reasoning above)
EXPECTED = {"angers", "lemans", "chartres", "paris"}

# Extract actual sources from closure
actual_sources = set(u for (u,v) in PATH if v == GOAL)

# Checks
assert ("paris", GOAL) in PATH, "Expected path(paris, nantes) not found in closure."
assert actual_sources == EXPECTED, f"Sources reaching {GOAL} mismatch: {actual_sources} vs {EXPECTED}"

# Optional: confirm non-solutions truly don’t reach nantes
non_sources = (NODES - EXPECTED - {GOAL})
for u in non_sources:
    assert (u, GOAL) not in PATH, f"Unexpected path({u}, {GOAL}) found."

print("Harness:")
print(f"  Sources that reach {GOAL}: {sorted(actual_sources)}")
print("  All checks passed. ✓")

