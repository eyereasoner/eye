# =============================================================================
# family.py
# =============================================================================
#
# What this is
# ------------
# A tiny Horn-logic forward-chaining reasoner with proof recording for a
# family tree. It saturates a knowledge base of facts + Horn rules,
# stores one proof tree per derived fact, and exposes a small convenience API.
#
# Contract & invariants
# ---------------------
# - Facts are triples: (subject, predicate, object).
# - Rules are Horn clauses:   head :- body1, ..., bodyn
# - Variables start with '?'.
# - Built-in: inequality "≠" in rule bodies.
# - Output sections printed by main(): "Answer", "Reason why", "Check (harness)".
#
# Knobs you can change
# --------------------
# - To enforce *strict* siblings (never x==y), add ("?x","≠","?y") to the
#   "sibling" rule body below. The user-facing helpers won’t need changes.
#
# Notes on complexity
# -------------------
# - The implementation scans all facts when joining rule literals. For these
#   small KBs it’s simple and robust. If the KB grows, add a predicate index
#   (map: predicate -> list of facts) in _candidates_for().
# =============================================================================

from __future__ import annotations
from collections import namedtuple
from itertools import count
from typing import Dict, Iterable, List, Optional, Tuple

Triple = Tuple[str, str, str]
Subst  = Dict[str, str]

# ──────────────────────────────────────────────────────────────
# helpers: variables, unification, substitution, renaming
# ──────────────────────────────────────────────────────────────

def is_var(t: str) -> bool:
    """A term is a (Logic) variable if it starts with '?'."""
    return isinstance(t, str) and t.startswith("?")

def unify(pat: Triple, fact: Triple, theta: Subst) -> Optional[Subst]:
    """
    Unify ONE pattern triple with ONE fact under current θ (no occurs-check).
    Returns an extended substitution or None on clash.
    """
    θ: Subst = dict(theta)
    for p, f in zip(pat, fact):
        if is_var(p):                      # variable in pattern
            if p in θ and θ[p] != f:
                return None
            θ[p] = f
        elif is_var(f):                    # (rare) variable inside a fact
            if f in θ and θ[f] != p:
                return None
            θ[f] = p
        elif p != f:                       # both ground but different
            return None
    return θ

def subst(triple: Triple, theta: Subst) -> Triple:
    """Substitute θ into a triple (may stay partially ground)."""
    return tuple(theta.get(t, t) for t in triple)  # type: ignore[return-value]

# Fresh-variable renaming per rule application
gensym  = count(1).__next__
Clause  = namedtuple("Clause", "name head body")

def rename(clause: Clause) -> Clause:
    """Renames all variables in a clause to fresh ones (per application)."""
    ren: Dict[str, str] = {}
    def f(t: str) -> str:
        return ren.setdefault(t, f"{t}_{gensym()}") if is_var(t) else t
    head = tuple(map(f, clause.head))
    body = [tuple(map(f, lit)) for lit in clause.body]
    return Clause(clause.name, head, body)

# ──────────────────────────────────────────────────────────────
# knowledge base (facts)
# ──────────────────────────────────────────────────────────────
base_facts: List[Triple] = [
    # gender
    ("Frans", "a", "MALE"),
    ("Jo", "a", "MALE"),
    ("Paul", "a", "MALE"),
    ("Pieter-Jan", "a", "MALE"),
    ("Tim", "a", "MALE"),
    ("Bert", "a", "MALE"),
    ("Bart", "a", "MALE"),
    ("Maria", "a", "FEMALE"),
    ("Maaike", "a", "FEMALE"),
    ("Rita", "a", "FEMALE"),
    ("Goedele", "a", "FEMALE"),
    ("Veerle", "a", "FEMALE"),
    ("Ann", "a", "FEMALE"),
    ("Veer", "a", "FEMALE"),

    # parent
    ("Frans", "parent", "Jo"),
    ("Maria", "parent", "Jo"),
    ("Frans", "parent", "Rita"),
    ("Maria", "parent", "Rita"),
    ("Jo", "parent", "Goedele"),
    ("Maaike", "parent", "Goedele"),
    ("Jo", "parent", "Veerle"),
    ("Maaike", "parent", "Veerle"),
    ("Paul", "parent", "Ann"),
    ("Rita", "parent", "Ann"),
    ("Paul", "parent", "Bart"),
    ("Rita", "parent", "Bart"),

    # spouse  (one direction; symmetry via rule)
    ("Frans", "spouse", "Maria"),
    ("Jo", "spouse", "Maaike"),
    ("Paul", "spouse", "Rita"),
    ("Pieter-Jan", "spouse", "Goedele"),
    ("Tim", "spouse", "Veerle"),
    ("Bert", "spouse", "Ann"),
    ("Bart", "spouse", "Veer"),
]

# ──────────────────────────────────────────────────────────────
# rules (Horn clauses)
# ──────────────────────────────────────────────────────────────
# Sibling is *permissive* (x and y may be equal). We prevent
# “parent-as-uncle/aunt” at the rule level via x ≠ p in the
# blood rules; the API also filters parents from avuncular results.
rules: List[Clause] = [
    # symmetry of spouse
    Clause("sym-spouse",
           ("?y","spouse","?x"),
           [("?x","spouse","?y")]),

    # siblings (permissive)
    Clause("sibling",
           ("?x","sibling","?y"),
           [("?p","parent","?x"),
            ("?p","parent","?y")]),          # add ("?x","≠","?y") here for strict siblings

    Clause("sym-sib",
           ("?y","sibling","?x"),
           [("?x","sibling","?y")]),

    # gender-specialised siblings
    Clause("brother",
           ("?x","brother","?y"),
           [("?x","sibling","?y"),
            ("?x","a","MALE")]),

    Clause("sister",
           ("?x","sister","?y"),
           [("?x","sibling","?y"),
            ("?x","a","FEMALE")]),

    # grand relations
    Clause("grandparent",
           ("?x","grandparent","?z"),
           [("?x","parent","?y"),
            ("?y","parent","?z")]),

    Clause("grandfather",
           ("?x","grandfather","?z"),
           [("?x","grandparent","?z"),
            ("?x","a","MALE")]),

    Clause("grandmother",
           ("?x","grandmother","?z"),
           [("?x","grandparent","?z"),
            ("?x","a","FEMALE")]),

    # immediate parent
    Clause("father",
           ("?x","father","?y"),
           [("?x","parent","?y"),
            ("?x","a","MALE")]),

    Clause("mother",
           ("?x","mother","?y"),
           [("?x","parent","?y"),
            ("?x","a","FEMALE")]),

    # uncles & aunts (blood + by marriage)
    # IMPORTANT: block "parent-as-uncle/aunt" by requiring x ≠ p
    Clause("uncle-blood",
           ("?x","uncle","?y"),
           [("?x","brother","?p"),
            ("?p","parent","?y"),
            ("?x","≠","?p")]),               # guard

    Clause("aunt-blood",
           ("?x","aunt","?y"),
           [("?x","sister","?p"),
            ("?p","parent","?y"),
            ("?x","≠","?p")]),               # guard

    Clause("uncle-mar",
           ("?x","uncle","?y"),
           [("?x","spouse","?s"),
            ("?s","aunt","?y")]),

    Clause("aunt-mar",
           ("?x","aunt","?y"),
           [("?x","spouse","?s"),
            ("?s","uncle","?y")]),
]

# ──────────────────────────────────────────────────────────────
# forward saturation with proof recording
# ──────────────────────────────────────────────────────────────
facts_list: List[Triple] = list(base_facts)     # preserves insertion order
facts_set  = set(base_facts)                    # O(1) membership
proofs: Dict[Triple, Tuple[str, List[Triple]]] = {f: ("fact", []) for f in base_facts}

def add_fact(t: Triple, rule_name: str, premises: List[Triple]) -> None:
    """Insert new fact exactly once, keeping the list's stable order."""
    if t not in facts_set:
        facts_set.add(t)
        facts_list.append(t)
        proofs[t] = (rule_name, premises)

def _candidates_for(lit: Triple) -> Iterable[Triple]:
    """
    Candidate generator for a body literal.
    We do a full scan (dataset is small) which is simple and robust.
    If you need scale, replace with a predicate index here.
    """
    if not is_var(lit[1]):
        pred = lit[1]
        return (f for f in facts_list if f[1] == pred)
    return facts_list

def saturate() -> None:
    """Forward-chain until no new facts are added; record one proof per new fact."""
    changed = True
    while changed:
        changed = False
        for rule in rules:                 # deterministic rule order
            rc = rename(rule)              # fresh variables per pass
            substitutions: List[Subst] = [{}]
            premises:      List[List[Triple]] = [[]]

            # incremental join over the clause body
            for lit in rc.body:
                new_subs: List[Subst] = []
                new_prem: List[List[Triple]] = []

                # built-in inequality
                if lit[1] == "≠":
                    for θ, pr in zip(substitutions, premises):
                        a, _, b = subst(lit, θ)
                        if a != b:
                            new_subs.append(θ)
                            new_prem.append(pr)
                    substitutions, premises = new_subs, new_prem
                    if not substitutions:
                        break
                    continue

                # standard literal: unify against candidate facts
                for θ, pr in zip(substitutions, premises):
                    lit_inst = subst(lit, θ)
                    for fact in _candidates_for(lit_inst):
                        θ2 = unify(lit_inst, fact, θ)
                        if θ2 is not None:
                            new_subs.append(θ2)
                            new_prem.append(pr + [fact])

                substitutions, premises = new_subs, new_prem
                if not substitutions:      # early fail
                    break

            # add each resulting head instance
            for θ, body_facts in zip(substitutions, premises):
                head_inst = subst(rc.head, θ)
                if head_inst not in facts_set:
                    add_fact(head_inst, rc.name, body_facts)
                    changed = True

# Run saturation once on import
saturate()

# ──────────────────────────────────────────────────────────────
# proof display helpers
# ──────────────────────────────────────────────────────────────
def _show(triple: Triple, depth: int = 0) -> None:
    ind = "  " * depth
    kind, prem = proofs[triple]
    if kind == "fact":
        print(f"{ind}{triple}   [fact]")
    else:
        print(f"{ind}{triple}   [via {kind}]")
        for p in prem:
            _show(p, depth + 1)

def prove_query(pattern: Triple) -> bool:
    """Print a proof for *every* matching fact. Returns True if any proof was printed."""
    found = False
    for f in facts_list:
        θ = unify(pattern, f, {})
        if θ is None:
            continue
        found = True
        inst = subst(pattern, θ)
        print(f"\n--- Proof for {inst} (θ = {θ}) ---")
        _show(f)
    return found

# ──────────────────────────────────────────────────────────────
# convenience API (correct projections + gentle filtering)
# ──────────────────────────────────────────────────────────────

def q(pred: str, s: Optional[str] = None, o: Optional[str] = None) -> List[Triple]:
    """Return all triples with predicate `pred`, filtered by subject/object if provided."""
    return [(S,P,O) for (S,P,O) in facts_list if P == pred
            and (s is None or S == s) and (o is None or O == o)]

def father(c: str) -> Optional[str]:
    t = next(iter(q("father", None, c)), None)
    return t[0] if t else None

def mother(c: str) -> Optional[str]:
    t = next(iter(q("mother", None, c)), None)
    return t[0] if t else None

def children(p: str) -> List[str]:
    """All children of parent p (sorted, unique)."""
    return sorted({O for (S,_,O) in q("parent", p, None)})

def siblings(x: str) -> List[str]:
    """
    Siblings of x (sorted, unique), excluding x itself in the *API*.
    The logic may contain (x, sibling, x) due to permissive rule; we drop it here.
    """
    return sorted({O for (S,_,O) in q("sibling", x, None)} - {x})

def brothers(x: str) -> List[str]:
    return sorted({S for (S,_,O) in q("brother", None, x)})

def grandps(x: str) -> List[str]:
    return sorted({S for (S,_,O) in q("grandparent", None, x)})

def uncles(x: str) -> List[str]:
    """All uncles of x (sorted, unique). Direct parents are always excluded in the API."""
    parents = {S for (S,_,O) in q("parent", None, x)}
    return sorted({S for (S,_,O) in q("uncle", None, x)} - parents)

def aunts(x: str) -> List[str]:
    """All aunts of x (sorted, unique). Direct parents are always excluded in the API."""
    parents = {S for (S,_,O) in q("parent", None, x)}
    return sorted({S for (S,_,O) in q("aunt", None, x)} - parents)

# ──────────────────────────────────────────────────────────────
# demo & report (exactly three sections)
# ──────────────────────────────────────────────────────────────

def main() -> None:
    # ===== Answer =====
    print("Answer")
    print("------")
    print(f"children('Rita')   -> {children('Rita')}")
    print(f"siblings('Veerle') -> {siblings('Veerle')}")
    print(f"uncles('Veerle')   -> {uncles('Veerle')}")
    print(f"aunts('Bart')      -> {aunts('Bart')}")
    print(f"father('Goedele')  -> {father('Goedele')}")
    print(f"mother('Goedele')  -> {mother('Goedele')}")

    # A couple of proof trees
    print("\nProofs (selected):")
    prove_query(("Rita", "aunt", "Veerle"))      # blood
    prove_query(("Paul", "uncle", "Veerle"))     # by marriage
    prove_query(("Frans", "grandfather", "Goedele"))

    # ===== Reason why =====
    print("\nReason why")
    print("----------")
    print("1) Fixed projection in children(): we now return the OBJECT (child) of parent facts.")
    print("2) Sibling rule remains permissive (x may equal y), but 'uncle-blood' and 'aunt-blood'")
    print("   include a guard (x ≠ p) so a parent isn't mis-classified as an uncle/aunt.")
    print("3) User-facing siblings() excludes self; uncles()/aunts() exclude direct parents.")
    print("4) Deterministic forward chaining with proof recording; facts are added once with a stable order.")

    # ===== Check (harness) =====
    print("\nCheck (harness)")
    print("----------------")
    ok_children = (children('Rita') == ['Ann', 'Bart'])
    ok_siblings = (siblings('Veerle') == ['Goedele'])
    ok_uncles   = (uncles('Veerle') == ['Paul'])           # Jo is Veerle's father; not an uncle
    ok_father   = (father('Goedele') == 'Jo')
    ok_mother   = (mother('Goedele') == 'Maaike')

    before = len(facts_list); saturate(); after = len(facts_list)
    ok_fixpoint = (before == after)

    print(f"children('Rita')   == ['Ann', 'Bart'] : {ok_children}")
    print(f"siblings('Veerle') == ['Goedele']     : {ok_siblings}")
    print(f"uncles('Veerle')   == ['Paul']        : {ok_uncles}")
    print(f"father('Goedele')  == 'Jo'            : {ok_father}")
    print(f"mother('Goedele')  == 'Maaike'        : {ok_mother}")
    print(f"Fixpoint reached (no new facts)       : {ok_fixpoint}")
    print(f"\nALL CHECKS PASS: {ok_children and ok_siblings and ok_uncles and ok_father and ok_mother and ok_fixpoint}")

if __name__ == "__main__":
    main()

