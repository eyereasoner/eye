#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
README (plain text)
===================
Purpose
-------
This single-file program is a “branch of insights” in the spirit of
https://eyereasoner.github.io/eye/brains/ . It demonstrates how a problem that
*looks* like Second-Order Logic (SOL)—because it quantifies over **sets** and
**set-operators**—can be modeled with a **first-order core** using a small,
fixed set of predicates. This follows the core idea of Hayes–Menzel.

Core idea (Hayes–Menzel, in plain words)
----------------------------------------
- Treat things like “the set of evens” or “the successor relation” or even
  “the operator that maps a set S to S U succ(S)” as **named objects**.
  These **names** are called **intensions** (think: URIs or constants).
- Keep a single, fixed predicate for applying those intensions to arguments:
    • `Holds₁(S, x)` means: “x is a member of the set *named by* S.”
    • `Holds₂(op, S, x)` means: “x is a member of F_op(S), i.e., applying the
      set-operator *named by* op to the set *named by* S.”
- This avoids quantifying over predicates or functions themselves. We only ever
  quantify over **names** (ordinary objects), so the core stays first-order.

Intension vs Extension (gentle glossary)
----------------------------------------
- **Intension**: the *name* of a thing (like a URI, string, or constant).
  Example: `"ex:Evens"` is a name for the set of even numbers.
- **Extension**: the actual *collection of elements* the name refers to.
  Example: `{0,2,4}` is the extension of `"ex:Evens"` in our tiny world.
- Two different names can share the same extension (e.g., aliases).

What this program asks and answers (ambitious question)
-------------------------------------------------------
We work over a tiny domain D = {0,1,2,3,4}. We define several **named set-operators**:
each takes a subset of D and returns a new subset of D (e.g., “closure from 0
under successor”). For **each operator F** we:

1) Check whether F is **monotone**:
   For all S ⊆ T, do we have F(S) ⊆ F(T)?
   (Monotone operators are nice because fixed points exist by Tarski’s Theorem.)

2) If F is monotone, we compute its **least fixed point** (LFP(F)) in two ways:
   - **Kleene iteration**: start from ∅, repeatedly apply F, and see where it
     stabilizes.
   - **Tarski characterization**: LFP(F) = intersection of all *pre-fixed* sets,
     i.e., all S with F(S) ⊆ S.

3) If F is not monotone, we exhibit a **concrete counterexample** S ⊆ T with
   F(S) ⊄ F(T), and describe the Kleene chain from ∅ (which need not converge).

Why this shows the Hayes–Menzel trick
-------------------------------------
- “Quantifying over sets” is usually labeled SOL. Here, sets and operators are
  just **named objects**. Membership is expressed by a single fixed predicate
  `Holds₁`, and operator application by `Holds₂`. That’s a **first-order**
  signature.
- Despite the SOL *flavor*, the reasoning is carried out by plain data and
  fixed predicates. This mirrors how RDF/CLIF/CL treat classes/properties as
  first-order objects while keeping a first-order model theory.

What the program prints
-----------------------
1) **Model**  — domain, named sets and operators, and the successor relation.
2) **Question** — the ambitious question stated clearly.
3) **Answer** — monotonicity classification and least fixed points (when exist).
4) **Reason why** — short explanations in mathematical English (Kleene + Tarski,
   or a concrete monotonicity counterexample).
5) **Check (harness)** — 13 deterministic checks to validate everything.

How to run
----------
    python3 holdsn_lfp_tarski.py

No external dependencies; deterministic execution and output.
"""

from __future__ import annotations

from typing import Iterable, Tuple, Dict, Set, List, Callable, Optional

# =========================================================
# Model: domain D, Holds₁ for sets, Holds₂ for set-operators
# =========================================================

# We use a tiny, *fixed* domain, with a deterministic order to keep output stable.
D: Tuple[int, ...] = (0, 1, 2, 3, 4)

# We'll format names (intensions) as URI-like strings in an "ex:" namespace.
EX = "ex:"

# --- Unary side: "named sets" (intensions) and their extensions ----------------
# EXT1 maps a *name* (string) to the *extension* (a Python set of ints).
EXT1: Dict[str, Set[int]] = {}

def define_set(name: str, elems: Iterable[int]) -> str:
    """
    Register the extension (elements) for a named set (its intension).
    Example: define_set("ex:Evens", [0,2,4]) makes Holds1("ex:Evens", 2) True.
    """
    EXT1[name] = set(sorted(elems))  # sort -> deterministic storage
    return name

def Holds1(Sname: str, x: int) -> bool:
    """
    Read: Holds₁(S, x)  ≡  x ∈ extension of the set *named by* S.
    S is a *name* (intension), not a predicate symbol.
    """
    return x in EXT1.get(Sname, set())

# Define some named sets for display and testing.
N            = define_set(EX + "N",            D)         # the whole domain
Evens        = define_set(EX + "Evens",        [0, 2, 4]) # even numbers in D
Odds         = define_set(EX + "Odds",         [1, 3])    # odd numbers in D
Prefix012    = define_set(EX + "Prefix012",    [0, 1, 2]) # initial segment
ZeroOnly     = define_set(EX + "ZeroOnly",     [0])       # singleton {0}
EmptySet     = define_set(EX + "EmptySet",     [])        # empty set ∅

# An alias with the *same extension* as N, to illustrate intension ≠ extension.
omegaAlias   = define_set(EX + "omegaAlias",   D)

# We'll keep a small list of named sets for pretty printing.
NAMED_SETS: List[str] = [N, omegaAlias, Evens, Odds, Prefix012, ZeroOnly, EmptySet]

# --- Binary side: a named relation succ(x,y) ≡ y = x + 1 (within D) ----------
# EXT2 maps a *relation name* to a set of ordered pairs (its extension).
EXT2: Dict[str, Set[Tuple[int, int]]] = {}

def define_relation(name: str, pairs: Iterable[Tuple[int, int]]) -> str:
    """
    Register the extension (pairs) for a named binary relation.
    Example: define_relation("ex:succ", [(0,1),(1,2),...]).
    """
    EXT2[name] = {(a, b) for (a, b) in pairs}  # set of pairs
    return name

def Holds2(rel: str, x: int, y: int) -> bool:
    """
    Read: Holds₂(rel, x, y)  ≡  ⟨x,y⟩ ∈ extension of the relation named by rel.
    """
    return (x, y) in EXT2.get(rel, set())

# Define the successor relation restricted to D.
succ = define_relation(EX + "succ", [(0, 1), (1, 2), (2, 3), (3, 4)])

def succ_image(S: Set[int]) -> Set[int]:
    """
    Image under succ: { y | ∃x∈S . succ(x,y) }.
    Deterministic because EXT2[succ] is iterated in sorted order.
    """
    out: Set[int] = set()
    for (a, b) in sorted(EXT2[succ]):
        if a in S:
            out.add(b)
    return out

def pred_image(S: Set[int]) -> Set[int]:
    """
    Predecessor image under succ: { x | ∃y∈S . succ(x,y) }.
    """
    out: Set[int] = set()
    for (a, b) in sorted(EXT2[succ]):
        if b in S:
            out.add(a)
    return out

# --- Powerset utility: we will sometimes iterate over *all* subsets of D ------
def all_subsets(domain: Tuple[int, ...]) -> List[Set[int]]:
    """
    Enumerate all subsets of 'domain' (the powerset), in a deterministic order:
    by increasing size, then lexicographic by elements. This keeps proofs and
    counterexamples printed in a stable, human-friendly order.
    """
    n = len(domain)
    subs: List[Set[int]] = []
    order = list(domain)
    for mask in range(1 << n):
        s = {order[i] for i in range(n) if (mask >> i) & 1}
        subs.append(s)
    subs.sort(key=lambda s: (len(s), tuple(sorted(s))))
    return subs

ALL_SUBSETS: List[Set[int]] = all_subsets(D)
BOTTOM: Set[int] = set()
TOP: Set[int] = set(D)

# =============================
# Named set-operators (intensions)
# =============================
# We let each *operator name* map to a Python function F: P(D) -> P(D).
# Importantly: the *name* is the intension; the function realizes its extension.
OP_FUNC: Dict[str, Callable[[Set[int]], Set[int]]] = {}

def define_operator(name: str, F: Callable[[Set[int]], Set[int]]) -> str:
    """
    Register an operator name and its behavior (a Python function over sets).
    This mirrors: Holds₂(op, S, x)  ≡  x ∈ F_op(S).
    """
    OP_FUNC[name] = F
    return name

# Monotone operators (F(S) grows monotonically with S)
op_closure_from_zero   = define_operator(EX + "F_closureFrom0",
    lambda S: {0} | succ_image(S))                 # adds 0 and one-step successors
op_constant_prefix012  = define_operator(EX + "F_constPrefix012",
    lambda S: {0, 1, 2})                           # ignores S; constant operator
op_evens_or_succ       = define_operator(EX + "F_evensOrSucc",
    lambda S: set(EXT1[Evens]) | succ_image(S))    # always keeps Evens and adds successors
op_from_top_pred       = define_operator(EX + "F_fromTopPred",
    lambda S: {4} | pred_image(S))                 # adds 4 and predecessors of S

# Non-monotone operators (used to show what breaks)
op_toggle_complement   = define_operator(EX + "F_toggle",
    lambda S: set(D) - set(S))                     # complement toggling: ∅↔D↔∅↔…
op_branching_weird     = define_operator(EX + "F_weird",
    lambda S: {0} if 1 in S else {1})              # behavior flips on whether 1∈S

MONOTONE_OPS: List[str]     = [op_closure_from_zero, op_constant_prefix012, op_evens_or_succ, op_from_top_pred]
NON_MONOTONE_OPS: List[str] = [op_toggle_complement, op_branching_weird]
ALL_OPS: List[str]          = MONOTONE_OPS + NON_MONOTONE_OPS

def apply_op(op: str, S: Set[int]) -> Set[int]:
    """
    Apply operator 'op' to set 'S' to get F_op(S).
    We wrap with 'set(sorted(...))' for deterministic output ordering.
    """
    return set(sorted(OP_FUNC[op](set(S))))

def Holds2_op(op: str, Sname: str, x: int) -> bool:
    """
    Read: Holds₂(op, S, x)  ≡  x ∈ F_op(S), where S is the *name* of a set.
    This is exactly the Hayes–Menzel “application” pattern: a fixed predicate
    connects intensions (names) to their extensions.
    """
    return x in apply_op(op, EXT1[Sname])

# ==========================
# First-order style predicates
# ==========================

def SubsetEq(A: Set[int], B: Set[int]) -> bool:
    """A ⊆ B"""
    return A.issubset(B)

def ExtEq1(Sname1: str, Sname2: str) -> bool:
    """Extensionally equal named sets"""
    return EXT1[Sname1] == EXT1[Sname2]

def is_monotone(op: str) -> bool:
    """
    Check ∀S⊆T: F(S) ⊆ F(T). Since D is finite, we can brute-force *all* subsets.
    Deterministic enumeration guarantees stable witnesses and messages.
    """
    for i, S in enumerate(ALL_SUBSETS):
        for T in ALL_SUBSETS[i:]:
            if not S.issubset(T):
                continue
            if not apply_op(op, S).issubset(apply_op(op, T)):
                return False
    return True

def first_monotonicity_counterexample(op: str) -> Optional[Tuple[Set[int], Set[int]]]:
    """
    If op is not monotone, return the *first* S⊆T (in our deterministic order)
    such that F(S) ⊄ F(T). Otherwise return None.
    """
    for i, S in enumerate(ALL_SUBSETS):
        for T in ALL_SUBSETS[i:]:
            if not S.issubset(T):
                continue
            if not apply_op(op, S).issubset(apply_op(op, T)):
                return (S, T)
    return None

# Kleene and Tarski constructions (finite lattice = guaranteed termination behaviors)
def kleene_lfp(op: str, max_steps: int = 1 << len(D)) -> Tuple[Optional[Set[int]], List[Set[int]]]:
    """
    Kleene iteration from the bottom element ∅:
      S₀ := ∅;  S_{n+1} := F(S_n).
    If the sequence stabilizes at step k, we return (S_k, [S₀, …, S_k]).
    If it cycles (possible for non-monotone F), we return (None, chain_so_far).
    """
    chain: List[Set[int]] = [set()]
    seen: Dict[Tuple[int, ...], int] = {(): 0}  # remember visited sets by sorted tuple
    S = set()
    for _ in range(max_steps):
        S_next = apply_op(op, S)
        chain.append(S_next)
        if S_next == S:
            return S_next, chain  # fixed point reached
        key = tuple(sorted(S_next))
        if key in seen:
            # cycle: no fixed point reached by this process
            return None, chain
        seen[key] = len(chain) - 1
        S = S_next
    return None, chain  # defensive fallback (should not happen for our finite cases)

def tarski_lfp(op: str) -> Set[int]:
    """
    Tarski’s characterization:
      LFP(F) = ⋂ { S ⊆ D | F(S) ⊆ S }   (intersection of all *pre-fixed* sets).
    For finite D we can directly compute this intersection.
    """
    prefixed: List[Set[int]] = [S for S in ALL_SUBSETS if apply_op(op, S).issubset(S)]
    out = set(TOP)
    for S in prefixed:
        out &= S
    return out

# ======================
# Pretty-print utilities
# ======================

def fmt_set(S: Iterable[int]) -> str:
    """Human-friendly set printer with deterministic ordering."""
    seq = list(sorted(S))
    return "{" + ", ".join(str(x) for x in seq) if seq else "∅"

def fmt_chain(chain: List[Set[int]], limit: int = 8) -> str:
    """
    Render a Kleene chain like: ∅ ⊆ {0} ⊆ {0,1} ⊆ … (truncated with … if long).
    """
    parts = []
    for S in chain[:limit]:
        seq = list(sorted(S))
        parts.append("∅" if not seq else "{" + ", ".join(str(x) for x in seq) + "}")
    if len(chain) > limit:
        parts.append("…")
    return " ⊆ ".join(parts)

# ======================
# The Branch: Model → Q → A → Why
# ======================

def print_model() -> None:
    """Explain the world the program reasons about."""
    print("Model")
    print("=====")
    print(f"Domain D = {list(D)}")
    print()
    print("Signature")
    print("---------")
    print("• Holds₁(S, x): x ∈ extension of the set named by S (S is an *intension*).")
    print("• Holds₂(op, S, x): x ∈ F_op(S), where op names a set-operator F_op: P(D)→P(D).")
    print("• Named binary relation ex:succ with extension {(x,y) | y = x+1 within D}.")
    print()
    print("Named subsets (for display)")
    print("---------------------------")
    for Sname, blurb in [
        (N,          "the whole domain"),
        (omegaAlias, "alias of the whole domain (different name)"),
        (Evens,      "even numbers"),
        (Odds,       "odd numbers"),
        (Prefix012,  "prefix {0,1,2}"),
        (ZeroOnly,   "singleton {0}"),
        (EmptySet,   "empty set"),
    ]:
        print(f"- {Sname:<14}: {fmt_set(EXT1[Sname])} — {blurb}")
    print()
    print("Named set-operators (intensions)")
    print("--------------------------------")
    descriptions = {
        op_closure_from_zero:  "F(S) = {0} ∪ succ(S)                  (monotone)",
        op_constant_prefix012: "F(S) = {0,1,2}                        (monotone, constant)",
        op_evens_or_succ:      "F(S) = Evens ∪ succ(S)                (monotone)",
        op_from_top_pred:      "F(S) = {4} ∪ pred(S)                  (monotone)",
        op_toggle_complement:  "F(S) = D \\ S                          (non-monotone)",
        op_branching_weird:    "F(S) = {0} if 1∈S else {1}            (non-monotone)"
    }
    for op in ALL_OPS:
        print(f"- {op:<24}: {descriptions[op]}")
    print()

def print_question() -> None:
    """State the ambitious question in user-facing mathematical English."""
    print("Question")
    print("========")
    print("For each *named* set-operator F: P(D)→P(D),")
    print("  (i) determine whether F is monotone;")
    print("  (ii) if monotone, compute its **least fixed point** LFP(F),")
    print("       and justify it by BOTH Kleene iteration from ∅ and Tarski’s characterization")
    print("       LFP(F) = ⋂{ S ⊆ D | F(S) ⊆ S }.")
    print("  (iii) if not monotone, exhibit a concrete S ⊆ T with F(S) ⊄ F(T),")
    print("        and describe the behavior of the Kleene chain from ∅.")
    print()

def compute_answers() -> Dict[str, str]:
    """
    Produce a concise answer summary per operator:
      - If monotone: the least fixed point (by Tarski).
      - If non-monotone: a specific counterexample S⊆T with F(S)⊄F(T).
    """
    answers: Dict[str, str] = {}
    for op in ALL_OPS:
        mono = is_monotone(op)
        if mono:
            lfp_k, chain = kleene_lfp(op)
            lfp_t = tarski_lfp(op)
            answers[op] = f"LFP(F) = {fmt_set(lfp_t)}"
        else:
            witness = first_monotonicity_counterexample(op)
            answers[op] = f"Not monotone; witness S⊆T with F(S)⊄F(T): S={fmt_set(witness[0])}, T={fmt_set(witness[1])}"
    return answers

def print_answer(answers: Dict[str, str]) -> None:
    """Print the short answers in a predictable order: monotone first, then non-monotone."""
    print("Answer")
    print("======")
    for op in MONOTONE_OPS:
        print(f"- {op}: {answers[op]}")
    for op in NON_MONOTONE_OPS:
        print(f"- {op}: {answers[op]}")
    print()

def reason_for(op: str) -> str:
    """
    Provide a human-readable justification:
      - Monotone: show Kleene chain, show Tarski intersection result, assert equality.
      - Non-monotone: give an explicit S⊆T with F(S)⊄F(T) and show Kleene chain behavior.
    """
    if is_monotone(op):
        lfp_k, chain = kleene_lfp(op)
        lfp_t = tarski_lfp(op)
        chain_txt = fmt_chain(chain, limit=8)
        return (f"{op} is monotone, so by Tarski the least fixed point exists. "
                f"Kleene iteration from ∅ produces the ascending chain:\n"
                f"  {chain_txt}\n"
                f"which stabilizes at {fmt_set(lfp_k)}. "
                f"The Tarski intersection of all pre-fixed sets also yields {fmt_set(lfp_t)}; "
                f"therefore LFP(F) = {fmt_set(lfp_t)}.")
    else:
        S, T = first_monotonicity_counterexample(op)
        lfp_k, chain = kleene_lfp(op)
        chain_txt = fmt_chain(chain, limit=6)
        return (f"{op} is not monotone: with S⊆T given by S={fmt_set(S)}, T={fmt_set(T)} "
                f"we have F(S)={fmt_set(apply_op(op, S))} ⊄ F(T)={fmt_set(apply_op(op, T))}. "
                f"The Kleene sequence from ∅ behaves as:\n"
                f"  {chain_txt}\n"
                f"(no guaranteed least fixed point).")

def print_reason() -> None:
    """Print the full explanations for each operator, in the same fixed order."""
    print("Reason why")
    print("==========")
    for op in ALL_OPS:
        print(f"- {reason_for(op)}")
    print()

# ==================
# Check (harness) ≥12
# ==================

class CheckFailure(AssertionError):
    """Raised when a check fails; forces a loud, visible failure."""
    pass

def check(cond: bool, msg: str) -> None:
    """Tiny helper for readable, fail-fast checks."""
    if not cond:
        raise CheckFailure(msg)

def run_checks() -> List[str]:
    """
    A battery of deterministic checks. These serve as executable documentation:
    if the model, operators, or reasoning are changed, a failing check will
    explain exactly what broke.
    """
    notes: List[str] = []

    # 1) succ relation is exactly the chain
    check(EXT2[succ] == {(0,1),(1,2),(2,3),(3,4)}, "succ must be 0→1→2→3→4.")
    notes.append("PASS 1: succ = {(0,1),(1,2),(2,3),(3,4)}.")

    # 2) powerset size sanity check
    check(len(ALL_SUBSETS) == 32, "P(D) must have 32 subsets for |D|=5.")
    notes.append("PASS 2: |P(D)| = 32.")

    # 3) intension vs extension: two names, one extension
    check(ExtEq1(N, omegaAlias) and N != omegaAlias, "N and omegaAlias must be ext-equal but different names.")
    notes.append("PASS 3: N ≡ omegaAlias (extension), but names differ (intensions).")

    # 4) monotone vs non-monotone classification correct
    for op in MONOTONE_OPS:
        check(is_monotone(op), f"{op} should be monotone.")
    for op in NON_MONOTONE_OPS:
        check(not is_monotone(op), f"{op} should be non-monotone.")
    notes.append("PASS 4: Monotonicity classification correct.")

    # 5) Kleene LFP equals Tarski LFP for every monotone operator
    for op in MONOTONE_OPS:
        lfp_k, chain = kleene_lfp(op)
        lfp_t = tarski_lfp(op)
        check(lfp_k is not None and lfp_k == lfp_t, f"{op}: Kleene and Tarski LFP must match.")
    notes.append("PASS 5: Kleene LFP = Tarski LFP for all monotone ops.")

    # 6–9) Expected concrete LFPs (hand-checked)
    check(tarski_lfp(op_closure_from_zero)  == set(D),          "F_closureFrom0 LFP should be D.")
    notes.append("PASS 6: LFP(F_closureFrom0) = D.")
    check(tarski_lfp(op_constant_prefix012) == {0,1,2},         "F_constPrefix012 LFP should be {0,1,2}.")
    notes.append("PASS 7: LFP(F_constPrefix012) = {0,1,2}.")
    check(tarski_lfp(op_evens_or_succ)      == set(D),          "F_evensOrSucc LFP should be D.")
    notes.append("PASS 8: LFP(F_evensOrSucc) = D.")
    check(tarski_lfp(op_from_top_pred)      == set(D),          "F_fromTopPred LFP should be D.")
    notes.append("PASS 9: LFP(F_fromTopPred) = D.")

    # 10) Kleene chain length bounded and sensible for closure-from-0
    lfp0, chain0 = kleene_lfp(op_closure_from_zero)
    check(len(chain0) <= 1 + len(ALL_SUBSETS), "Kleene chain should be finite.")
    check(chain0[1] == {0} and chain0[-1] == set(D), "F_closureFrom0 should grow from {0} to D.")
    notes.append("PASS 10: Kleene chain for F_closureFrom0 grows {0}→…→D.")

    # 11) Non-monotone: toggle has a counterexample and its Kleene chain oscillates
    witness = first_monotonicity_counterexample(op_toggle_complement)
    check(witness is not None, "toggle must have a monotonicity counterexample.")
    lfp_toggle, chain_toggle = kleene_lfp(op_toggle_complement, max_steps=16)
    check(lfp_toggle is None and chain_toggle[0] == set() and chain_toggle[1] == set(D),
          "toggle oscillates ∅↔D; no fixed point by Kleene iteration.")
    notes.append("PASS 11: Non-monotone toggle oscillates and has a counterexample.")

    # 12) Each monotone LFP is actually a fixed point (idempotence check)
    for op in MONOTONE_OPS:
        L = tarski_lfp(op)
        check(apply_op(op, L) == L, f"{op}: computed LFP is not a fixed point.")
    notes.append("PASS 12: Computed LFPs are fixed points.")

    # 13) For the constant operator, the fixed points set is exactly { {0,1,2} }
    fixeds = [S for S in ALL_SUBSETS if apply_op(op_constant_prefix012, S) == S]
    check(len(fixeds) == 1 and fixeds[0] == {0,1,2}, "Constant operator should have exactly one fixed point: {0,1,2}.")
    notes.append("PASS 13: Constant operator has unique fixed point {0,1,2}.")

    return notes

# ===============
# Main orchestration
# ===============

def main() -> None:
    print_model()
    print_question()

    answers = compute_answers()
    print_answer(answers)
    print_reason()

    print("Check (harness)")
    print("===============")
    try:
        notes = run_checks()
    except CheckFailure as e:
        print("FAIL:", e)
        raise
    else:
        for line in notes:
            print(line)

if __name__ == "__main__":
    main()

