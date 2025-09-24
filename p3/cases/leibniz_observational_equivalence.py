#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Observational Equivalence (Leibniz-flavored) — N3 in • N3 out • EYE-backed

INTENT
------
This is a small, reproducible case in the P3 style (Answer • Reason • Check)
that formalizes a Leibniz-like notion of *observational equivalence* (obsEq) WITHOUT
using full identity (owl:sameAs). Two entities are obsEq if they match on a chosen,
finite set of observables. We then allow *guarded substitution* only for predicates
marked as substitutable (e.g., :inCluster, :hasType). Administrative identifiers and
unrelated relations do not propagate.

PIPELINE (what the script does)
-------------------------------
1) Writes three N3 artifacts to a temporary directory:
     • LOGIC — rules for obsEq and guarded substitution.
     • DATA  — small world with three stars and a substitution policy.
     • GOAL  — FILTER RULES expressing the query patterns we want back.
2) Runs EYE once: eye --quiet --nope data.n3 logic.n3 --query goal.n3
3) Parses EYE’s N3 result into triples.
4) Emitts a single N3 document to stdout with only TRIPLES:
     :Answer :is { … } .
     :Reason :is "…" .
     :Check  :is { … } .
"""

import os, re, subprocess, sys, tempfile

# ──────────────────────────────────────────────────────────────────────────────
# LOGIC — obsEq (no sameAs) + reflexive/symmetric/transitive + guarded substitution
# Notes:
# • obsEq is derived from equality on (:colorIndex, :spectrumLine, :apparentMag).
# • We add explicit *reflexivity* for any entity that has at least one observable.
#   (EYE will not auto-generate x :obsEq x; making it explicit keeps teaching crisp.)
# • Guarded substitution fires ONLY for predicates with :substitutable true.
#   We include both subject- and object-position rules.
# ──────────────────────────────────────────────────────────────────────────────
LOGIC_N3 = """@prefix : <http://example.org/astro#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Derive observational equivalence from the finite signature
{ ?x :colorIndex ?ci . ?y :colorIndex ?ci .
  ?x :spectrumLine ?sl . ?y :spectrumLine ?sl .
  ?x :apparentMag ?m .  ?y :apparentMag ?m .
} => { ?x :obsEq ?y } .

# Make reflexive obsEq for anyone who has at least one observable
{ ?x :colorIndex ?_ }   => { ?x :obsEq ?x } .
{ ?x :spectrumLine ?_ } => { ?x :obsEq ?x } .
{ ?x :apparentMag ?_ }  => { ?x :obsEq ?x } .

# obsEq behaves like an equivalence relation (here: symmetric + transitive)
{ ?x :obsEq ?y } => { ?y :obsEq ?x } .
{ ?x :obsEq ?y . ?y :obsEq ?z } => { ?x :obsEq ?z } .

# Guarded substitution — subject position
# If P is substitutable, and x obsEq y, and x P O, then y P O.
{ ?p :substitutable true .
  ?x :obsEq ?y .
  ?x ?p ?o
} => { ?y ?p ?o } .

# Guarded substitution — object position
# If P is substitutable, and S P x, then S P y when x obsEq y.
{ ?p :substitutable true .
  ?x :obsEq ?y .
  ?s ?p ?x
} => { ?s ?p ?y } .
"""

# ──────────────────────────────────────────────────────────────────────────────
# DATA — two equivalent stars (s1,s2) + one different (s3); substitution policy
# Notes:
# • :inCluster and :hasType are allowed to propagate across obsEq.
# • :catalogId and :contains are deliberately *not* substitutable.
# ──────────────────────────────────────────────────────────────────────────────
DATA_N3 = """@prefix : <http://example.org/astro#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Substitution policy (what is allowed to propagate)
:inCluster :substitutable true .
:hasType   :substitutable true .
:catalogId :substitutable false .
:contains  :substitutable false .

# Three "stars" and their observables
:s1 :colorIndex "0.65"^^xsd:decimal ; :spectrumLine :Halpha ; :apparentMag "5.1"^^xsd:decimal .
:s2 :colorIndex "0.65"^^xsd:decimal ; :spectrumLine :Halpha ; :apparentMag "5.1"^^xsd:decimal .
:s3 :colorIndex "0.40"^^xsd:decimal ; :spectrumLine :Halpha ; :apparentMag "5.1"^^xsd:decimal .

# Facts about s1 that SHOULD propagate (because substitutable)
:s1 :inCluster :M67 .
:s1 :hasType   :G .

# Facts that must NOT propagate (by policy)
:s1 :catalogId "TYC 1234-567-1" .
:M67 :contains :s1 .
"""

# ──────────────────────────────────────────────────────────────────────────────
# GOAL — FILTER RULES describing which patterns to return
# Why filter rules?
#   EYE accepts goals as rules that copy matched triples to the query output.
#   Using a separate rule per pattern is portable and clear.
# ──────────────────────────────────────────────────────────────────────────────
GOAL_N3 = """@prefix : <http://example.org/astro#> .
{ ?x :obsEq ?y . }         => { ?x :obsEq ?y . } .
{ :s2 :inCluster ?c . }    => { :s2 :inCluster ?c . } .
{ :s2 :hasType ?t . }      => { :s2 :hasType ?t . } .
{ :s2 :catalogId ?id . }   => { :s2 :catalogId ?id . } .      # expected to return nothing
{ :M67 :contains :s2 . }   => { :M67 :contains :s2 . } .      # expected to return nothing
"""

# ──────────────────────────────────────────────────────────────────────────────
# EYE invocation and N3 parsing
# ──────────────────────────────────────────────────────────────────────────────
TRIPLE_RE = re.compile(r'^\s*([^ ]+)\s+([^ ]+)\s+(.+?)\s*\.\s*$')

def run_eye_and_parse():
    """
    Writes DATA/LOGIC/GOAL to a temp dir, runs EYE once, and parses the N3 result
    into a list of (s, p, o) triples (strings as printed by EYE).
    """
    with tempfile.TemporaryDirectory() as td:
        dp, lp, gp = f"{td}/data.n3", f"{td}/logic.n3", f"{td}/goal.n3"
        open(dp, "w", encoding="utf-8").write(DATA_N3)
        open(lp, "w", encoding="utf-8").write(LOGIC_N3)
        open(gp, "w", encoding="utf-8").write(GOAL_N3)

        # --nope skips proof printing; we assemble our own Reason/Check narrative.
        out = subprocess.check_output(
            ["eye", "--quiet", "--nope", dp, lp, "--query", gp],
            text=True
        ).strip()

    triples = []
    for line in out.splitlines():
        m = TRIPLE_RE.match(line)
        if m:
            triples.append((m.group(1), m.group(2), m.group(3)))
    return triples

# ──────────────────────────────────────────────────────────────────────────────
# Build the three N3 sections (as triples-only)
#   :Answer :is { … } .
#   :Reason :is "…" .
#   :Check  :is { … } .
# ──────────────────────────────────────────────────────────────────────────────
def build_answer_graph_n3(triples):
    """
    Extract exactly what we want to present as “Answer”:
      • All returned :obsEq pairs (including reflexive entries).
      • Propagated substitutable facts on :s2 (:inCluster, :hasType).
      • If anything forbidden leaked (e.g., :catalogId on :s2), surface it as a comment.
    """
    obs = [(s, o) for (s, p, o) in triples if p == ":obsEq"]
    s2_clusters = [o for (s, p, o) in triples if s == ":s2" and p == ":inCluster"]
    s2_types    = [o for (s, p, o) in triples if s == ":s2" and p == ":hasType"]
    s2_ids      = [o for (s, p, o) in triples if s == ":s2" and p == ":catalogId"]
    m67_contains_s2 = any(s == ":M67" and p == ":contains" and o == ":s2" for (s, p, o) in triples)

    # Serialize solely as triples, wrapped under :Answer :is { … } .
    lines = []
    lines.append("# Answer — derived obsEq pairs and propagated facts of interest")
    lines.append(":Answer :is {")
    for a, b in sorted(set(obs)):
        lines.append(f"  {a} :obsEq {b} .")
    for c in sorted(set(s2_clusters)):
        lines.append(f"  :s2 :inCluster {c} .")
    for t in sorted(set(s2_types)):
        lines.append(f"  :s2 :hasType {t} .")
    # If any forbidden propagation happened, we still show it (as a commented warning)
    for idv in sorted(set(s2_ids)):
        lines.append(f"  # WARNING unexpected: :s2 :catalogId {idv} .")
    if m67_contains_s2:
        lines.append(f"  # WARNING unexpected: :M67 :contains :s2 .")
    lines.append("} .")

    # Keep a structured “derived” bundle for the Check phase
    derived = {
        "obs_pairs": set(obs),
        "s2_clusters": set(s2_clusters),
        "s2_types": set(s2_types),
        "s2_ids": s2_ids,
        "m67_contains_s2": m67_contains_s2
    }
    return "\n".join(lines), derived

def build_reason_n3():
    """
    Human-readable Reason, still emitted as a triple via :Reason :is "…".
    """
    txt = (
        "We treat :obsEq as an observational equivalence built from the finite signature "
        "(:colorIndex, :spectrumLine, :apparentMag). It is symmetric and transitive, and we "
        "also assert reflexive pairs for any entity that bears at least one observable. Only "
        "predicates declared :substitutable true (here :inCluster, :hasType) are allowed to "
        "substitute across :obsEq; administrative IDs and unrelated relations do not propagate."
    )
    return f':Reason :is "{txt}" .'

def build_check_graph_n3(derived):
    """
    Five concise tests, each serialized as a :Test node with :name and :ok.
    We also include a tiny summary with counts.
    """
    tests = []
    def add(name, ok, details=None):
        tests.append((name, ok, details))

    pairs = derived["obs_pairs"]

    # 1) s1 and s2 must be observationally equivalent (either direction is fine)
    add("obsEq_s1_s2", ((":s1", ":s2") in pairs) or ((":s2", ":s1") in pairs),
        None if (((":s1", ":s2") in pairs) or ((":s2", ":s1") in pairs))
        else "Expected :s1 :obsEq :s2 (or symmetric).")

    # 2) No *cross* obsEq involving :s3 (reflexive :s3 :obsEq :s3 is fine)
    def cross_s3(a, b): return (a == ":s3" and b != ":s3") or (b == ":s3" and a != ":s3")
    no_cross_s3 = not any(cross_s3(a, b) for (a, b) in pairs)
    add("no_cross_obsEq_with_s3", no_cross_s3,
        None if no_cross_s3 else "Unexpected cross :obsEq involving :s3 detected.")

    # 3) Guarded substitution (subject pos): s2 gets :inCluster from s1
    add("subject_guarded_inCluster", len(derived["s2_clusters"]) >= 1,
        None if len(derived["s2_clusters"]) >= 1 else "Expected :s2 :inCluster ?c via substitution.")

    # 4) Guarded substitution (subject pos): s2 gets :hasType from s1
    add("subject_guarded_hasType", len(derived["s2_types"]) >= 1,
        None if len(derived["s2_types"]) >= 1 else "Expected :s2 :hasType ?t via substitution.")

    # 5) Non-substitutable facts did NOT propagate
    non_leak = (len(derived["s2_ids"]) == 0) and (derived["m67_contains_s2"] is False)
    add("non_substitutable_blocked", non_leak,
        None if non_leak else "Unexpected propagation of :catalogId or :contains detected.")

    # Serialize the tests as triples under :Check :is { … } .
    lines = []
    lines.append("# Check — five tests as :Test nodes")
    lines.append(":Check :is {")
    passed = 0
    for i, (name, ok, details) in enumerate(tests, start=1):
        tid = f":test{i}"
        # Keep each :Test as simple triples; no blank nodes → easier to grep/pipe
        lines.append(f"  {tid} a :Test .")
        lines.append(f"  {tid} :name \"{name}\" .")
        lines.append(f"  {tid} :ok {str(ok).lower()} .")
        if ok: passed += 1
    # Small rollup summary (also triples)
    lines.append(f"  :summary :passed \"{passed}\" .")
    lines.append(f"  :summary :total \"{len(tests)}\" .")
    lines.append("} .")

    return "\n".join(lines), (passed == len(tests))

# ──────────────────────────────────────────────────────────────────────────────
# MAIN — run EYE, build the three sections, and print a single N3 document
# ──────────────────────────────────────────────────────────────────────────────
def main():
    triples = run_eye_and_parse()
    answer_n3, derived = build_answer_graph_n3(triples)
    reason_n3 = build_reason_n3()
    check_n3, ok = build_check_graph_n3(derived)

    # Minimal header; prefixes only (triples that follow define the triad)
    header = """@prefix : <http://example.org/astro#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

"""
    print(header, end="")
    print(answer_n3)
    print()
    print(reason_n3)
    print()
    print(check_n3)

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())

