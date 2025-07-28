# Backward‑chaining proof for the “ageAbove” rule example
# -------------------------------------------------------
#
# N3 rule (tutorial style):
#
# { ?S :ageAbove ?A } <= {
#     ?S :birthDay ?B .
#     "" time:localTime ?D .
#     (?D ?B) math:difference ?F .
#     ?F math:greaterThan ?A .
# } .
#
# Query:
#     ?S :ageAbove  "P80Y"^^xsd:duration .
#
# The engine below:
#   • supports three built‑ins: time:localTime, math:difference, math:greaterThan
#   • treats ISO‑date strings and ISO‑8601 duration "PnY" (years only)
#   • prints an indented, numbered proof trace

from datetime import date, datetime
from itertools import count
import re

# ──────────────────────────────────────────────────────────────
# Ground facts
# ──────────────────────────────────────────────────────────────
facts = {
    (":patH", ":birthDay", "1944-08-21"),            # ISO‑date string
}

# ──────────────────────────────────────────────────────────────
# Rule
# ──────────────────────────────────────────────────────────────
rule_id   = "R-ageAbove"
rule_head = ("?S", ":ageAbove", "?A")
rule_body = [
    ("?S", ":birthDay", "?B"),
    ("",   "time:localTime", "?D"),
    (("?D","?B"), "math:difference", "?F"),          # subject is a tuple (D,B)
    ("?F", "math:greaterThan", "?A"),
]

# ──────────────────────────────────────────────────────────────
# Utility: parse dates & durations
# ──────────────────────────────────────────────────────────────
def parse_iso_date(s):
    return datetime.strptime(s, "%Y-%m-%d").date()

_duration_pat = re.compile(r"P(\d+)Y$")
def duration_to_years(dur):
    """Take 'P80Y' → 80.0"""
    m = _duration_pat.fullmatch(dur)
    if not m:
        raise ValueError(f"Unsupported duration literal {dur}")
    return float(m.group(1))

# ──────────────────────────────────────────────────────────────
# Unification helpers
# ──────────────────────────────────────────────────────────────
is_var = lambda t: isinstance(t, str) and t.startswith("?")

def unify(pattern, datum, θ):
    θ = dict(θ)
    for p, d in zip(pattern, datum):
        if is_var(p):
            if p in θ and θ[p] != d:
                return None
            θ[p] = d
        elif p != d:
            return None
    return θ

def subst(term, θ):
    if isinstance(term, tuple):
        return tuple(subst(t, θ) for t in term)
    return θ.get(term, term)

def subst_triple(trp, θ):
    return tuple(subst(t, θ) for t in trp)

# ──────────────────────────────────────────────────────────────
# Pretty‑print helpers
# ──────────────────────────────────────────────────────────────
counter = count(1)
indent  = lambda d: "  " * d

# ──────────────────────────────────────────────────────────────
# Built‑ins
# ──────────────────────────────────────────────────────────────
def builtin_time_localTime(s, o):
    if s != "":
        return None
    today = date(2025, 6, 30)            # fixed “today” per instructions
    return str(today) if is_var(o) or o == str(today) else None

def builtin_math_difference(pair, o):
    if not isinstance(pair, tuple) or len(pair) != 2:
        return None
    d_str, b_str = pair
    d_val = parse_iso_date(d_str)
    b_val = parse_iso_date(b_str)
    years = (d_val - b_val).days / 365.2425
    result = years
    return result if is_var(o) or o == result else None

def builtin_math_greaterThan(s, o):
    # s expected numeric, o duration or numeric
    if isinstance(o, str):
        o_val = duration_to_years(o)
    else:
        o_val = o
    return True if isinstance(s,(int,float)) and s > o_val else None

# ──────────────────────────────────────────────────────────────
# Backward‑chaining prover
# ──────────────────────────────────────────────────────────────
def bc_prove(goal, θ, depth, seen):
    g = subst_triple(goal, θ)
    tag = next(counter)
    print(f"{indent(depth)}Step {tag:02}: prove {g}")

    subj, pred, obj = g

    # Built‑ins first
    if pred == "time:localTime":
        res = builtin_time_localTime(subj, obj)
        if res is not None:
            θ2 = dict(θ)
            if is_var(obj):
                θ2[obj] = res
            print(f"{indent(depth)}  ✓ built‑in time:localTime → {res}")
            yield θ2
        return

    if pred == "math:difference":
        res = builtin_math_difference(subj, obj)
        if res is not None:
            θ2 = dict(θ)
            if is_var(obj):
                θ2[obj] = res
            print(f"{indent(depth)}  ✓ built‑in math:difference → {res:.2f}y")
            yield θ2
        return

    if pred == "math:greaterThan":
        if builtin_math_greaterThan(subj, obj):
            print(f"{indent(depth)}  ✓ built‑in {subj} > {obj}")
            yield θ
        else:
            print(f"{indent(depth)}  ✗ built‑in fails ({subj} ≤ {obj})")
        return

    # Check ground facts
    for fact in facts:
        θ2 = unify(g, fact, {})
        if θ2 is not None:
            print(f"{indent(depth)}  ✓ fact {fact}")
            yield {**θ, **θ2}
            return

    # Apply the single rule if head matches
    θ_head = unify(rule_head, g, {})
    if θ_head is None:
        return
    head_inst = subst_triple(rule_head, θ_head)
    if head_inst in seen:
        return
    print(f"{indent(depth)}  → via {rule_id}")
    θ_curr = {**θ, **θ_head}

    def prove_body(i, θ_now):
        if i == len(rule_body):
            yield θ_now
        else:
            for θ_next in bc_prove(rule_body[i], θ_now, depth+1, seen|{head_inst}):
                yield from prove_body(i+1, θ_next)

    yield from prove_body(0, θ_curr)

# ──────────────────────────────────────────────────────────────
# Query
# ──────────────────────────────────────────────────────────────
def ask(query):
    print(f"\n=== Query {query} ===")
    for θ in bc_prove(query, {}, 0, frozenset()):
        print(f"✔ PROVED {subst_triple(query, θ)}\n")
        return
    print("✗ NOT PROVED\n")

ask( ("?S", ":ageAbove", "P80Y") )

