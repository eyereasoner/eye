# Defeasible Logic Mini-Demo
# Scenario:
#   Strict: penguin(X) -> bird(X)
#   Defeasible: bird(X) => flies(X)            [priority 1]
#   Defeasible: penguin(X) => not flies(X)     [priority 2]  (more specific/stronger)
# Facts: penguin(Tweety), bird(Robin)
#
# Goal: Conclude whether Tweety flies (answer should be "not flies(Tweety)"),
#       explain "why", and run a small test harness.

from dataclasses import dataclass
from typing import List, Dict, Tuple, Optional, Set

# ---------------------------
# Core data structures
# ---------------------------

@dataclass(frozen=True)
class Rule:
    name: str
    antecedents: List[str]   # e.g., ["bird(X)"]
    consequent: str          # e.g., "flies(X)" or "not flies(X)"
    kind: str                # "strict" or "defeasible"
    priority: int = 0        # only used for defeasible rules; higher = stronger

@dataclass
class Support:
    rule_name: str
    kind: str                # "strict" or "defeasible" or "fact"
    strength: int            # 1000 for strict/facts; else rule priority
    antecedents: List[str]   # the grounded antecedents used
    note: str = ""           # free text used in explanations

# ---------------------------
# Helpers for literals
# ---------------------------

def is_neg(lit: str) -> bool:
    return lit.startswith("not ")

def complement(lit: str) -> str:
    return lit[4:] if is_neg(lit) else "not " + lit

def pred_and_const(lit: str) -> Tuple[str, str]:
    # Works for unary predicates like "bird(Tweety)" or "not flies(Tweety)"
    base = lit[4:] if is_neg(lit) else lit
    pred = base.split("(")[0].strip()
    const = base[base.find("(") + 1 : base.rfind(")")].strip()
    return pred, const

def substitute_X(expr: str, const: str) -> str:
    return expr.replace("(X)", f"({const})")

def constants_in_facts(facts: Set[str]) -> Set[str]:
    consts = set()
    for f in facts:
        _, c = pred_and_const(f)
        consts.add(c)
    return consts

# ---------------------------
# Reasoning Engine
# ---------------------------

STRICT_STRENGTH = 1000  # outranks any defeasible priority

def strict_closure(facts: Set[str], strict_rules: List[Rule]) -> Tuple[Set[str], Dict[str, Support]]:
    derived = set(facts)
    proof: Dict[str, Support] = {}
    for f in facts:
        proof[f] = Support(rule_name="(given)", kind="fact", strength=STRICT_STRENGTH, antecedents=[], note="Given as a fact.")

    changed = True
    while changed:
        changed = False
        consts = constants_in_facts(derived)
        for r in strict_rules:
            for c in consts:
                if all(substitute_X(a, c) in derived for a in r.antecedents):
                    cons = substitute_X(r.consequent, c)
                    if cons not in derived:
                        derived.add(cons)
                        proof[cons] = Support(
                            rule_name=r.name,
                            kind="strict",
                            strength=STRICT_STRENGTH,
                            antecedents=[substitute_X(a, c) for a in r.antecedents],
                            note=f"Applied strict rule {r.name}."
                        )
                        changed = True
    return derived, proof

def defeasible_infer(known: Set[str],
                     base_proof: Dict[str, Support],
                     defeasible_rules: List[Rule]) -> Tuple[Dict[str, Support], Dict[str, List[Support]]]:
    """
    Returns:
      winners: best accepted support per literal (includes strict supports passed in)
      candidates: all defeasible supports that were considered (per literal)
    """
    winners: Dict[str, Support] = dict(base_proof)  # start from strict/facts
    candidates: Dict[str, List[Support]] = {}

    # We'll iterate a few times to allow chaining of defeasible conclusions if needed
    # (not strictly necessary for this example, but keeps it general).
    for _ in range(5):
        progressed = False
        consts = constants_in_facts(set(winners.keys()) | known)
        # Consider rules in descending priority so stronger rules get first shot.
        for r in sorted(defeasible_rules, key=lambda x: x.priority, reverse=True):
            for c in consts:
                ants = [substitute_X(a, c) for a in r.antecedents]
                if not all(a in winners for a in ants):
                    continue  # only fire when all antecedents are supported already
                cons = substitute_X(r.consequent, c)
                sup = Support(
                    rule_name=r.name,
                    kind="defeasible",
                    strength=r.priority,
                    antecedents=ants,
                    note=f"Applied defeasible rule {r.name} (priority {r.priority})."
                )
                candidates.setdefault(cons, []).append(sup)

                # Compare against any existing accepted support for cons or its complement
                current = winners.get(cons, None)
                opposing = winners.get(complement(cons), None)

                # If there's an opposing with >= strength, we can't accept this support.
                opp_strength = opposing.strength if opposing else -1
                if sup.strength <= opp_strength:
                    continue

                # If current is weaker than this support, or absent, accept it.
                curr_strength = current.strength if current else -1
                if sup.strength > curr_strength:
                    winners[cons] = sup
                    progressed = True
        if not progressed:
            break
    return winners, candidates

# ---------------------------
# Knowledge Base (Example)
# ---------------------------

strict_rules = [
    Rule(name="R1", antecedents=["penguin(X)"], consequent="bird(X)", kind="strict"),
]

defeasible_rules = [
    Rule(name="R2", antecedents=["bird(X)"],    consequent="flies(X)",     kind="defeasible", priority=1),
    Rule(name="R3", antecedents=["penguin(X)"], consequent="not flies(X)", kind="defeasible", priority=2),
    Rule(name="R4", antecedents=["bird(X)"],    consequent="lays_eggs(X)", kind="defeasible", priority=1),
]

facts = {
    "penguin(Tweety)",  # implies bird(Tweety) via strict rule
    "bird(Robin)",      # a normal bird
}

# ---------------------------
# Run reasoning
# ---------------------------

strict_derived, strict_proof = strict_closure(facts, strict_rules)
all_known = set(strict_derived)  # set of literals accepted on strict grounds
winners, considered = defeasible_infer(all_known, strict_proof, defeasible_rules)

# Convenience access
def accepted(lit: str) -> bool:
    return lit in winners and (
        winners[lit].kind in ("strict", "fact") or winners[lit].kind == "defeasible"
    )

# ---------------------------
# Build "Answer" + "Reason why"
# ---------------------------

query = "flies(Tweety)"
q_comp = complement(query)

def explain(query_lit: str) -> str:
    lines = []
    pred, const = pred_and_const(query_lit)

    # Facts and strict derivations used
    lines.append("Facts & strict derivations:")
    # Show only the relevant chain for Tweety/Robin
    relevant = [k for k in winners if pred_and_const(k)[1] == pred_and_const(query_lit)[1]]
    for lit in sorted(set(relevant)):
        sup = winners[lit]
        if sup.kind in ("fact", "strict"):
            if sup.kind == "fact":
                lines.append(f"  • {lit}  — Given as a fact.")
            else:
                ants = ", ".join(sup.antecedents) if sup.antecedents else "—"
                lines.append(f"  • {lit}  — From {sup.rule_name} using [{ants}].")

    # Competing defeasible supports
    lines.append("\nCompeting defeasible arguments:")
    for side in [query_lit, complement(query_lit)]:
        cands = considered.get(side, [])
        if not cands:
            lines.append(f"  • {side}: (no defeasible rule applicable)")
        else:
            for s in sorted(cands, key=lambda x: x.strength, reverse=True):
                ants = ", ".join(s.antecedents)
                lines.append(f"  • {side}: {s.rule_name} (priority {s.strength}) from [{ants}]")

    # Outcome
    if accepted(query_lit) and not accepted(complement(query_lit)):
        lines.append(f"\nResolution: ACCEPT {query_lit} — strongest applicable rule supports it; the contrary is weaker or absent.")
    elif accepted(complement(query_lit)) and not accepted(query_lit):
        lines.append(f"\nResolution: REJECT {query_lit} — {complement(query_lit)} is supported by a stronger, more specific rule.")
    elif accepted(query_lit) and accepted(complement(query_lit)):
        lines.append("\nResolution: CONFLICT — both sides accepted (tie). (This engine blocks only on ≥ strength; equal strengths may induce ambiguity.)")
    else:
        lines.append("\nResolution: UNDETERMINED — no applicable support.")
    return "\n".join(lines)

# ---------------------------
# Output
# ---------------------------

print("Answer")
print("------")
if accepted(query):
    print(f"Conclusion: {query}")
elif accepted(q_comp):
    print(f"Conclusion: {q_comp}")
else:
    print("Conclusion: Undetermined")

print("\nReason why")
print("----------")
print(explain(query))

# ---------------------------
# Check (harness)
# ---------------------------

tests = [
    ("not flies(Tweety)", True,  "Specific penguin rule defeats general bird rule"),
    ("flies(Robin)",      True,  "Ordinary bird should fly"),
    ("lays_eggs(Tweety)", True,  "Birds typically lay eggs; Tweety is (strictly) a bird"),
    ("flies(Tweety)",     False, "Tweety should not fly due to stronger contrary rule"),
]

print("\nCheck (harness)")
print("----------------")
passed = 0
for lit, expected, msg in tests:
    got = accepted(lit)
    ok = (got == expected)
    passed += ok
    status = "PASS" if ok else "FAIL"
    print(f"[{status}] {lit:20s} expected={expected!r} got={got!r} — {msg}")

print(f"\n{passed}/{len(tests)} tests passed.")

# EOF

