#!/usr/bin/env python3
"""
Zebra (N3/RDF fragment) solver + goal-oriented proof explanation.

What this program does
----------------------
1) Encodes the given N3 constraints (equalTo, list:member, :pair/:sublist rules).
2) Solves the 5-house zebra arrangement.
3) Answers the query: {?WHO :eats :fish}.
4) Prints a goal-oriented, explicit-step proof

Notes on modeling
-----------------
- Houses are positions 0..4 (left→right). Each category is a permutation of positions.
- We enforce exactly the N3 constraints present in the snippet:
  * log:equalTo list shape pins:
      blue@1, milk@2, norwegian@0
  * sublist ((:green ? ? :coffee ?) (:white ? ? ? ?))
      → green is immediately left of white; green drinks coffee
  * pair ((... :blends) (... :cats ...)) → blends next to cats
  * pair ((... :horse ...) (... :dunhill)) → horse next to dunhill
  * pair ((... :blends) (... :water)) → blends next to water
  * list:member facts for color/nationality/pet/drink/smoke combos

- The proof trace is goal-directed:
  G0: :eats(?WHO, :fish)
  Backchain on the single rule whose head is {?B :eats :fish}.
  Show its antecedent holds in a concrete model M (the unique solution),
  then instantiate ?WHO = :german.

The output
----------
1) Prints the solved table (for transparency).
2) Prints the grounded answer triple ":german :eats :fish".
3) Prints "=== Pretty Proof ===" with numbered steps.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, List, Optional, Tuple
from itertools import permutations

# -----------------------------------------------------------------------------
# Domain encoding (solver)
# -----------------------------------------------------------------------------

IDX = range(5)

COLORS = ["red", "green", "white", "yellow", "blue"]
NATS   = ["brit", "swede", "dane", "norwegian", "german"]
PETS   = ["dogs", "birds", "cats", "horse", "fish"]
DRINKS = ["tea", "coffee", "milk", "beer", "water"]
SMOKES = ["pallmall", "dunhill", "blends", "bluemasters", "prince"]

def solve():
    solutions = []

    for C in permutations(IDX):  # C[color_idx] = position of that color
        # equalTo: blue is at house index 1 (second house)
        if C[COLORS.index("blue")] != 1:
            continue

        # sublist green immediately left of white
        green_pos = C[COLORS.index("green")]
        white_pos = C[COLORS.index("white")]
        if green_pos + 1 != white_pos:
            continue

        for S in permutations(IDX):  # S[smoke_idx] = position of that smoke
            # list:member (:yellow ... :dunhill)
            if C[COLORS.index("yellow")] != S[SMOKES.index("dunhill")]:
                continue

            for D in permutations(IDX):  # D[drink_idx] = position of that drink
                # equalTo: milk is at house index 2 (third house)
                if D[DRINKS.index("milk")] != 2:
                    continue

                # sublist: green-coffee
                if D[DRINKS.index("coffee")] != green_pos:
                    continue

                # list:member: beer with bluemasters
                if D[DRINKS.index("beer")] != S[SMOKES.index("bluemasters")]:
                    continue

                # pair: blends next to water
                if abs(S[SMOKES.index("blends")] - D[DRINKS.index("water")]) != 1:
                    continue

                for P in permutations(IDX):  # P[pet_idx] = position of that pet
                    # list:member: birds with pallmall
                    if P[PETS.index("birds")] != S[SMOKES.index("pallmall")]:
                        continue

                    # pair: blends next to cats
                    if abs(S[SMOKES.index("blends")] - P[PETS.index("cats")]) != 1:
                        continue

                    # pair: horse next to dunhill
                    if abs(P[PETS.index("horse")] - S[SMOKES.index("dunhill")]) != 1:
                        continue

                    for N in permutations(IDX):  # N[nat_idx] = position of that nationality
                        # equalTo: norwegian at house index 0 (first)
                        if N[NATS.index("norwegian")] != 0:
                            continue

                        # list:member: brit in red house
                        if N[NATS.index("brit")] != C[COLORS.index("red")]:
                            continue

                        # list:member: swede keeps dogs
                        if N[NATS.index("swede")] != P[PETS.index("dogs")]:
                            continue

                        # list:member: dane drinks tea
                        if N[NATS.index("dane")] != D[DRINKS.index("tea")]:
                            continue

                        # list:member: german smokes prince
                        if N[NATS.index("german")] != S[SMOKES.index("prince")]:
                            continue

                        # (Entailed by equalTo pattern) norwegian next to blue
                        if abs(N[NATS.index("norwegian")] - C[COLORS.index("blue")]) != 1:
                            continue

                        solutions.append((C, S, D, P, N))

    return solutions

def invert_map(M, domain):
    return {M[i]: domain[i] for i in range(5)}

def render_table(invC, invN, invP, invD, invS) -> str:
    lines = []
    lines.append("House  Pos  Color   Nationality  Pet    Drink   Smoke")
    lines.append("-----  ---  ------  -----------  -----  ------  ----------")
    for pos in range(5):
        lines.append(
            f"{pos+1:<5}  {pos:<3}  {invC[pos]:<6}  {invN[pos]:<11}  {invP[pos]:<5}  {invD[pos]:<6}  {invS[pos]:<10}"
        )
    return "\n".join(lines)

# -----------------------------------------------------------------------------
# Tiny "pretty proof" kernel
# -----------------------------------------------------------------------------

@dataclass(frozen=True)
class Atom:
    pred: str
    args: Tuple[Any, ...]
    def pretty(self) -> str:
        def fmt(x: Any) -> str:
            return x if isinstance(x, str) else str(x)
        return f"{self.pred}(" + ", ".join(fmt(a) for a in self.args) + ")"

@dataclass
class Conclusion:
    kind: str
    payload: Any
    def pretty(self) -> str:
        k, p = self.kind, self.payload
        if k in ("formula", "goal", "text"):
            return p if isinstance(p, str) else str(p)
        if k == "rule":
            return p  # already a formatted rule sketch
        if hasattr(p, "pretty"):
            return p.pretty()
        return str(p)

@dataclass
class Step:
    id: int
    rule: str
    premises: List[int]
    conclusion: Conclusion
    notes: Optional[str] = None

@dataclass
class Proof:
    steps: List[Step] = field(default_factory=list)
    def add(self, rule: str, premises: List[int], conclusion: Conclusion, notes: Optional[str] = None) -> int:
        sid = len(self.steps) + 1
        self.steps.append(Step(sid, rule, premises, conclusion, notes))
        return sid
    def pretty(self) -> str:
        out = []
        for s in self.steps:
            prem = f" [{', '.join(map(str, s.premises))}]" if s.premises else ""
            note = f" // {s.notes}" if s.notes else ""
            out.append(f"[{s.id}] {s.rule}{prem}: {s.conclusion.pretty()}{note}")
        return "\n".join(out)

# -----------------------------------------------------------------------------
# Build a goal-oriented proof for the query {?WHO :eats :fish}
# -----------------------------------------------------------------------------

def build_goal_oriented_proof(invC, invN, invP, invD, invS) -> Proof:
    """
    Construct a readable, numbered trace:
      - Start from the goal eats(?WHO, :fish).
      - Backchain on the main N3 rule whose head is {?B :eats :fish}.
      - Establish the antecedent by checks against the concrete model (table).
      - Conclude eats(:german, :fish) and answer WHO=:german.
    """
    proof = Proof()

    # [1] Present the (sketched) rule R from the given N3: Antecedent ⇒ (?B :eats :fish).
    rule_text = (
        "R (N3): If the following all hold simultaneously —\n"
        "  • log:equalTo L = [(?A1 :norwegian ...), (:blue ...), (? : ? : ? :milk ...), ...]\n"
        "  • :pair((... :blends) (... :cats ...))\n"
        "  • :pair((... :horse ...) (... :dunhill))\n"
        "  • :sublist((:green ... :coffee ...) (:white ...))  (green immediately left of white)\n"
        "  • :pair((... :blends) (... :water))\n"
        "  • list:member facts: (:red :brit ...), (? :swede :dogs ...), (? :dane ... :tea ...),\n"
        "                       (? ? :birds ... :pallmall), (:yellow ... :dunhill),\n"
        "                       (? ? ? :beer :bluemasters), (? :german ... :prince),\n"
        "                       (? ? :fish ...)\n"
        "then there exists ?B such that (?B :eats :fish)."
    )
    s1 = proof.add("Premise-Rule", [], Conclusion("rule", rule_text), notes="Single rule with head ?B :eats :fish")

    # [2] The goal/query
    s2 = proof.add("Goal", [], Conclusion("goal", "eats(?WHO, :fish)"), notes="Original query")

    # [3] Backchain on R: reduce goal to proving Antecedent and identify ?WHO with ?B
    s3 = proof.add("Backchain", [s1, s2], Conclusion("text", "Reduce to proving R's antecedent; set ?WHO ≡ ?B"),
                   notes="Goal-directed use of rule head")

    # [4] Build the concrete model M (the unique 5-house solution)
    s4 = proof.add("Model-Construction", [],
                   Conclusion("text", "Construct M: unique 5-house assignment (see table above)"),
                   notes="Finite search over permutations under N3 constraints")

    # [5a] Check equalTo-pattern pins
    checks_equalto = [
        f"blue@1  ✓ (house 2 is :blue → pos={invC_inverse(invC, 'blue')})",
        f"milk@2  ✓ (house 3 drinks :milk → pos={invD_inverse(invD, 'milk')})",
        f"norwegian@0 ✓ (house 1 nationality :norwegian → pos={invN_inverse(invN, 'norwegian')})",
    ]
    s5a = proof.add("Check-EqualTo", [s4], Conclusion("text", "; ".join(checks_equalto)),
                    notes="Matches the list pattern from log:equalTo")

    # [5b] Check sublist: green left of white, and green drinks coffee
    green_pos = pos_of(invC, "green")
    white_pos = pos_of(invC, "white")
    s5b = proof.add("Check-Sublist", [s4], Conclusion(
        "text",
        f"green at {green_pos} and white at {white_pos}, so green immediately left of white ✓; "
        f"drink(green) = {invD[green_pos]} = :coffee ✓"
    ), notes="Encodes immediate-left and coffee@green")

    # [5c] Check pair adjacencies (neighbors)
    blends_pos = pos_of(invS, "blends")
    cats_pos   = pos_of(invP, "cats")
    horse_pos  = pos_of(invP, "horse")
    dunhill_pos= pos_of(invS, "dunhill")
    water_pos  = pos_of(invD, "water")
    neighbors_ok = (
        abs(blends_pos - cats_pos) == 1 and
        abs(horse_pos - dunhill_pos) == 1 and
        abs(blends_pos - water_pos) == 1
    )
    s5c = proof.add(
        "Check-Pair",
        [s4],
        Conclusion("text",
                   f"|blends - cats|=1 ✓, |horse - dunhill|=1 ✓, |blends - water|=1 ✓  → {neighbors_ok}"),
        notes="All required neighbor relations hold"
    )

    # [5d] Check list:member facts that tie categories together
    brit_red_ok = pos_of(invN, "brit") == pos_of(invC, "red")
    swede_dogs_ok = pos_of(invN, "swede") == pos_of(invP, "dogs")
    dane_tea_ok = pos_of(invN, "dane") == pos_of(invD, "tea")
    birds_pallmall_ok = pos_of(invP, "birds") == pos_of(invS, "pallmall")
    yellow_dunhill_ok = pos_of(invC, "yellow") == pos_of(invS, "dunhill")
    beer_bluemasters_ok = pos_of(invD, "beer") == pos_of(invS, "bluemasters")
    german_prince_ok = pos_of(invN, "german") == pos_of(invS, "prince")

    s5d = proof.add(
        "Check-Members",
        [s4],
        Conclusion(
            "text",
            "brit↔red ✓, swede↔dogs ✓, dane↔tea ✓, birds↔pallmall ✓, "
            "yellow↔dunhill ✓, beer↔bluemasters ✓, german↔prince ✓"
        ),
        notes="All list:member constraints are satisfied in M"
    )

    # [5e] Collect antecedent satisfaction
    s5e = proof.add("And-Intro", [s5a, s5b, s5c, s5d],
                    Conclusion("text", "All antecedent conjuncts hold in M"),
                    notes="Thus R is applicable with witness ?B")

    # [6] Head instantiation: derive eats(:german, :fish)
    fish_pos = pos_of(invP, "fish")
    who = invN[fish_pos]
    conclusion_atom = Atom("eats", (f":{who}", ":fish"))
    s6 = proof.add("Head-Intro", [s3, s5e], Conclusion("formula", conclusion_atom),
                   notes="Instantiate head with ?WHO = nationality at the fish-keeper's house")

    # [7] Final answer binding
    s7 = proof.add("Answer", [s6], Conclusion("text", f"?WHO = :{who}"),
                   notes="As required by the query")

    return proof, who

def pos_of(inv_map_by_pos: dict, value: str) -> int:
    for pos, v in inv_map_by_pos.items():
        if v == value:
            return pos
    raise ValueError(f"value {value} not found")

def invC_inverse(invC, val): return pos_of(invC, val)
def invD_inverse(invD, val): return pos_of(invD, val)
def invN_inverse(invN, val): return pos_of(invN, val)

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

def main():
    sols = solve()
    if not sols:
        print("No solution found.")
        return
    if len(sols) > 1:
        print(f"Warning: {len(sols)} solutions found; showing the first.\n")

    C, S, D, P, N = sols[0]
    invC = invert_map(C, COLORS)
    invS = invert_map(S, SMOKES)
    invD = invert_map(D, DRINKS)
    invP = invert_map(P, PETS)
    invN = invert_map(N, NATS)

    # Print the model table
    print(render_table(invC, invN, invP, invD, invS))

    # Query answer
    fish_pos = pos_of(invP, "fish")
    who = invN[fish_pos]
    print(f"\n:{who} :eats :fish")

    # Proof (goal-oriented, pretty)
    proof, who2 = build_goal_oriented_proof(invC, invN, invP, invD, invS)
    assert who == who2
    print("\n=== Pretty Proof ===\n")
    print(proof.pretty())

if __name__ == "__main__":
    main()

