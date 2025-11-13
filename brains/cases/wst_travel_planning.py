# -*- coding: utf-8 -*-
"""
CASE MODULE (standalone runnable)
=================================
wstLogic-style State Transition Planning (Travel) with COST *and* TIME
in the Hayes–Menzel holds₂ setting
----------------------------------

This example mirrors state-transition planning in a small, bounded-horizon world
and shows how we can model both **cost** and **time** — still staying first-order
by treating action names and property names as *intensions* (URIs), and applying
them through fixed predicates `holds1/2`.

Big picture
-----------
• We have discrete "states" S0→S1→S2→S3 (a tiny time line).
• A *class name* (e.g., ex:At_Paris) holding in a given state S is written:
      ex:holds1(C, S)
  meaning S satisfies "C".

• An *action name* (e.g., ex:Fly_Brussels_Paris) creates a transition S→S1:
      ex:holds2(A, S, S1)
  meaning: doing action A at S leads to S1 (if enabled).

• Actions have metadata (first-order facts about names):
    ex:Pre(A, Cpre)   -- precondition (class to hold in FROM state)
    ex:Add(A, Cadd)   -- effect     (class that will hold in TO state)
    ex:Cost(A, c)     -- cost of A      (small natural encoded as a name "0".."9")
    ex:Time(A, t)     -- time of A      (ditto)

• We compute paths with two derived measures in parallel:
    ex:PathCost(S0, S, c)   -- cumulative cost from S0 to S
    ex:PathTime(S0, S, t)   -- cumulative time from S0 to S

  using a small, explicit addition table ex:Plus(u, v, w) and an order ex:LeqN(u,v).

• Minimality is selected with a classic positive "lower bound" trick (no negation):
    ex:BestCost(S, c)  holds if PathCost(S0,S,c) and c ≤ c' for *every* PathCost(S0,S,c').
    ex:BestTime(S, t)  similarly for time.

• We keep it first-order: there are no function symbols and no arithmetic built in;
  everything is expressed through relations over *names* (intensions) and individuals.

How to run
----------
    python wst_travel_planning_cost_time.py
    python eyezero.py wst_travel_planning_cost_time.py

Printed sections
----------------
Model → Question → Answer → Reason why → Check (12 tests)

Notes
-----
To make materialization robust, we split action enabling into two steps:
  1) Enabled(A,S) :- Pre(A,C), holds1(C,S).
  2) holds2(A,S,S1) :- Enabled(A,S), Next(S,S1).
This avoids brittle multi-way joins and works well with naive bottom-up iteration.

Everything else (effects, cost and time accumulation, minimal selectors) then follows.
"""

from typing import List, Tuple
from eyezero import (
    Var, Atom, Clause, atom, fact,
    solve_topdown, solve_bottomup, match_against_facts,
    NAME, IND, Signature, deref,
    local,
)

# ─────────────────────────────────────────────────────────────────────────────
# Domain (finite individuals): 4 states (horizon 3)
# ─────────────────────────────────────────────────────────────────────────────
S0, S1, S2, S3 = "S0", "S1", "S2", "S3"
STATES: Tuple[str, ...] = (S0, S1, S2, S3)

# Cities → we turn each into a class name ex:At_<City>
CITIES = ("Brussels", "Paris", "London", "Berlin")
EX = "ex:"
At = {city: EX + f"At_{city}" for city in CITIES}

# Action *names* (intensions)
Fly_BP   = EX + "Fly_Brussels_Paris"
Train_BP = EX + "Train_Brussels_Paris"
Fly_PL   = EX + "Fly_Paris_London"
Fly_PB   = EX + "Fly_Paris_Berlin"
Train_LB = EX + "Train_London_Berlin"

# Meta over action names (still first-order)
Pre      = EX + "Pre"      # NAME×NAME  (action, pre-class)
Add      = EX + "Add"      # NAME×NAME  (action, add-class)
Cost     = EX + "Cost"     # NAME×NAME  (action, cost-as-name "0".."9")
Time     = EX + "Time"     # NAME×NAME  (action, time-as-name "0".."9")

# State successor (no functions, so we encode the small timeline explicitly)
Next     = EX + "Next"     # IND×IND    (S, S1)

# Application predicates (the Hayes–Menzel core)
Holds1   = EX + "holds1"   # NAME×IND         : class C holds in state S
Holds2   = EX + "holds2"   # NAME×IND×IND     : action A takes S to S1

# Derived helpers / measures
Enabled  = EX + "Enabled"   # NAME×IND        : A is enabled at S (Pre satisfied)
Plus     = EX + "Plus"      # NAME×NAME×NAME  : tiny addition table over names
LeqN     = EX + "LeqN"      # NAME×NAME       : total order on small naturals
PathCost = EX + "PathCost"  # IND×IND×NAME    : cumulative cost S0→S
PathTime = EX + "PathTime"  # IND×IND×NAME    : cumulative time S0→S
BestCost = EX + "BestCost"  # IND×NAME        : minimal cost to S
BestTime = EX + "BestTime"  # IND×NAME        : minimal time to S
Forbidden= EX + "Forbidden" # NAME            : action is forbidden (policy tag)

# Signature (NAME/IND) — lets the engine ground head-only variables safely
SIGNATURE: Signature = {
    Holds1:   (NAME, IND),
    Holds2:   (NAME, IND, IND),
    Pre:      (NAME, NAME),
    Add:      (NAME, NAME),
    Cost:     (NAME, NAME),
    Time:     (NAME, NAME),
    Next:     (IND, IND),
    Enabled:  (NAME, IND),
    Plus:     (NAME, NAME, NAME),
    LeqN:     (NAME, NAME),
    PathCost: (IND, IND, NAME),
    PathTime: (IND, IND, NAME),
    BestCost: (IND, NAME),
    BestTime: (IND, NAME),
    Forbidden:(NAME,),
}

# ─────────────────────────────────────────────────────────────────────────────
# PROGRAM (facts + rules)
# ─────────────────────────────────────────────────────────────────────────────
PROGRAM: List[Clause] = []

# Initial condition: at Brussels in S0
PROGRAM.append(fact(Holds1, At["Brussels"], S0))

# Small successor chain (bounded horizon)
for a,b in [(S0,S1),(S1,S2),(S2,S3)]:
    PROGRAM.append(fact(Next, a, b))

# Action schema: preconditions, effects, costs, times
PROGRAM += [
    # Brussels → Paris
    fact(Pre,    Fly_BP,   At["Brussels"]),  fact(Add,   Fly_BP,   At["Paris"]),
    fact(Cost,   Fly_BP,   "1"),              fact(Time,  Fly_BP,   "1"),

    fact(Pre,    Train_BP, At["Brussels"]),  fact(Add,   Train_BP, At["Paris"]),
    fact(Cost,   Train_BP, "2"),              fact(Time,  Train_BP, "2"),

    # Paris → London / Berlin
    fact(Pre,    Fly_PL,   At["Paris"]),     fact(Add,   Fly_PL,   At["London"]),
    fact(Cost,   Fly_PL,   "1"),              fact(Time,  Fly_PL,   "2"),

    fact(Pre,    Fly_PB,   At["Paris"]),     fact(Add,   Fly_PB,   At["Berlin"]),
    fact(Cost,   Fly_PB,   "2"),              fact(Time,  Fly_PB,   "2"),

    # London → Berlin
    fact(Pre,    Train_LB, At["London"]),    fact(Add,   Train_LB, At["Berlin"]),
    fact(Cost,   Train_LB, "2"),              fact(Time,  Train_LB, "3"),
]

# Optional policy example: flying into London is flagged Forbidden
PROGRAM.append(fact(Forbidden, Fly_PL))

# Phase 1: enable an action in state S when its precondition holds
A, S, Cpre = Var("A"), Var("S"), Var("Cpre")
PROGRAM.append(Clause(atom(Enabled, A, S),
                      [atom(Pre, A, Cpre), atom(Holds1, Cpre, S)]))

# Phase 2: generate transitions along Next from any enabled action
S1v = Var("S1")
PROGRAM.append(Clause(atom(Holds2, A, S, S1v),
                      [atom(Enabled, A, S), atom(Next, S, S1v)]))

# Effects: whatever Add(A,Cadd) says, add that class to the TO state
Cadd = Var("Cadd")
PROGRAM.append(Clause(atom(Holds1, Cadd, S1v),
                      [atom(Holds2, A, S, S1v), atom(Add, A, Cadd)]))

# Aggregation tables over tiny naturals (names "0".."9")
NATS = [str(i) for i in range(10)]
for u in NATS:
    PROGRAM.append(fact(Plus, "0", u, u))
for u in [str(i) for i in range(1,10)]:
    for v in NATS:
        w = min(9, int(u)+int(v))   # saturate at 9 for safety (tiny domain)
        PROGRAM.append(fact(Plus, u, v, str(w)))

# Total order ≤ over those tiny naturals
for i,u in enumerate(NATS):
    for v in NATS[i:]:
        PROGRAM.append(fact(LeqN, u, v))

# Path measures (both cost and time), in lockstep
Si, Sj, C0, C1, Cw = Var("Si"), Var("Sj"), Var("C0"), Var("C1"), Var("Cw")
Ti, Tj, T0, T1, Tw = Var("Ti"), Var("Tj"), Var("T0"), Var("T1"), Var("Tw")
A2 = Var("A2")

# Base: zero cost/time at the start
PROGRAM.append(fact(PathCost, S0, S0, "0"))
PROGRAM.append(fact(PathTime, S0, S0, "0"))

# Extend by one step using the action's Cost/Time and Plus
PROGRAM.append(Clause(atom(PathCost, S0, Sj, C1),
                      [atom(PathCost, S0, Si, C0),
                       atom(Holds2, A2, Si, Sj), atom(Cost, A2, Cw),
                       atom(Plus, C0, Cw, C1)]))
PROGRAM.append(Clause(atom(PathTime, S0, Sj, T1),
                      [atom(PathTime, S0, Si, T0),
                       atom(Holds2, A2, Si, Sj), atom(Time, A2, Tw),
                       atom(Plus, T0, Tw, T1)]))

# Minimal selectors (no negation → use "lower bound" trick)
Sg, Cmin, Cother = Var("Sg"), Var("Cmin"), Var("Cother")
PROGRAM.append(Clause(atom(BestCost, Sg, Cmin),
                      [atom(PathCost, S0, Sg, Cmin),
                       atom(LeqN, Cmin, Cother), atom(PathCost, S0, Sg, Cother)]))

Sg2, Tmin, Tother = Var("Sg2"), Var("Tmin"), Var("Tother")
PROGRAM.append(Clause(atom(BestTime, Sg2, Tmin),
                      [atom(PathTime, S0, Sg2, Tmin),
                       atom(LeqN, Tmin, Tother), atom(PathTime, S0, Sg2, Tother)]))

# ─────────────────────────────────────────────────────────────────────────────
# Case-specific engine glue
# ─────────────────────────────────────────────────────────────────────────────
def _is_var(t) -> bool: return isinstance(t, Var)

def choose_engine(goals: List[Atom]) -> str:
    """Heuristic: enumeration-heavy goals use bottom-up; targeted goals use top-down."""
    heavy = {Holds2, Enabled, PathCost, PathTime, BestCost, BestTime}
    for g in goals:
        if g.pred in heavy and any(_is_var(t) for t in g.args):
            return "bottomup"
    return "topdown"

def ask(goals: List[Atom], step_limit: int = 10000, fallback_threshold: int = 4000):
    engine = choose_engine(goals)
    if engine == "topdown":
        sols, metric = solve_topdown(PROGRAM, goals, step_limit=step_limit)
        return engine, sols, metric
    else:
        facts, rounds = solve_bottomup(PROGRAM, SIGNATURE)
        sols = match_against_facts(goals, facts)
        return engine, sols, rounds

# ─────────────────────────────────────────────────────────────────────────────
# Presentation
# ─────────────────────────────────────────────────────────────────────────────
def print_model() -> None:
    print("Model")
    print("=====")
    print(f"States (time steps) = {list(STATES)}")
    print("Locations as classes: " + ", ".join(local(At[c]) for c in CITIES))
    print("\nFixed predicates (signature)")
    print("----------------------------")
    print("• ex:holds1(C,S)     — C holds in state S; sorts: (NAME, IND)")
    print("• ex:holds2(A,S,S1)  — do action-name A from S to S1; sorts: (NAME, IND, IND)")
    print("• ex:Pre/Add         — action precondition/effect (first-order over *names*)")
    print("• ex:Cost/Time       — small naturals as *names* for measures")
    print("• ex:PathCost/PathTime, ex:BestCost/BestTime — accumulated and minimal measures")
    print("\nInitial & step graph")
    print("--------------------")
    print(f"Initial: {local(At['Brussels'])} holds in S0; step chain S0→S1→S2→S3.")
    print("\nActions (cost, time)")
    print("--------------------")
    print("• Fly Brussels→Paris (cost 1, time 1)  | Train Brussels→Paris (2, 2)")
    print("• Fly Paris→London (1, 2) [Forbidden]  | Fly Paris→Berlin (2, 2)")
    print("• Train London→Berlin (2, 3)\n")

def print_question() -> None:
    print("Question")
    print("========")
    print("Q1) Enumerate all plan edges holds2(A,S,S1).                              [auto engine]")
    print("Q2) Minimal COST to reach any state where At_Berlin holds?                [auto engine]")
    print("Q3) Minimal TIME to reach any state where At_Berlin holds?                [auto engine]")
    print("Q4) Example (COST, TIME) pairs to S2 (Berlin via Fly_BP then Fly_PB).     [auto engine]")
    print("Q5) Policy: does a transition using Fly_Paris_London appear (Forbidden)?  [auto engine]")
    print()

def run_queries():
    # Q1 enumerate edges
    Av, Sv, Sv1 = Var("A"), Var("S"), Var("S1")
    eng1, sols1, m1 = ask([atom(Holds2, Av, Sv, Sv1)])
    edges = sorted({(deref(Av,s), deref(Sv,s), deref(Sv1,s)) for s in sols1})

    # Q2 minimal cost to any Berlin-reaching state
    Sgoal, Cmin = Var("Sg"), Var("Cmin")
    eng2, sols2, m2 = ask([atom(Holds1, At["Berlin"], Sgoal),
                           atom(BestCost, Sgoal, Cmin)])
    best_costs = sorted({(deref(Sgoal,s), deref(Cmin,s)) for s in sols2 if isinstance(deref(Cmin,s), str)})

    # Q3 minimal time to any Berlin-reaching state
    SgoalT, Tmin = Var("SgT"), Var("Tmin")
    eng3, sols3, m3 = ask([atom(Holds1, At["Berlin"], SgoalT),
                           atom(BestTime, SgoalT, Tmin)])
    best_times = sorted({(deref(SgoalT,s), deref(Tmin,s)) for s in sols3 if isinstance(deref(Tmin,s), str)})

    # Q4 concrete (cost,time) pairs for S2
    Cvar, Tvar = Var("C"), Var("T")
    eng4, sols4, m4 = ask([atom(Holds1, At["Berlin"], S2),
                           atom(PathCost, S0, S2, Cvar),
                           atom(PathTime, S0, S2, Tvar)])

    # Q5 forbidden presence
    eng5, sols5, m5 = ask([atom(Holds2, Fly_PL, Var("Sx"), Var("Sy")), atom(Forbidden, Fly_PL)])

    return (("Q1", eng1, edges, m1),
            ("Q2", eng2, best_costs, m2),
            ("Q3", eng3, best_times, m3),
            ("Q4", eng4, (Cvar, Tvar, sols4), m4),
            ("Q5", eng5, bool(sols5), m5))

def print_answer(res1, res2, res3, res4, res5) -> None:
    print("Answer")
    print("======")
    tag1, eng1, edges, _ = res1
    tag2, eng2, best_costs, _ = res2
    tag3, eng3, best_times, _ = res3
    tag4, eng4, (Cvar, Tvar, sols4), _ = res4
    tag5, eng5, forb_ok, _ = res5

    if edges:
        triples = ", ".join(f"{local(a)}:{s}->{t}" for (a,s,t) in edges)
        print(f"{tag1}) Engine: {eng1} → {triples}")
    else:
        print(f"{tag1}) Engine: {eng1} → ∅")

    if best_costs:
        show = ", ".join(f"{sg}@cost={c}" for (sg,c) in best_costs)
        print(f"{tag2}) Engine: {eng2} → minimal costs: {show}")
    else:
        print(f"{tag2}) Engine: {eng2} → no Berlin reachable")

    if best_times:
        show = ", ".join(f"{sg}@time={t}" for (sg,t) in best_times)
        print(f"{tag3}) Engine: {eng3} → minimal times: {show}")
    else:
        print(f"{tag3}) Engine: {eng3} → no Berlin reachable")

    if sols4:
        cvals = sorted({deref(Cvar, s) for s in sols4 if isinstance(deref(Cvar, s), str)})
        tvals = sorted({deref(Tvar, s) for s in sols4 if isinstance(deref(Tvar, s), str)})
        print(f"{tag4}) Engine: {eng4} → for S2: PathCost in {cvals}, PathTime in {tvals}")
    else:
        print(f"{tag4}) Engine: {eng4} → no (cost,time) example found")

    print(f"{tag5}) Engine: {eng5} → Forbidden action present among transitions: {'Yes' if forb_ok else 'No'}\n")

def print_reason(eng1, eng2) -> None:
    print("Reason why")
    print("==========")
    print("• Names as intensions (URIs); application via fixed holds₁/holds₂ → stays first-order.")
    print("• Enabling split (Enabled(A,S) then holds2(A,S,S1)) makes bottom-up robust.")
    print("• Cost and time both use explicit tiny arithmetic (Plus, LeqN) on names ('0'..'9').")
    print("• PathCost/PathTime accumulate measures; BestCost/BestTime select minima via a lower-bound trick.")
    print("• Policy (Forbidden) is just a tag you can check in queries.\n")

# ─────────────────────────────────────────────────────────────────────────────
# Check (12 tests)
# ─────────────────────────────────────────────────────────────────────────────
class CheckFailure(AssertionError): pass
def check(c: bool, msg: str):
    if not c: raise CheckFailure(msg)

def run_checks() -> List[str]:
    notes: List[str] = []

    facts, _ = solve_bottomup(PROGRAM, SIGNATURE)

    # 1) Core edges appear (S0→S1 by Fly_BP; S1→S2 by Fly_PB)
    Aed, Sed, Ted = Var("A"), Var("S"), Var("T")
    bu = match_against_facts([atom(Holds2, Aed, Sed, Ted)], facts)
    edges = {(deref(Aed,s), deref(Sed,s), deref(Ted,s)) for s in bu}
    check((Fly_BP, S0, S1) in edges and (Fly_PB, S1, S2) in edges, "Core edges missing.")
    notes.append("PASS 1: Core transitions generated (S0→S1 via Fly_BP; S1→S2 via Fly_PB).")

    # 2) Berlin is reachable at S2
    check(bool(match_against_facts([atom(Holds1, At["Berlin"], S2)], facts)), "Berlin not reachable at S2.")
    notes.append("PASS 2: Berlin reachable at S2.")

    # 3) PathCost exists for S2
    Cq = Var("Cq")
    pc = match_against_facts([atom(PathCost, S0, S2, Cq)], facts)
    check(bool(pc), "PathCost to S2 missing.")
    notes.append("PASS 3: PathCost to S2 exists.")

    # 4) A cost 3 plan to S2 exists (Fly_BP=1 + Fly_PB=2)
    cvals = {deref(Cq, s) for s in pc}
    check("3" in cvals, "Expected cost 3 among costs to S2.")
    notes.append("PASS 4: A cost 3 plan to S2 exists.")

    # 5) BestCost(S2,3) present (lower-bound selection)
    Cmin = Var("Cmin")
    bc = match_against_facts([atom(BestCost, S2, Cmin)], facts)
    check(any(deref(Cmin, s) == "3" for s in bc), "BestCost for S2 should include 3.")
    notes.append("PASS 5: BestCost(S2,3) present.")

    # 6) PathTime exists for S2 and includes 3 (1 + 2)
    Tq = Var("Tq")
    pt = match_against_facts([atom(PathTime, S0, S2, Tq)], facts)
    check(bool(pt), "PathTime to S2 missing.")
    check("3" in {deref(Tq, s) for s in pt}, "Expected time 3 among times to S2.")
    notes.append("PASS 6: PathTime to S2 includes 3.")

    # 7) BestTime(S2,3) present
    Tmin = Var("Tmin")
    bt = match_against_facts([atom(BestTime, S2, Tmin)], facts)
    check(any(deref(Tmin, s) == "3" for s in bt), "BestTime for S2 should include 3.")
    notes.append("PASS 7: BestTime(S2,3) present.")

    # 8) Forbidden action is detectable among transitions
    check(bool(match_against_facts([atom(Holds2, Fly_PL, Var("Sx"), Var("Sy")), atom(Forbidden, Fly_PL)], facts)),
          "Forbidden action not detectable.")
    notes.append("PASS 8: Forbidden action detectable among transitions.")

    # 9) No transitions from the last state S3
    none = match_against_facts([atom(Holds2, Var("A"), S3, Var("T"))], facts)
    check(not none, "Unexpected transitions from final state S3.")
    notes.append("PASS 9: No transitions from S3.")

    # 10) Tabled top-down can prove the Berlin-at-S2 goal
    td, _ = solve_topdown(PROGRAM, [atom(Holds1, At["Berlin"], S2)])
    check(bool(td), "Top-down failed to prove goal at S2.")
    notes.append("PASS 10: Tabled top-down proves the goal.")

    # 11) Bottom-up closure stability
    f1,_ = solve_bottomup(PROGRAM, SIGNATURE); f2,_ = solve_bottomup(PROGRAM, SIGNATURE)
    check(f1[Holds2]==f2[Holds2] and f1[Holds1]==f2[Holds1] and f1[PathCost]==f2[PathCost] and f1[PathTime]==f2[PathTime],
          "Bottom-up closure not stable.")
    notes.append("PASS 11: Bottom-up closure stable.")

    # 12) Engine chooser: enumeration vs ground
    e1, _, _ = ask([atom(Holds2, Var("A"), Var("S"), Var("T"))])
    e2, _, _ = ask([atom(Holds1, At["Berlin"], S2)])
    check(e1=="bottomup" and e2=="topdown", "Engine chooser mismatch.")
    notes.append("PASS 12: Engine chooser behaves as intended.")

    return notes

# ─────────────────────────────────────────────────────────────────────────────
# Standalone runner (works with the dual engine runner as well)
# ─────────────────────────────────────────────────────────────────────────────
def main():
    print_model()
    print_question()
    res1, res2, res3, res4, res5 = run_queries()
    print_answer(res1, res2, res3, res4, res5)
    print_reason(res1[1], res2[1])
    print("Check (harness)")
    print("===============")
    class CheckFailure(AssertionError): pass
    try:
        for note in run_checks():
            print(note)
    except CheckFailure as e:
        print("FAIL:", e)
        raise

if __name__ == "__main__":
    main()

