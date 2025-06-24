"""
family.py — a tiny self‑contained microKanren implementation in Python with
a small family knowledge‑base (father, mother, child, brother, sister, uncle,
aunt, grandfather, grandmother, …).
"""
from itertools import count
from typing import Dict, Iterable, Callable, List, Tuple, Any

# -----------------------------------------------------------------------------
# microKanren core
# -----------------------------------------------------------------------------

class Var:
    __slots__ = ("n",)
    def __init__(self, n: int):
        self.n = n
    def __repr__(self):
        return f"_{self.n}"
    def __hash__(self):
        return hash(self.n)
    def __eq__(self, other):
        return isinstance(other, Var) and self.n == other.n

_counter = count()                    # fresh‑variable counter
Subst = Dict[Var, Any]                # substitution type alias


def fresh_var() -> Var:
    return Var(next(_counter))


def walk(u: Any, s: Subst) -> Any:
    while isinstance(u, Var) and u in s:
        u = s[u]
    return u


def ext(s: Subst, v: Var, val: Any) -> Subst:
    s2 = s.copy(); s2[v] = val; return s2


def unify(u: Any, v: Any, s: Subst) -> Subst | None:
    u, v = walk(u, s), walk(v, s)
    if isinstance(u, Var):
        return ext(s, u, v)
    if isinstance(v, Var):
        return ext(s, v, u)
    if type(u) != type(v):
        return None
    if isinstance(u, (tuple, list)):
        if len(u) != len(v):
            return None
        for a, b in zip(u, v):
            s = unify(a, b, s)
            if s is None:
                return None
        return s
    return s if u == v else None

Goal = Callable[[Subst], Iterable[Subst]]


def succeed(s: Subst):
    yield s

def fail(_: Subst):
    if False:
        yield  # never


def eq(u: Any, v: Any) -> Goal:
    return lambda s: (x for x in (unify(u, v, s),) if x is not None)


def disj(*goals: Goal) -> Goal:
    # Preserve argument order deterministically
    return lambda s: (g for goal in goals for g in goal(s))


def conj(*goals: Goal) -> Goal:
    if not goals:
        return succeed
    first, *rest = goals
    def goal(s: Subst):
        for s1 in first(s):
            yield from conj(*rest)(s1)
    return goal


def call_fresh(f: Callable[[Var], Goal]) -> Goal:
    return lambda s: f(fresh_var())(s)


def neq(u: Any, v: Any) -> Goal:
    def goal(s: Subst):
        u1, v1 = walk(u, s), walk(v, s)
        if isinstance(u1, Var) or isinstance(v1, Var):
            yield from disj(conj(eq(u1, v1), fail), succeed)(s)
        elif u1 != v1:
            yield s
    return goal

# -----------------------------------------------------------------------------
# Reification & run helpers
# -----------------------------------------------------------------------------

def reify(q: Any, s: Subst) -> Any:
    mapping: Dict[Var, str] = {}
    def walk_star(u: Any):
        u = walk(u, s)
        if isinstance(u, Var):
            if u not in mapping:
                mapping[u] = f"_{len(mapping)}"
            return mapping[u]
        if isinstance(u, (tuple, list)):
            return tuple(walk_star(x) for x in u)
        return u
    return walk_star(q)


def run(n: int | None, goal_builder: Callable[[Var], Goal]) -> List[Any]:
    q = fresh_var()
    goal = goal_builder(q)
    out: List[Any] = []
    for s in goal({}):
        out.append(reify(q, s))
        if n is not None and len(out) >= n:
            break
    return out


def run_unique(n: int | None, goal_builder: Callable[[Var], Goal]) -> List[Any]:
    seen, out = set(), []
    for ans in run(None, goal_builder):
        if ans not in seen:
            seen.add(ans)
            out.append(ans)
            if n is not None and len(out) >= n:
                break
    return out

# -----------------------------------------------------------------------------
# Ordered family facts & derived relations
# -----------------------------------------------------------------------------

FatherFacts: List[Tuple[str, str]] = [
    ("john", "linda"),
    ("john", "robert"),
    ("robert", "james"),
    ("robert", "patricia"),
    ("michael", "barbara"),
]

MotherFacts: List[Tuple[str, str]] = [
    ("mary", "linda"),
    ("mary", "robert"),
    ("linda", "jane"),
    ("linda", "patrick"),
    ("barbara", "anne"),
]

Males   = ["john", "robert", "james", "michael", "patrick"]
Females = ["mary", "linda", "patricia", "barbara", "anne", "jane"]


def fact_relation(facts: List[Tuple[str, str]]):
    def rel(x: Any, y: Any) -> Goal:
        # Build goals in the fixed order of the list
        return disj(*(conj(eq(x, a), eq(y, b)) for a, b in facts))
    return rel

father = fact_relation(FatherFacts)
mother = fact_relation(MotherFacts)

def male(x):   return disj(*(eq(x, m) for m in Males))
def female(x): return disj(*(eq(x, f) for f in Females))

parent      = lambda x, y: disj(father(x, y), mother(x, y))
child       = lambda x, y: parent(y, x)

def sibling(x, y):
    return conj(call_fresh(lambda p: conj(parent(p, x), parent(p, y))), neq(x, y))

brother     = lambda x, y: conj(sibling(x, y), male(x))
sister      = lambda x, y: conj(sibling(x, y), female(x))

grandparent = lambda x, y: call_fresh(lambda p: conj(parent(x, p), parent(p, y)))
grandfather = lambda x, y: conj(grandparent(x, y), male(x))
grandmother = lambda x, y: conj(grandparent(x, y), female(x))

uncle = lambda x, y: call_fresh(lambda p: conj(parent(p, y), brother(x, p)))
aunt  = lambda x, y: call_fresh(lambda p: conj(parent(p, y), sister(x, p)))

# -----------------------------------------------------------------------------
# Demo
# -----------------------------------------------------------------------------
if __name__ == "__main__":
    print("Children of robert:", run_unique(None, lambda q: child(q, "robert")))
    print("Grandparents of patricia:", run_unique(None, lambda q: grandparent(q, "patricia")))
    print("Sisters of james:", run_unique(None, lambda q: sister(q, "james")))
    print("Uncles of james:", run_unique(None, lambda q: uncle(q, "james")))

    print("(Unique) sibling pairs:")

# Build a goal that reifies a *tuple* (x, y) rather than a single var

def sibling_pair(pair):
    return call_fresh(
        lambda x: call_fresh(
            lambda y: conj(
                eq(pair, (x, y)),       # pair = (x, y)
                sibling(x, y),
                male(x)                 # optional: only show male first
            )
        )
    )

for tup in run_unique(10, sibling_pair):
    print("  ", tup)

