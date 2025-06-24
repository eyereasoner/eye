"""
family.py — a tiny self‑contained microKanren implementation in Python with
a small family knowledge‑base (father, mother, child, brother, sister, uncle,
aunt, grandfather, grandmother, …).
"""
from itertools import count
from typing import Dict, Generator, Iterable, Tuple, Callable, List, Any

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

_counter = count()                     # global counter to create fresh vars
Subst = Dict[Var, Any]                 # type alias for substitutions


def fresh_var() -> Var:
    return Var(next(_counter))


def walk(u: Any, s: Subst) -> Any:
    """Follow substitution links until reaching a non‑variable."""
    while isinstance(u, Var) and u in s:
        u = s[u]
    return u


def ext(s: Subst, x: Var, v: Any) -> Subst:
    s2 = s.copy(); s2[x] = v; return s2


def unify(u: Any, v: Any, s: Subst) -> Subst | None:
    """Most general unifier (MGU). Returns new subst or None on failure."""
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

Goal = Callable[[Subst], Iterable[Subst]]  # Goal type alias


def succeed(s: Subst):
    yield s


def fail(_: Subst):
    if False:
        yield  # never


def eq(u: Any, v: Any) -> Goal:
    return lambda s: (x for x in (unify(u, v, s),) if x is not None)


def disj(*goals: Goal) -> Goal:        # logical OR
    return lambda s: (yield from (g for goal in goals for g in goal(s)))


def conj(*goals: Goal) -> Goal:        # logical AND
    if not goals:
        return succeed
    first, *rest = goals
    def goal(s: Subst):
        for s1 in first(s):
            yield from conj(*rest)(s1)
    return goal


def call_fresh(f: Callable[[Var], Goal]) -> Goal:
    return lambda s: f(fresh_var())(s)

# Negation‑as‑failure inequality

def neq(u: Any, v: Any) -> Goal:
    def goal(s: Subst):
        u1, v1 = walk(u, s), walk(v, s)
        if isinstance(u1, Var) or isinstance(v1, Var):
            yield from disj(conj(eq(u1, v1), fail), succeed)(s)
        elif u1 != v1:
            yield s
    return goal

# -----------------------------------------------------------------------------
# Reification helpers and run/1, run_unique/1
# -----------------------------------------------------------------------------

def reify(q: Any, s: Subst) -> Any:
    """Turn residual vars into readable symbols _0, _1 …"""
    r: Dict[Var, str] = {}
    def walk_star(u: Any):
        u = walk(u, s)
        if isinstance(u, Var):
            if u not in r:
                r[u] = f"_{len(r)}"
            return r[u]
        if isinstance(u, (tuple, list)):
            return tuple(walk_star(x) for x in u)
        return u
    return walk_star(q)


def run(n: int | None, goal_builder: Callable[[Var], Goal]) -> List[Any]:
    """Collect up to *n* answers (None = all)."""
    q = fresh_var()
    goal = goal_builder(q)
    out: List[Any] = []
    for s in goal({}):
        out.append(reify(q, s))
        if n is not None and len(out) >= n:
            break
    return out


def run_unique(n: int | None, goal_builder: Callable[[Var], Goal]) -> List[Any]:
    """`run` but remove duplicate answers while preserving order."""
    seen, out = set(), []
    for ans in run(None, goal_builder):
        if ans not in seen:
            seen.add(ans)
            out.append(ans)
            if n is not None and len(out) >= n:
                break
    return out

# -----------------------------------------------------------------------------
# Family knowledge‑base (facts) and derived relations
# -----------------------------------------------------------------------------

_father_facts: set[Tuple[str, str]] = {
    ("john", "robert"), ("john", "linda"),
    ("robert", "patricia"), ("robert", "james"),
    ("michael", "barbara"),
}

_mother_facts: set[Tuple[str, str]] = {
    ("mary", "robert"), ("mary", "linda"),
    ("linda", "patrick"), ("linda", "jane"),
    ("barbara", "anne"),
}

_males   = {"john", "robert", "james", "michael", "patrick"}
_females = {"mary", "linda", "patricia", "barbara", "anne", "jane"}


def fact_relation(facts: set[Tuple[str, str]]):
    def rel(x, y):
        return disj(*(conj(eq(x, a), eq(y, b)) for a, b in facts))
    return rel

father = fact_relation(_father_facts)
mother = fact_relation(_mother_facts)

def male(x):   return disj(*(eq(x, m) for m in _males))
def female(x): return disj(*(eq(x, f) for f in _females))

parent      = lambda x, y: disj(father(x, y), mother(x, y))
child       = lambda x, y: parent(y, x)

sibling     = lambda x, y: conj(call_fresh(lambda p: conj(parent(p, x), parent(p, y))), neq(x, y))
brother     = lambda x, y: conj(sibling(x, y), male(x))
sister      = lambda x, y: conj(sibling(x, y), female(x))

grandparent   = lambda x, y: call_fresh(lambda p: conj(parent(x, p), parent(p, y)))
grandfather   = lambda x, y: conj(grandparent(x, y), male(x))
grandmother   = lambda x, y: conj(grandparent(x, y), female(x))

uncle       = lambda x, y: call_fresh(lambda p: conj(parent(p, y), brother(x, p)))
aunt        = lambda x, y: call_fresh(lambda p: conj(parent(p, y), sister(x, p)))

# -----------------------------------------------------------------------------
# Demo (only runs when executed as a script)
# -----------------------------------------------------------------------------

if __name__ == "__main__":
    print("Children of robert:", run_unique(None, lambda q: child(q, "robert")))
    print("Grandparents of patricia:", run_unique(None, lambda q: grandparent(q, "patricia")))
    print("Sisters of james:", run_unique(None, lambda q: sister(q, "james")))
    print("Uncles of james:", run_unique(None, lambda q: uncle(q, "james")))

    # Proper sibling pairs (unordered, duplicates removed)
    def sibling_pair(pair):
        return call_fresh(
            lambda x: call_fresh(
                lambda y: conj(
                    eq(pair, (x, y)),
                    sibling(x, y),
                    neq(x, y)   # avoid (bob, bob)
                )
            )
        )

    pairs = run_unique(10, sibling_pair)
    print("(Unique) sibling pairs:")
    for p in pairs:
        print("  ", p)

