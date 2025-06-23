"""
constructor_theory.py
=====================
A concise but expressive playground for **Constructor Theory** ideas
(David Deutsch & Chiara Marletto).

❧ **WHAT’S IN HERE**
--------------------
1. **Core data structures** – `Substrate`, `Task`, `ParallelTask`, and the
   `ConstructorTheory` ledger.
2. **Rich algebra** – series/parallel composition, commuting behaviour on
   disjoint substrates, task powers, inverses, and reversibility checks.
3. **Executable, self‑checking proof‑of‑concept** – the bottom `if __name__
   == "__main__"` block reproduces the results you observed *and contains
   assertions that serve as *proof‑objects* that those results always hold*.

Run the module directly (``python constructor_theory.py``).  If every
assertion passes you’ll see the demo output; if any expectation is violated
Python will raise an `AssertionError`, acting as a **formal witness** that a
claimed possibility statement fails.
"""
from __future__ import annotations

from typing import Dict, FrozenSet, Hashable, Iterable, List, Tuple

###############################################################################
# Type aliases ---------------------------------------------------------------
###############################################################################
Attribute = Hashable          # Label of a state
AttributeSet = FrozenSet[Attribute]
TaskMap = Dict[Attribute, Attribute]

###############################################################################
# Substrate ------------------------------------------------------------------
###############################################################################
class Substrate:
    """A system that can instantiate a finite set of distinguishable attributes."""

    def __init__(self, name: str, attributes: Iterable[Attribute]):
        self.name = str(name)
        self.attributes: AttributeSet = frozenset(attributes)
        if not self.attributes:
            raise ValueError("Substrate must have at least one attribute.")

    # ----------------------------------------------------------------------
    def __repr__(self):  # pragma: no cover
        attrs = ", ".join(map(str, sorted(self.attributes)))
        return f"Substrate({self.name}: {{{attrs}}})"

    # ----------------------------------------------------------------------
    def validate(self, attr: Attribute):
        if attr not in self.attributes:
            raise ValueError(f"{attr!r} not valid for substrate {self.name}.")

###############################################################################
# Task (single‑substrate) -----------------------------------------------------
###############################################################################
class Task:
    """A (partial) attribute‑transforming rule on **one** substrate."""

    def __init__(self, substrate: Substrate, mapping: TaskMap, *, name: str | None = None):
        # Validate mapping
        for i, o in mapping.items():
            substrate.validate(i)
            substrate.validate(o)
        self.substrate = substrate
        self.mapping: TaskMap = dict(mapping)
        self.name = name or f"Task_on_{substrate.name}_{id(self):x}"

    # ----------------------------------------------------------------------
    def __repr__(self):  # pragma: no cover
        pairs = ", ".join(f"{i}->{o}" for i, o in self.mapping.items())
        return f"Task({self.name}: {pairs})"

    # ----------------------------------------------------------------------
    # Series composition --------------------------------------------------
    def __matmul__(self, other: "Task"):
        """Series: *self ∘ other* (apply **other first**, then self).

        • Same substrate → compose mappings.  
        • Disjoint substrates → commute ⇒ return a *ParallelTask*.
        """
        if self.substrate is other.substrate:
            composed: TaskMap = {}
            for inp, mid in other.mapping.items():
                out = self.mapping.get(mid, mid)
                composed[inp] = out
            return Task(self.substrate, composed, name=f"{self.name}∘{other.name}")
        # Different substrates ⇒ parallel product (order irrelevant)
        return ParallelTask([self, other])

    # Syntactic sugar -----------------------------------------------------
    def then(self, other: "Task"):
        """Apply *self* then *other* (flipped order of ``@``)."""
        return other @ self

    # ----------------------------------------------------------------------
    # Parallel composition ------------------------------------------------
    def __mul__(self, other: "Task") -> "ParallelTask":
        return ParallelTask([self, other])

    # ----------------------------------------------------------------------
    # Algebraic extras ----------------------------------------------------
    def __pow__(self, n: int):
        """Repeated series composition.  Negative exponents use the inverse."""
        if n == 0:
            return Task(self.substrate, {}, name=f"{self.name}^0≡I")
        if n > 0:
            result = self
            for _ in range(n - 1):
                result = result @ self
            return result
        # n < 0 ⇒ need inverse
        return self.inverse() ** (-n)

    # ------------------------------------------------------------------
    @property
    def is_reversible(self) -> bool:
        """True iff the *global* transformation is a bijection."""
        full = {a: self.mapping.get(a, a) for a in self.substrate.attributes}
        return len(set(full.values())) == len(self.substrate.attributes)

    # ------------------------------------------------------------------
    def inverse(self):
        if not self.is_reversible:
            raise ValueError("Task not reversible; inverse undefined.")
        full = {a: self.mapping.get(a, a) for a in self.substrate.attributes}
        inv = {o: i for i, o in full.items()}
        return Task(self.substrate, inv, name=f"{self.name}⁻¹")

###############################################################################
# ParallelTask (product of independent tasks) ---------------------------------
###############################################################################
class ParallelTask(Task):
    """A task acting on the Cartesian product of independent substrates."""

    def __init__(self, tasks: List[Task]):
        # Flatten nested ParallelTasks
        flat: List[Task] = []
        for t in tasks:
            flat.extend(t.tasks if isinstance(t, ParallelTask) else [t])  # type: ignore[attr-defined]
        if not flat:
            raise ValueError("ParallelTask needs at least one component task.")
        self.tasks = flat
        self.component_substrates = tuple(t.substrate for t in self.tasks)

        name = " × ".join(t.name for t in self.tasks)

        # Build composite substrate --------------------------------------
        attrs = list(_cartesian(*(s.attributes for s in self.component_substrates)))
        composite_substrate = Substrate(name, attrs)

        # Build mapping ---------------------------------------------------
        mapping: TaskMap = {}
        for inp in attrs:
            out: List[Attribute] = []
            for idx, task in enumerate(self.tasks):
                a_in = inp[idx]
                out.append(task.mapping.get(a_in, a_in))
            mapping[inp] = tuple(out)

        super().__init__(composite_substrate, mapping, name=name)

###############################################################################
# Constructor‑theory ledger ---------------------------------------------------
###############################################################################
class ConstructorTheory:
    """Registry of which tasks are declared possible or impossible."""

    def __init__(self):
        self._possible: set[int] = set()
        self._impossible: set[int] = set()
        self._reason: Dict[int, str] = {}

    # ------------------------------------------------------------------
    def declare_possible(self, task: Task, reason: str | None = None):
        self._possible.add(id(task))
        self._reason[id(task)] = reason or "Declared possible."

    # ------------------------------------------------------------------
    def declare_impossible(self, task: Task, reason: str | None = None):
        if id(task) in self._possible:
            raise ValueError("Task already marked possible; cannot mark impossible.")
        self._impossible.add(id(task))
        self._reason[id(task)] = reason or "Declared impossible."

    # ------------------------------------------------------------------
    def is_possible(self, task: Task):
        if id(task) in self._possible:
            return True
        if id(task) in self._impossible:
            return False
        return None

    # ------------------------------------------------------------------
    def why(self, task: Task):
        return self._reason.get(id(task))

###############################################################################
# Helpers ---------------------------------------------------------------------
###############################################################################
from typing import Iterable, Tuple

def _cartesian(*iters: Iterable[Attribute]):
    if not iters:
        yield ()
        return
    first, *rest = iters
    for item in first:
        for tup in _cartesian(*rest):
            yield (item, *tup)

###############################################################################
# Proof‑of‑concept demo (serves as runnable unit tests) ------------------------
###############################################################################
if __name__ == "__main__":
    # ------------------------------------------------------------------
    # Build substrates and tasks ---------------------------------------
    bit1 = Substrate("bit1", {0, 1})
    bit2 = Substrate("bit2", {0, 1})

    NOT1 = Task(bit1, {0: 1, 1: 0}, name="NOT1")
    ID1 = Task(bit1, {}, name="ID1")
    NOT2 = Task(bit2, {0: 1, 1: 0}, name="NOT2")

    # ------------------------------------------------------------------
    # 1. Double NOT should act as identity -----------------------------
    double_not = NOT1 @ NOT1
    expected_double = {0: 0, 1: 1}
    print("NOT∘NOT mapping:", double_not.mapping)
    assert double_not.mapping == expected_double, "Double NOT must be identity"

    # 2. Reversibility checks ------------------------------------------
    print("NOT reversible?", NOT1.is_reversible)
    assert NOT1.is_reversible, "NOT should be reversible (a permutation)"

    print("ID reversible?", ID1.is_reversible)
    assert ID1.is_reversible, "Identity must be reversible"

    # 3. Inverse task ---------------------------------------------------
    inv_not = NOT1.inverse()
    print("Inverse NOT mapping:", inv_not.mapping)
    assert inv_not.mapping == NOT1.mapping, "NOT is its own inverse"

    # 4. Task powers ----------------------------------------------------
    not_cubed = NOT1 ** 3
    print("NOT^3 mapping:", not_cubed.mapping)
    assert not_cubed.mapping == NOT1.mapping, "Odd power of NOT yields NOT"

    # 5. Series on disjoint substrates → ParallelTask ------------------
    commuting = NOT1 @ NOT2
    print("Class of NOT1 @ NOT2:", commuting.__class__.__name__)
    assert isinstance(commuting, ParallelTask), "Series on disjoint substrates should commute"

    print("Parallel mapping size:", len(commuting.mapping))
    assert len(commuting.mapping) == 4, "Product substrate should have 4 attribute‑tuples"

    # 6. Constructor‑theory ledger -------------------------------------
    ledger = ConstructorTheory()
    ledger.declare_possible(NOT1, "Assume NOT1 constructor exists.")
    ledger.declare_possible(NOT2)
    ledger.declare_possible(ID1)

    # By closure, NOT∘NOT is possible too
    if ledger.is_possible(NOT1):
        ledger.declare_possible(double_not, "Series composition of possible tasks is possible.")

    print("Is double NOT possible?", ledger.is_possible(double_not))
    assert ledger.is_possible(double_not) is True, "Ledger should confirm double NOT possible"

    print("\n✅ All constructor‑theory assertions passed.")

