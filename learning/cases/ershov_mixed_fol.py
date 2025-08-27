#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — first-order logic edition (finite structures).

What this program shows
-----------------------
Ershov’s *mixed computation* (partial evaluation) lets us pre-compute all work
that depends only on *static* inputs and generate a smaller, faster *residual
program* that awaits only the *dynamic* inputs.

First-order logic example (finite model checking with a free variable):
  - Static: a finite structure 𝔄 with domain D = {0,…,n−1} and fixed
    predicate interpretations (here, a unary predicate Even and a binary
    predicate Edge), and a formula φ(x) with one free variable x.
  - Mixed computation: we *unroll all quantifiers over D* and fold every
    ground atom to True/False *at mix time*, emitting a tiny residual function
    φ_res(x) that depends only on the *dynamic* value x (the element to test).

Concretely, we use the formula:

  φ(x) = (∀y. Edge(x, y) → ∃z. Edge(y, z) ∧ Even(z))
         ∧ (∃y. Edge(y, x) ∧ ¬Even(y))

Intuition:
  - “Every out-neighbor y of x has some even out-neighbor z”, and
  - “x has some in-neighbor y that is odd.”

The program prints three sections:

1) "Answer" — the residual code for φ_res(x) and a sample run,
2) "Reason why" — a concise trace of the quantifier unrolling and constant folding,
3) "Check (harness)" — verification that φ_res matches the generic evaluator
   for all x in the domain.

Run with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import List, Dict, Tuple, Callable, Union

# ---------- Tiny FOL AST (just what we need) ----------

@dataclass(frozen=True)
class Var:
    name: str

@dataclass(frozen=True)
class Pred:
    name: str           # 'Even' (unary) or 'Edge' (binary)
    args: List[Var]

@dataclass(frozen=True)
class Not:
    inner: "Formula"

@dataclass(frozen=True)
class And:
    parts: List["Formula"]

@dataclass(frozen=True)
class Or:
    parts: List["Formula"]

@dataclass(frozen=True)
class Implies:
    a: "Formula"
    b: "Formula"

@dataclass(frozen=True)
class Forall:
    var: Var
    body: "Formula"

@dataclass(frozen=True)
class Exists:
    var: Var
    body: "Formula"

Formula = Union[Pred, Not, And, Or, Implies, Forall, Exists]


# ---------- A fixed, finite structure for our demo ----------

def build_structure() -> Dict:
    """
    Domain D = {0,1,2,3,4}
    Even(i) is True for i ∈ {0,2,4}
    Edge is a small directed graph:
        0→1, 0→2, 1→2, 1→3, 2→3, 3→4, 4→0
    """
    n = 5
    domain = list(range(n))
    EVEN = [i % 2 == 0 for i in domain]
    EDGE = [[False] * n for _ in range(n)]
    for (u, v) in [(0, 1), (0, 2), (1, 2), (1, 3), (2, 3), (3, 4), (4, 0)]:
        EDGE[u][v] = True
    return {"domain": domain, "Even": EVEN, "Edge": EDGE}


# ---------- Generic evaluator (unspecialized semantics) ----------

def eval_pred(struct: Dict, name: str, args: List[int]) -> bool:
    if name == "Even":
        return struct["Even"][args[0]]
    if name == "Edge":
        return struct["Edge"][args[0]][args[1]]
    raise KeyError(name)

def eval_formula(struct: Dict, f: Formula, env: Dict[str, int]) -> bool:
    D = struct["domain"]
    if isinstance(f, Pred):
        args = [env[v.name] for v in f.args]
        return eval_pred(struct, f.name, args)
    if isinstance(f, Not):
        return not eval_formula(struct, f.inner, env)
    if isinstance(f, And):
        return all(eval_formula(struct, p, env) for p in f.parts)
    if isinstance(f, Or):
        return any(eval_formula(struct, p, env) for p in f.parts)
    if isinstance(f, Implies):
        return (not eval_formula(struct, f.a, env)) or eval_formula(struct, f.b, env)
    if isinstance(f, Forall):
        v = f.var.name
        for d in D:
            env2 = dict(env); env2[v] = d
            if not eval_formula(struct, f.body, env2):
                return False
        return True
    if isinstance(f, Exists):
        v = f.var.name
        for d in D:
            env2 = dict(env); env2[v] = d
            if eval_formula(struct, f.body, env2):
                return True
        return False
    raise TypeError("Unknown formula node")


# ---------- Specialization artefact ----------

@dataclass
class SpecializationResult:
    func: Callable[[int], bool]   # residual φ_res(x)
    source: str                   # generated Python source code
    trace: List[str]              # mix-time reasoning


# ---------- The "mixer": unroll quantifiers, fold constants ----------

def specialize_fol(struct: Dict, formula: Formula, free_var: str = "x") -> SpecializationResult:
    D = struct["domain"]
    EVEN = struct["Even"]
    EDGE = struct["Edge"]
    trace: List[str] = []

    # A tiny partial evaluator that returns either a boolean constant or a code string.
    def render(node: Formula, env: Dict[str, Union[int, str]]) -> Tuple[bool, Union[bool, None], str]:
        # Return (is_const, const_value_or_None, code_str)
        if isinstance(node, Pred):
            # Resolve arguments to either int constants or the symbol 'x'
            kinds: List[Tuple[str, Union[int, None]]] = []
            for v in node.args:
                a = env.get(v.name)
                kinds.append(("x", None) if a == "x" else ("const", int(a)))
            if node.name == "Even":
                kind, val = kinds[0]
                if kind == "const":
                    return True, EVEN[val], "True" if EVEN[val] else "False"
                else:
                    return False, None, "EVEN[x]"
            if node.name == "Edge":
                (k0, v0), (k1, v1) = kinds
                if k0 == "const" and k1 == "const":
                    b = EDGE[v0][v1]
                    return True, b, "True" if b else "False"
                if k0 == "x" and k1 == "const":
                    return False, None, f"EDGE[x][{v1}]"
                if k0 == "const" and k1 == "x":
                    return False, None, f"EDGE[{v0}][x]"
                if k0 == "x" and k1 == "x":
                    return False, None, "EDGE[x][x]"
            raise KeyError(node.name)

        if isinstance(node, Not):
            c, val, code = render(node.inner, env)
            if c:
                return True, (not val), "True" if not val else "False"
            return False, None, f"(not {code})"

        if isinstance(node, And):
            dyn: List[str] = []
            for p in node.parts:
                c, val, code = render(p, env)
                if c and not val:
                    return True, False, "False"
                if not c:
                    dyn.append(code)
            if not dyn:
                return True, True, "True"
            return False, None, "(" + " and ".join(dyn) + ")"

        if isinstance(node, Or):
            dyn: List[str] = []
            for p in node.parts:
                c, val, code = render(p, env)
                if c and val:
                    return True, True, "True"
                if not c:
                    dyn.append(code)
            if not dyn:
                return True, False, "False"
            return False, None, "(" + " or ".join(dyn) + ")"

        if isinstance(node, Implies):
            # A → B  ≡  (¬A) ∨ B
            c1, v1, code1 = render(Not(node.a), env)
            c2, v2, code2 = render(node.b, env)
            if c1 and c2:
                val = v1 or v2
                return True, val, "True" if val else "False"
            if c1 and not c2:
                if v1:
                    return True, True, "True"
                return False, None, code2
            if not c1 and c2:
                if v2:
                    return True, True, "True"
                return False, None, code1
            return False, None, f"({code1} or {code2})"

        if isinstance(node, Forall):
            v = node.var.name
            dyn: List[str] = []
            for d in D:
                env2 = dict(env); env2[v] = d
                c, val, code = render(node.body, env2)
                if c and not val:
                    trace.append(f"Forall over {v}: instance {v} = {d} reduced to False, so the whole conjunction is False.")
                    return True, False, "False"
                if c and val:
                    continue  # drop True
                dyn.append(code)
            if not dyn:
                trace.append(f"Forall over {v}: all {len(D)} instances were True at mix time.")
                return True, True, "True"
            trace.append(f"Forall over {v}: emitted conjunction of {len(dyn)} dynamic clauses; {len(D) - len(dyn)} instances folded to True.")
            return False, None, "(" + " and ".join(dyn) + ")"

        if isinstance(node, Exists):
            v = node.var.name
            dyn: List[str] = []
            for d in D:
                env2 = dict(env); env2[v] = d
                c, val, code = render(node.body, env2)
                if c and val:
                    trace.append(f"Exists over {v}: instance {v} = {d} is True at mix time, so the whole disjunction is True.")
                    return True, True, "True"
                if c and not val:
                    continue  # drop False
                dyn.append(code)
            if not dyn:
                trace.append(f"Exists over {v}: all {len(D)} instances were False at mix time.")
                return True, False, "False"
            trace.append(f"Exists over {v}: emitted disjunction of {len(dyn)} dynamic clauses; {len(D) - len(dyn)} instances folded to False.")
            return False, None, "(" + " or ".join(dyn) + ")"

        raise TypeError("Unknown node in render")

    # Some structure-level context for the trace
    outdeg = [sum(1 for z in D if EDGE[u][z]) for u in D]
    trace.append("We treat the finite structure and the formula as static, and the value of x as dynamic.")
    trace.append(f"Domain size = {len(D)} with elements 0..{len(D)-1}")
    trace.append(f"Even as 0/1 flags = {[1 if b else 0 for b in EVEN]}")
    trace.append(f"Edge out-degrees per node = {outdeg}")
    trace.append("At mix time we unroll all quantifiers over the domain and fold ground atoms to constants.")

    # Start partial evaluation with the environment where x is dynamic
    is_const, const_val, expr_code = render(formula, {free_var: "x"})
    final_expr = "True" if (is_const and const_val) else ("False" if (is_const and not const_val) else expr_code)

    # Emit residual code with baked-in arrays
    n = len(D)
    even_list = ", ".join("True" if v else "False" for v in EVEN)
    edge_rows = []
    for i in D:
        row = ", ".join("True" if EDGE[i][j] else "False" for j in D)
        edge_rows.append("[" + row + "]")
    edge_literal = "[" + ", ".join(edge_rows) + "]"

    lines: List[str] = []
    lines.append("def phi(x):")
    lines.append('    """Residual program: decide φ(x) over a fixed finite structure."""')
    lines.append(f"    N = {n}")
    lines.append("    if not (0 <= x < N):")
    lines.append(f"        raise ValueError('x not in domain 0..{n-1}')")
    lines.append(f"    EVEN = [{even_list}]")
    lines.append(f"    EDGE = {edge_literal}")
    lines.append(f"    return {final_expr}")
    source = "\n".join(lines)

    # Materialize the function from the generated source
    namespace: Dict = {}
    exec(source, namespace)  # safe: code generated locally from fixed data
    residual_func = namespace["phi"]

    return SpecializationResult(func=residual_func, source=source, trace=trace)


# ---------- Pretty-printers for the three requested sections ----------

def print_answer(spec: SpecializationResult, x_example: int) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    value = spec.func(x_example)
    print(f"Example evaluation: for x = {x_example}, φ_res(x) = {value}")
    print()

def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()

def print_check(spec: SpecializationResult, struct: Dict, formula: Formula) -> None:
    print("Check (harness)")
    print("----------------")
    D = struct["domain"]
    failures: List[Tuple[int, bool, bool]] = []
    for x in D:
        want = eval_formula(struct, formula, {"x": x})
        got = spec.func(x)
        if want != got:
            failures.append((x, want, got))
    if not failures:
        print(f"PASS: residual φ_res agrees with the generic evaluator for all {len(D)} elements of the domain.")
    else:
        print(f"FAIL: {len(failures)} mismatches:")
        for (x, want, got) in failures:
            print(f"  x = {x}: expected {want}, got {got}")
    print()


# ---------- Main demo ----------

def main() -> None:
    # Static structure
    struct = build_structure()

    # Static formula φ(x):
    x = Var("x"); y = Var("y"); z = Var("z")
    phi = And([
        Forall(y, Implies(Pred("Edge", [x, y]),
                          Exists(z, And([Pred("Edge", [y, z]), Pred("Even", [z])])))),
        Exists(y, And([Pred("Edge", [y, x]), Not(Pred("Even", [y]))]))
    ])

    # Build residual program specialized to the structure and φ
    spec = specialize_fol(struct, phi, free_var="x")

    # 1) Show the residual code and a sample evaluation
    example_x = 2
    print_answer(spec, example_x)

    # 2) Explain the mix-time decisions
    print_reason(spec)

    # 3) Verify equivalence vs. the generic evaluator
    print_check(spec, struct, phi)


if __name__ == "__main__":
    main()

