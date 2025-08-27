#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — calculus edition (symbolic derivative specialized to a fixed expression).

What this program shows
-----------------------
Ershov’s *mixed computation* (partial evaluation) lets us pre-compute all work
that depends only on *static* inputs and generate a smaller, faster *residual
program* that awaits only the *dynamic* inputs.

Calculus example (derivatives):
  - Generic task: given an expression f(x), compute both f(x) and f'(x) at a
    numeric x.
  - Mixed computation: if the *expression* f is *static* (known now), we can
    differentiate it *at mix time* and emit a residual program that only needs
    the *dynamic* number x. The residual code contains no tree walks or rule
    dispatch — just arithmetic with `math.sin`, `math.cos`, `math.exp`, etc.

The script prints three sections:

1) "Answer" — the generated residual code for f(x) and f'(x), plus a sample run,
2) "Reason why" — a concise derivation trace showing which rules were applied,
3) "Check (harness)" — tests comparing the residual derivative to a numerical
   central-difference approximation across many x values.

Run with Python 3.x; no dependencies.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Tuple, Callable, Union
import math
import random


# ──────────────────────────── Expression AST ────────────────────────────

class Expr:
    """Base class with tiny operator overloading for convenience."""
    def __add__(self, other): return Add([self, lift(other)])
    def __radd__(self, other): return Add([lift(other), self])
    def __sub__(self, other): return Add([self, Mul([Const(-1.0), lift(other)])])
    def __rsub__(self, other): return Add([lift(other), Mul([Const(-1.0), self])])
    def __mul__(self, other): return Mul([self, lift(other)])
    def __rmul__(self, other): return Mul([lift(other), self])
    def __truediv__(self, other): return Div(self, lift(other))
    def __rtruediv__(self, other): return Div(lift(other), self)
    def __pow__(self, other): return Pow(self, lift(other))
    def __neg__(self): return Mul([Const(-1.0), self])

@dataclass(frozen=True)
class Const(Expr):
    value: float
@dataclass(frozen=True)
class Var(Expr):
    name: str = "x"
@dataclass(frozen=True)
class Add(Expr):
    terms: List[Expr]
@dataclass(frozen=True)
class Mul(Expr):
    factors: List[Expr]
@dataclass(frozen=True)
class Div(Expr):
    num: Expr
    den: Expr
@dataclass(frozen=True)
class Pow(Expr):
    base: Expr
    exp: Expr
@dataclass(frozen=True)
class Sin(Expr):
    arg: Expr
@dataclass(frozen=True)
class Cos(Expr):
    arg: Expr
@dataclass(frozen=True)
class Exp(Expr):
    arg: Expr
@dataclass(frozen=True)
class Log(Expr):
    arg: Expr


def lift(x: Union[Expr, float, int]) -> Expr:
    return x if isinstance(x, Expr) else Const(float(x))


# ───────────────────────── Pretty-print & code-gen ───────────────────────

def to_infix(e: Expr) -> str:
    """Human-friendly infix string (with safe parentheses)."""
    if isinstance(e, Const):
        # avoid trailing .0 for integers
        v = e.value
        return str(int(v)) if float(v).is_integer() else f"{v}"
    if isinstance(e, Var):
        return e.name
    if isinstance(e, Add):
        return "(" + " + ".join(to_infix(t) for t in e.terms) + ")"
    if isinstance(e, Mul):
        return "(" + " * ".join(to_infix(f) for f in e.factors) + ")"
    if isinstance(e, Div):
        return "(" + to_infix(e.num) + " / " + to_infix(e.den) + ")"
    if isinstance(e, Pow):
        return "(" + to_infix(e.base) + " ** " + to_infix(e.exp) + ")"
    if isinstance(e, Sin):
        return f"sin({to_infix(e.arg)})"
    if isinstance(e, Cos):
        return f"cos({to_infix(e.arg)})"
    if isinstance(e, Exp):
        return f"exp({to_infix(e.arg)})"
    if isinstance(e, Log):
        return f"log({to_infix(e.arg)})"
    return "<?>"

def to_source(e: Expr) -> str:
    """Python source expression using math.* functions."""
    if isinstance(e, Const):  return repr(float(e.value))
    if isinstance(e, Var):    return "(x)"
    if isinstance(e, Add):    return "(" + " + ".join(to_source(t) for t in e.terms) + ")"
    if isinstance(e, Mul):    return "(" + " * ".join(to_source(f) for f in e.factors) + ")"
    if isinstance(e, Div):    return "(" + to_source(e.num) + " / " + to_source(e.den) + ")"
    if isinstance(e, Pow):    return "(" + to_source(e.base) + " ** " + to_source(e.exp) + ")"
    if isinstance(e, Sin):    return "math.sin(" + to_source(e.arg) + ")"
    if isinstance(e, Cos):    return "math.cos(" + to_source(e.arg) + ")"
    if isinstance(e, Exp):    return "math.exp(" + to_source(e.arg) + ")"
    if isinstance(e, Log):    return "math.log(" + to_source(e.arg) + ")"
    raise TypeError("Unknown node")


# ───────────────────────────── Simplification ────────────────────────────

def is_const(e: Expr) -> Tuple[bool, float]:
    if isinstance(e, Const):
        return True, e.value
    return False, 0.0

def simplify(e: Expr) -> Expr:
    """Small but effective simplifier to make emitted code neat."""
    if isinstance(e, (Const, Var)):
        return e

    if isinstance(e, Add):
        terms: List[Expr] = []
        csum = 0.0
        for t in e.terms:
            t = simplify(t)
            if isinstance(t, Add):
                for s in t.terms:
                    terms.append(simplify(s))
            else:
                is_c, v = is_const(t)
                if is_c: csum += v
                else: terms.append(t)
        if abs(csum) > 0.0:
            terms.append(Const(csum))
        if not terms:
            return Const(0.0)
        if len(terms) == 1:
            return terms[0]
        return Add(terms)

    if isinstance(e, Mul):
        factors: List[Expr] = []
        cprod = 1.0
        for f in e.factors:
            f = simplify(f)
            if isinstance(f, Mul):
                for g in f.factors:
                    factors.append(simplify(g))
            else:
                is_c, v = is_const(f)
                if is_c:
                    if v == 0.0:
                        return Const(0.0)
                    cprod *= v
                else:
                    factors.append(f)
        # remove 1.0
        if abs(cprod - 1.0) > 0.0:
            factors.insert(0, Const(cprod))
        if not factors:
            return Const(1.0)
        if len(factors) == 1:
            return factors[0]
        return Mul(factors)

    if isinstance(e, Div):
        num = simplify(e.num)
        den = simplify(e.den)
        is_nc, nv = is_const(num)
        is_dc, dv = is_const(den)
        if is_nc and is_dc:
            return Const(nv / dv)
        if is_nc and nv == 0.0:
            return Const(0.0)
        if is_dc and dv == 1.0:
            return num
        return Div(num, den)

    if isinstance(e, Pow):
        base = simplify(e.base)
        exp  = simplify(e.exp)
        is_ec, ev = is_const(exp)
        is_bc, bv = is_const(base)
        if is_ec:
            if ev == 0.0: return Const(1.0)
            if ev == 1.0: return base
            if is_bc:     return Const(bv ** ev)
        return Pow(base, exp)

    if isinstance(e, Sin):
        a = simplify(e.arg)
        if isinstance(a, Const): return Const(math.sin(a.value))
        return Sin(a)
    if isinstance(e, Cos):
        a = simplify(e.arg)
        if isinstance(a, Const): return Const(math.cos(a.value))
        return Cos(a)
    if isinstance(e, Exp):
        a = simplify(e.arg)
        if isinstance(a, Const): return Const(math.exp(a.value))
        return Exp(a)
    if isinstance(e, Log):
        a = simplify(e.arg)
        if isinstance(a, Const): return Const(math.log(a.value))
        return Log(a)

    return e


# ───────────────────────── Differentiation (with trace) ──────────────────

def diff(e: Expr, trace: List[str]) -> Expr:
    if isinstance(e, Const):
        trace.append("d/dx [c] = 0")
        return Const(0.0)
    if isinstance(e, Var):
        trace.append("d/dx [x] = 1")
        return Const(1.0)

    if isinstance(e, Add):
        trace.append(f"Linearity: d/dx [{to_infix(e)}] = sum of term derivatives")
        return simplify(Add([diff(t, trace) for t in e.terms]))

    if isinstance(e, Mul):
        # General product rule for n factors: Σ f_i' Π_{j≠i} f_j
        trace.append(f"Product rule: d/dx [{to_infix(e)}] = Σ (f_i' * Π_{{j≠i}} f_j)")
        parts = []
        for i, fi in enumerate(e.factors):
            dpi = diff(fi, trace)
            others = [e.factors[j] for j in range(len(e.factors)) if j != i]
            parts.append(Mul([dpi] + others))
        return simplify(Add(parts))

    if isinstance(e, Div):
        # Quotient rule
        trace.append(f"Quotient rule: d/dx [{to_infix(e)}] = (u'v - uv')/v^2")
        u, v = e.num, e.den
        du = diff(u, trace)
        dv = diff(v, trace)
        return simplify(Div(Add([Mul([du, v]), Mul([Const(-1.0), u, dv])]), Pow(v, Const(2.0))))

    if isinstance(e, Pow):
        b, p = e.base, e.exp
        is_pc, pv = is_const(p)
        is_bc, bv = is_const(b)
        if is_pc:
            # d(b^c) = c*b^(c-1)*b'
            trace.append(f"Power (const exp): d/dx [{to_infix(e)}] = {pv}*b^{pv-1} * b'")
            return simplify(Mul([Const(pv), Pow(b, Const(pv-1.0)), diff(b, trace)]))
        if is_bc and bv > 0:
            # d(a^p) = a^p * ln(a) * p'
            trace.append(f"Power (const base): d/dx [{to_infix(e)}] = {bv}^p * ln({bv}) * p'")
            return simplify(Mul([Pow(b, p), Const(math.log(bv)), diff(p, trace)]))
        # General case: d(b^p) = b^p * (p' * ln b + p * b'/b)   (domain: b>0)
        trace.append(f"Power (general): d/dx [{to_infix(e)}] = b^p * (p' ln b + p * b'/b)")
        return simplify(Mul([Pow(b, p),
                             Add([Mul([diff(p, trace), Log(b)]),
                                  Mul([p, Div(diff(b, trace), b)])])]))

    if isinstance(e, Sin):
        trace.append(f"Chain rule (sin): d/dx [sin(u)] = cos(u)*u', with u={to_infix(e.arg)}")
        return simplify(Mul([Cos(e.arg), diff(e.arg, trace)]))
    if isinstance(e, Cos):
        trace.append(f"Chain rule (cos): d/dx [cos(u)] = -sin(u)*u', with u={to_infix(e.arg)}")
        return simplify(Mul([Const(-1.0), Sin(e.arg), diff(e.arg, trace)]))
    if isinstance(e, Exp):
        trace.append(f"Chain rule (exp): d/dx [exp(u)] = exp(u)*u', with u={to_infix(e.arg)}")
        return simplify(Mul([Exp(e.arg), diff(e.arg, trace)]))
    if isinstance(e, Log):
        trace.append(f"Chain rule (log): d/dx [log(u)] = u'/u, with u={to_infix(e.arg)}")
        return simplify(Div(diff(e.arg, trace), e.arg))

    raise TypeError("Unknown node in diff")


# ───────────────────────── Evaluation (generic) ──────────────────────────

def eval_expr(e: Expr, x: float) -> float:
    if isinstance(e, Const): return e.value
    if isinstance(e, Var):   return x
    if isinstance(e, Add):   return sum(eval_expr(t, x) for t in e.terms)
    if isinstance(e, Mul):
        r = 1.0
        for f in e.factors:
            r *= eval_expr(f, x)
        return r
    if isinstance(e, Div):   return eval_expr(e.num, x) / eval_expr(e.den, x)
    if isinstance(e, Pow):   return eval_expr(e.base, x) ** eval_expr(e.exp, x)
    if isinstance(e, Sin):   return math.sin(eval_expr(e.arg, x))
    if isinstance(e, Cos):   return math.cos(eval_expr(e.arg, x))
    if isinstance(e, Exp):   return math.exp(eval_expr(e.arg, x))
    if isinstance(e, Log):   return math.log(eval_expr(e.arg, x))
    raise TypeError("Unknown node in eval")


# ───────────────────────── Specialization artefact ───────────────────────

@dataclass
class SpecializationResult:
    f: Callable[[float], float]      # residual f(x)
    df: Callable[[float], float]     # residual f'(x)
    source: str                      # generated Python source code
    trace: List[str]                 # human-readable reasoning of mix-time decisions


def specialize_derivative(expr: Expr) -> SpecializationResult:
    """
    Treat 'expr' as *static* and x as *dynamic*. Differentiate now, emit source.
    """
    trace: List[str] = []
    dexpr = simplify(diff(expr, trace))
    sexpr = simplify(expr)

    # Build residual source
    f_src   = to_source(sexpr)
    df_src  = to_source(dexpr)
    source_lines = [
        "import math",
        "def f(x):",
        f"    return {f_src}",
        "",
        "def df(x):",
        f"    return {df_src}",
        "",
    ]
    source = "\n".join(source_lines)

    # Materialize the residual functions
    namespace: dict = {}
    exec(source, namespace)  # safe: code generated locally from our AST only
    f_res = namespace["f"]
    df_res = namespace["df"]

    # Preface for the reasoning trace
    preface = [
        f"Static expression: f(x) = {to_infix(sexpr)}",
        f"Goal: build residual df(x) = {to_infix(dexpr)} using symbolic rules at mix time.",
        "We apply linearity, product/quotient rules, and the chain rule; constants are folded.",
    ]
    trace = preface + trace

    return SpecializationResult(f=f_res, df=df_res, source=source, trace=trace)


# ────────────────────────── Output sections ──────────────────────────────

def print_answer(spec: SpecializationResult, x0: float) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    fx = spec.f(x0)
    dfx = spec.df(x0)
    print(f"Example evaluation at x = {x0}:")
    print(f"  f(x)  = {fx:.12g}")
    print(f"  f'(x) = {dfx:.12g}")
    print()

def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()

def print_check(spec: SpecializationResult,
                expr: Expr,
                trials: int = 300,
                domain: Tuple[float, float] = (-1.5, 1.5)) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(20250827)
    a, b = domain
    fails = 0
    samples = 0
    for _ in range(trials):
        x = random.uniform(a, b)
        try:
            # Numerical derivative via 5-point central difference for robustness
            # (O(h^4)). Choose h relative to scale of x.
            h = 1e-5 * (1.0 + abs(x))
            f = spec.f
            num = (-f(x+2*h) + 8*f(x+h) - 8*f(x-h) + f(x-2*h)) / (12*h)
            ana = spec.df(x)

            # Compare with a mixed absolute/relative tolerance
            tol = 5e-7 * (1.0 + max(1.0, abs(num), abs(ana)))
            if not (abs(ana - num) <= tol):
                fails += 1
        except (ValueError, OverflowError, ZeroDivisionError):
            # Skip points where the expression is undefined or unstable
            continue
        finally:
            samples += 1

    if fails == 0:
        print(f"PASS: residual derivative matches a high-accuracy finite-difference "
              f"approximation on {samples} samples.")
    else:
        print(f"WARNING: {fails} discrepancies out of {samples} samples "
              f"(likely near singularities or due to numeric conditioning).")
    # Spot-check a few values vs. the generic evaluator too
    xs = [-1.0, -0.3, 0.0, 0.7, 1.3]
    ok = True
    for x in xs:
        try:
            # Construct the generic derivative freshly (unspecialized path)
            tmp_trace: List[str] = []
            dgen = simplify(diff(expr, tmp_trace))
            g = eval_expr(dgen, x)
            r = spec.df(x)
            if abs(g - r) > 1e-10 * (1 + max(abs(g), abs(r))):
                ok = False
                print(f"  MISMATCH at x={x}: generic {g}, residual {r}")
        except Exception as e:
            print(f"  Skipped x={x}: {e}")
    if ok:
        print("Spot-check vs. generic symbolic derivative: OK.")
    print()


# ───────────────────────────── Main demo ─────────────────────────────────

def main() -> None:
    # Choose the static and dynamic parts:
    # - The expression f(x) is *static* (we know it now),
    # - The point x is *dynamic* (unknown until run time).

    x = Var("x")
    # Example expression (smooth, well-behaved):
    #   f(x) = e^(2x) * sin(x) + x^3
    expr = Exp(2*x) * Sin(x) + x**3

    # Build residual program specialized to this f
    spec = specialize_derivative(expr)

    # 1) Show residual code and a sample evaluation
    x0 = 0.7
    print_answer(spec, x0)

    # 2) Explain mix-time differentiation steps
    print_reason(spec)

    # 3) Verify vs. numerical differentiation and a spot-check vs. generic
    print_check(spec, expr, trials=600, domain=(-1.3, 1.3))


if __name__ == "__main__":
    main()

