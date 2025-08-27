#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ershov's mixed computation — geometry edition (point-in-triangle).

What this program shows
-----------------------
Ershov’s *mixed computation* (partial evaluation) lets us pre-compute all work
that depends only on *static* inputs and generate a smaller, faster *residual
program* that awaits only the *dynamic* inputs.

Geometry example:
  - Generic program: `point_in_triangle_plain(p, a, b, c)` returns whether
    point p lies in triangle (a,b,c) using barycentric coordinates.
  - Mixed computation: If the triangle (a,b,c) is *static* (known now),
    partially evaluate the generic program to produce a residual function
    `inside_T(x, y)` of only the *dynamic* point p=(x,y).
    The residual code has no branching on triangle data; it is a few affine
    arithmetic operations with precomputed constants.

The program prints three sections:

1) "Answer" — the generated residual code and a sample evaluation,
2) "Reason why" — a concise step-by-step justification of what was decided
   at mix time,
3) "Check (harness)" — a test harness that verifies the residual program
   matches the original for many points (including edge cases).

Run with Python 3.x; no dependencies.
"""

from dataclasses import dataclass
from typing import Callable, List, Tuple
import random
import math


# ---------- Numeric guard ----------
EPS = 1e-12  # small tolerance to make inclusive boundary checks robust


# ---------- Original (unspecialized) program ----------

def point_in_triangle_plain(x: float, y: float,
                            ax: float, ay: float,
                            bx: float, by: float,
                            cx: float, cy: float,
                            inclusive: bool = True) -> bool:
    """
    Generic point-in-triangle via barycentric coordinates.

    Let v0 = b - a, v1 = c - a. Solve for (u, v) in:
        [v0 v1] [u, v]^T = (p - a)
    with det = v0x*v1y - v1x*v0y != 0 (non-degenerate triangle).
    Inside test:
        inclusive:   u >= 0, v >= 0, u + v <= 1
        exclusive:   u >  0, v >  0, u + v <  1
    """
    v0x, v0y = bx - ax, by - ay
    v1x, v1y = cx - ax, cy - ay
    det = v0x * v1y - v1x * v0y
    if abs(det) < EPS:
        raise ValueError("Degenerate triangle (area ~ 0).")

    # Inverse of [[v0x, v1x],[v0y,v1y]]:
    inv00 =  v1y / det
    inv01 = -v1x / det
    inv10 = -v0y / det
    inv11 =  v0x / det

    px, py = x - ax, y - ay
    u = inv00 * px + inv01 * py
    v = inv10 * px + inv11 * py
    s = u + v

    if inclusive:
        return (u >= -EPS) and (v >= -EPS) and (s <= 1 + EPS)
    else:
        tol = EPS if det > 0 else EPS  # symmetrical; here just keep EPS
        return (u > tol) and (v > tol) and (s < 1 - tol)


# ---------- Specialization result container ----------

@dataclass
class SpecializationResult:
    func: Callable[[float, float], bool]  # residual: takes only (x, y)
    source: str                           # generated Python source
    trace: List[str]                      # human-readable mix-time reasoning


# ---------- The "mixer": partially evaluate w.r.t. the static triangle ----------

def specialize_point_in_triangle(a: Tuple[float, float],
                                 b: Tuple[float, float],
                                 c: Tuple[float, float],
                                 inclusive: bool = True) -> SpecializationResult:
    """
    Given static vertices a,b,c, generate residual function inside_T(x,y) that
    returns whether p=(x,y) lies inside triangle (a,b,c) under the chosen
    inclusive/exclusive rule.

    Mixed computation: precompute det and the inverse matrix entries; emit the
    affine arithmetic for (u,v) and the simple inequalities.
    """
    ax, ay = a
    bx, by = b
    cx, cy = c

    v0x, v0y = bx - ax, by - ay
    v1x, v1y = cx - ax, cy - ay
    det = v0x * v1y - v1x * v0y
    if abs(det) < EPS:
        raise ValueError("Degenerate triangle (area ~ 0).")

    inv00 =  v1y / det
    inv01 = -v1x / det
    inv10 = -v0y / det
    inv11 =  v0x / det

    lines: List[str] = []
    trace: List[str] = []

    func_name = "inside_T"
    incl_flag = "True" if inclusive else "False"

    lines.append(f"def {func_name}(x, y):")
    lines.append(f"    \"\"\"Residual program: point ∈ triangle? (inclusive={incl_flag})\"\"\"")
    # Embed constants (triangle and inverse matrix):
    lines.append(f"    ax, ay = {ax:.16g}, {ay:.16g}")
    lines.append(f"    inv00, inv01 = {inv00:.16g}, {inv01:.16g}")
    lines.append(f"    inv10, inv11 = {inv10:.16g}, {inv11:.16g}")
    lines.append(f"    # Compute barycentric (u,v) for p - a")
    lines.append(f"    px = x - ax"); lines.append(f"    py = y - ay")
    lines.append(f"    u = inv00*px + inv01*py")
    lines.append(f"    v = inv10*px + inv11*py")
    lines.append(f"    s = u + v")
    if inclusive:
        lines.append(f"    return (u >= {-EPS:.16g}) and (v >= {-EPS:.16g}) and (s <= {1+EPS:.16g})")
    else:
        lines.append(f"    return (u > {EPS:.16g}) and (v > {EPS:.16g}) and (s < {1-EPS:.16g})")

    source = "\n".join(lines)

    # Materialize function
    namespace: dict = {}
    exec(source, namespace)  # Safe: code generated locally from numeric constants
    residual_func = namespace[func_name]

    # Build reasoning trace
    area2 = det  # signed double area
    orientation = "counter-clockwise" if area2 > 0 else "clockwise"
    preface = [
        "We treat the triangle vertices (a, b, c) as *static* and the query point p=(x,y) as *dynamic*.",
        "At mix time we compute the inverse of the 2×2 matrix [v0 v1] with columns v0=b-a and v1=c-a.",
        "This lets the residual program compute barycentric (u, v) via two affine forms and test simple inequalities.",
    ]
    trace.extend(preface)
    trace.append(f"a = ({ax:.6g}, {ay:.6g}), b = ({bx:.6g}, {by:.6g}), c = ({cx:.6g}, {cy:.6g})")
    trace.append(f"v0 = b-a = ({v0x:.6g}, {v0y:.6g}), v1 = c-a = ({v1x:.6g}, {v1y:.6g})")
    trace.append(f"det = v0x*v1y - v1x*v0y = {det:.6g}  (triangle is {orientation}; |area| = {abs(det)/2:.6g})")
    trace.append("Inverse([v0 v1]) = (1/det) * [[ v1y, -v1x], [-v0y,  v0x]]")
    trace.append(f"→ inv00={inv00:.8g}, inv01={inv01:.8g}, inv10={inv10:.8g}, inv11={inv11:.8g}")
    if inclusive:
        trace.append("Inside test (inclusive): u ≥ 0, v ≥ 0, u+v ≤ 1 (with small EPS tolerance).")
    else:
        trace.append("Inside test (exclusive): u > 0, v > 0, u+v < 1 (with small EPS tolerance).")

    return SpecializationResult(func=residual_func, source=source, trace=trace)


# ---------- Pretty-printers for the three requested sections ----------

def print_answer(spec: SpecializationResult, example_xy: Tuple[float, float]) -> None:
    print("Answer")
    print("------")
    print("Residual program (generated at mix time):")
    print(spec.source)
    x, y = example_xy
    value = spec.func(x, y)
    print(f"Example evaluation: for p = ({x}, {y}), inside_T(p) = {value}")
    print()


def print_reason(spec: SpecializationResult) -> None:
    print("Reason why")
    print("----------")
    for line in spec.trace:
        print("•", line)
    print()


def print_check(spec: SpecializationResult,
                a: Tuple[float, float],
                b: Tuple[float, float],
                c: Tuple[float, float],
                inclusive: bool,
                trials: int = 600) -> None:
    print("Check (harness)")
    print("----------------")
    random.seed(2025)

    ax, ay = a; bx, by = b; cx, cy = c

    # Bounding box with margin
    minx = min(ax, bx, cx) - 1.0
    maxx = max(ax, bx, cx) + 1.0
    miny = min(ay, by, cy) - 1.0
    maxy = max(ay, by, cy) + 1.0

    # Include deterministic edge/vertex probes + random points
    samples: List[Tuple[float, float]] = []
    # vertices
    samples += [(ax, ay), (bx, by), (cx, cy)]
    # edge midpoints
    samples += [((ax+bx)/2, (ay+by)/2), ((bx+cx)/2, (by+cy)/2), ((cx+ax)/2, (cy+ay)/2)]
    # a handful of interior points by convex combos
    samples += [(0.2*ax+0.3*bx+0.5*cx, 0.2*ay+0.3*by+0.5*cy),
                (0.33*ax+0.33*bx+0.34*cx, 0.33*ay+0.33*by+0.34*cy)]
    # randoms
    for _ in range(trials):
        x = random.uniform(minx, maxx)
        y = random.uniform(miny, maxy)
        samples.append((x, y))

    failures: List[Tuple[float, float, bool, bool]] = []
    for (x, y) in samples:
        want = point_in_triangle_plain(x, y, ax, ay, bx, by, cx, cy, inclusive=inclusive)
        got = spec.func(x, y)
        if want != got:
            failures.append((x, y, want, got))

    if not failures:
        print(f"PASS: residual program agrees with the generic test "
              f"on {len(samples)} points (including edges and randoms).")
    else:
        print(f"FAIL: {len(failures)} mismatches found. Showing up to 10:")
        for (x, y, want, got) in failures[:10]:
            print(f"  p=({x:.6g},{y:.6g}): expected {want}, got {got}")
    print()


# ---------- Main demo ----------

def main() -> None:
    # Choose the static and dynamic parts:
    # - Triangle (a,b,c) is *static* (known now),
    # - Query point p=(x,y) is *dynamic* (unknown until run time).
    a = (0.0, 0.0)
    b = (3.0, 0.0)
    c = (1.0, 2.0)
    inclusive = True  # Change to False to use strict interior

    # Build residual program for this specific triangle
    spec = specialize_point_in_triangle(a, b, c, inclusive=inclusive)

    # 1) Show the residual code and a sample evaluation
    example_p = (1.0, 1.0)  # inside this triangle
    print_answer(spec, example_p)

    # 2) Explain the mix-time decisions
    print_reason(spec)

    # 3) Verify equivalence vs. the generic implementation
    print_check(spec, a, b, c, inclusive=inclusive, trials=800)


if __name__ == "__main__":
    main()

