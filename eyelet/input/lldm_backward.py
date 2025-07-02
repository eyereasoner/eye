"""
lldm_backward.py – verbose “proof trace” for the leg-length
discrepancy algorithm (Medicad demo).

Given four landmark points p1…p4 it emits a 30-step explanation
showing *every* equation, substituted numbers, intermediate result and
unit.

Input landmarks (cm)
Point      x       y    Description
------------------------------------------------------------------
p1      10.1     7.8    right-hip reference on pelvic line L1
p2      45.1     5.6    left-hip reference on pelvic line L1
p3       3.6    29.8    right-shoulder landmark on femoral line L3
p4      54.7    28.5    left-shoulder landmark on femoral line L3

Usage (demo values from the original N3 file are in __main__):
    python lldm_backward.py
"""

#!/usr/bin/env python3
# leg_length_trace.py
#
# Reproduces the verbose 30-step proof-style output for the leg-length
# discrepancy calculation, byte-for-byte (except for normal whitespace).

from math import sqrt

# ────────────────────────── helpers ──────────────────────────
EN_DASH = "–"        # U+2013 so the minus sign matches your sample

def f(num, width=7, dec=4):
    """Format with + / en-dash sign, fixed decimals, right-aligned."""
    sgn = "+" if num >= 0 else EN_DASH
    return f"{sgn}{abs(num):.{dec}f}".rjust(width + dec + 2)

def line1(step, lhs, rhs):
    print(f"Step {step:02d}  {lhs} = {rhs}")

def line_cont(expr):
    print(f"        = {expr}")

def sep():
    print("─" * 76)

# ────────────────────────── main routine ─────────────────────
def trace(p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y):
    step = 1

    # Basic deltas ---------------------------------------------------
    dx12 = p1x - p2x
    dy12 = p1y - p2y
    dy13 = p1y - p3y
    dy24 = p2y - p4y

    line1(step, "dx12Cm", "p1x – p2x"); line_cont(f"{p1x} – {p2x}")
    line_cont(f"{f(dx12)} cm            # horizontal span of pelvic baseline L1")
    step += 1

    line1(step, "dy12Cm", "p1y – p2y"); line_cont(f"{p1y} – {p2y}")
    line_cont(f"{f(dy12)} cm             # vertical drop of L1 from right→left hip")
    step += 1

    line1(step, "dy13Cm", "p1y – p3y"); line_cont(f"{p1y} – {p3y}")
    line_cont(f"{f(dy13)} cm            # vertical offset from p1 down to p3")
    step += 1

    line1(step, "dy24Cm", "p2y – p4y"); line_cont(f"{p2y} – {p4y}")
    line_cont(f"{f(dy24)} cm            # vertical offset from p2 down to p4")
    step += 1
    sep()

    # Slopes ---------------------------------------------------------
    cL1  = dy12 / dx12
    dL3m = -1 / cL1
    cL3  = -dL3m

    line1(step, "cL1 ", "slope of L1 = dy12 / dx12")
    line_cont(f"{f(dy12,dec=4)} / {f(dx12,dec=4)}")
    line_cont(f"{f(cL1,dec=6)}            # small negative slope, pelvis tilted ⇘")
    step += 1

    line1(step, "dL3m", EN_DASH + "1 / cL1")
    line_cont(f"{EN_DASH}1 / {f(cL1,dec=6)}")
    line_cont(f"{f(dL3m,dec=4)}             # perpendicular slope to L1 (negative reciprocal)")
    step += 1

    line1(step, "cL3 ", EN_DASH + "dL3m")
    line_cont(f"{EN_DASH}({f(dL3m,dec=4)})")
    line_cont(f"{f(cL3,dec=4)}             # we invert sign so L3 runs upward (steep ⇗)")
    step += 1
    sep()

    # m·x terms ------------------------------------------------------
    pL1x1 = cL1 * p1x
    pL1x2 = cL1 * p2x
    pL3x3 = cL3 * p3x
    pL3x4 = cL3 * p4x

    line1(step, "pL1x1", "cL1 · p1x"); line_cont(f"{f(cL1,dec=6)} · {p1x}")
    line_cont(f"{f(pL1x1)} cm")
    step += 1

    line1(step, "pL1x2", "cL1 · p2x"); line_cont(f"{f(cL1,dec=6)} · {p2x}")
    line_cont(f"{f(pL1x2)} cm            # both 8-9 are “m x” items used like intercepts")
    step += 1

    line1(step, "pL3x3", "cL3 · p3x"); line_cont(f"{f(cL3,dec=4)} · {p3x}")
    line_cont(f"{f(pL3x3)} cm")
    step += 1

    line1(step, "pL3x4", "cL3 · p4x"); line_cont(f"{f(cL3,dec=4)} · {p4x}")
    line_cont(f"{f(pL3x4)} cm")
    step += 1
    sep()

    # Diagonal deltas ------------------------------------------------
    dd13  = pL1x1 - pL3x3
    ddy13 = dd13 - dy13
    dd24  = pL1x2 - pL3x4
    ddy24 = dd24 - dy24

    line1(step, "dd13Cm", "pL1x1 – pL3x3"); line_cont(f"{f(pL1x1)} – {f(pL3x3)}")
    line_cont(f"{f(dd13)} cm          # Δ of “m x” terms for the (1,3) diagonal")
    step += 1

    line1(step, "ddy13Cm", "dd13Cm – dy13Cm"); line_cont(f"{f(dd13)} – ({f(dy13)})")
    line_cont(f"{f(ddy13)} cm          # vertical Δ to project p3 ↔ L1")
    step += 1

    line1(step, "dd24Cm", "pL1x2 – pL3x4"); line_cont(f"{f(pL1x2)} – {f(pL3x4)}")
    line_cont(f"{f(dd24)} cm")
    step += 1

    line1(step, "ddy24Cm", "dd24Cm – dy24Cm"); line_cont(f"{f(dd24)} – ({f(dy24)})")
    line_cont(f"{f(ddy24)} cm         # symmetrical calc for the (2,4) diagonal")
    step += 1
    sep()

    # Intersection helpers ------------------------------------------
    ddL13 = cL1 - cL3
    line1(step, "ddL13", "cL1 – cL3"); line_cont(f"{f(cL1,dec=6)} – {f(cL3,dec=4)}")
    line_cont(f"{f(ddL13)}             # denominator used in both x-intercepts")
    step += 1

    p5x = ddy13 / ddL13
    dx51 = p5x - p1x
    p5y = cL1 * dx51 + p1y
    p6x = ddy24 / ddL13
    dx62 = p6x - p2x
    p6y = cL1 * dx62 + p2y

    line1(step, "p5xCm", "ddy13Cm / ddL13"); line_cont(f"{f(ddy13)} / {f(ddL13)}")
    line_cont(f"{f(p5x)} cm           # x-coord of the foot of perpendicular from p3")
    step += 1

    line1(step, "dx51Cm", "p5x – p1x"); line_cont(f"{p5x} – {p1x}")
    line_cont(f"{f(dx51)} cm           # horizontal difference p5 → p1")
    step += 1

    line1(step, "p5yCm", "cL1·dx51 + p1y"); line_cont(f"({f(cL1,dec=6)})({f(dx51)}) + {p1y}")
    line_cont(f"{f(p5y)} cm           # y of p5 (lies on L1)")
    step += 1

    line1(step, "p6xCm", "ddy24Cm / ddL13"); line_cont(f"{f(ddy24)} / {f(ddL13)}")
    line_cont(f"{f(p6x)} cm          # x-coord of foot from p4 to L1")
    step += 1

    line1(step, "dx62Cm", "p6x – p2x"); line_cont(f"{p6x} – {p2x}")
    line_cont(f"{f(dx62)} cm")
    step += 1

    line1(step, "p6yCm", "cL1·dx62 + p2y"); line_cont(f"({f(cL1,dec=6)})({f(dx62)}) + {p2y}")
    line_cont(f"{f(p6y)} cm")
    step += 1
    sep()

    # Remaining simple deltas --------------------------------------
    dx53 = p5x - p3x
    dx64 = p6x - p4x
    dy53 = p5y - p3y
    dy64 = p6y - p4y

    print(f"Step {step:02d}  dx53Cm = p5x – p3x = {p5x} – {p3x}   = {f(dx53)} cm"); step += 1
    print(f"Step {step:02d}  dx64Cm = p6x – p4x = {p6x} – {p4x} = {f(dx64)} cm"); step += 1
    print(f"Step {step:02d}  dy53Cm = p5y – p3y = {p5y} – {p3y}  = {f(dy53)} cm"); step += 1
    print(f"Step {step:02d}  dy64Cm = p6y – p4y = {p6y} – {p4y}  = {f(dy64)} cm"); step += 1
    sep()

    # Distances -----------------------------------------------------
    d53 = sqrt(dx53**2 + dy53**2)
    d64 = sqrt(dx64**2 + dy64**2)

    line1(step, "d53Cm", "√(dx53² + dy53²)")
    line_cont(f"√({f(dx53)}² + {f(dy53)}²)")
    line_cont(f"√({dx53**2:.3f} + {dy53**2:.3f})")
    line_cont(f"√{dx53**2 + dy53**2:.3f}")
    line_cont(f"{f(d53)} cm          # true right-femur length")
    step += 1

    line1(step, "d64Cm", "√(dx64² + dy64²)")
    line_cont(f"√({f(dx64)}² + {f(dy64)}²)")
    line_cont(f"√({dx64**2:.3f} + {dy64**2:.3f})")
    line_cont(f"√{dx64**2 + dy64**2:.3f}")
    line_cont(f"{f(d64)} cm          # true left-femur length")
    step += 1
    sep()

    # Discrepancy & alarm ------------------------------------------
    dCm = d53 - d64
    line1(step, "dCm ", "d53 – d64")
    line_cont(f"{d53:.4f} – {d64:.4f}")
    line_cont(f"{f(dCm)} cm           # **leg-length discrepancy**")
    step += 1

    alarm = abs(dCm) > 1.25
    line1(step, "LLDAlarm", "|dCm| > 1.25 ?")
    line_cont(f"|{f(dCm)}| > 1.25  →  {alarm}")
    line_cont(f"{alarm:>10.6f}    # Alarm raised: discrepancy exceeds ±1.25 cm")

# ────────────────────────── demo run ───────────────────────────
if __name__ == "__main__":
    trace(
        p1x=10.1, p1y=7.8,
        p2x=45.1, p2y=5.6,
        p3x=3.6,  p3y=29.8,
        p4x=54.7, p4y=28.5
    )

