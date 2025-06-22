# See https://en.wikipedia.org/wiki/Integral

from sympy import exp, sin, integrate, oo
from sympy.abc import x

if __name__ == "__main__":
    cases = [
        "integrate(exp(-x**2), (x, -oo, oo))",
        "integrate(1/(1 + x**2), (x, -oo, oo))",
        "integrate(sin(x)**3, (x, 0, 1))"
    ]

    for c in cases:
        print("%s = %s" % (c, eval(c)))
