# See https://en.wikipedia.org/wiki/Integral

from sympy import exp, integrate, oo
from sympy.abc import x

if __name__ == "__main__":
    cases = [
        "integrate(exp(-x**2), (x, -oo, oo))",
        "integrate(1/(1 + x**2), (x, -oo, oo))"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
