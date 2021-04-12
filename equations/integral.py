# See https://en.wikipedia.org/wiki/Integral

from sympy import Symbol
from sympy import exp
from sympy import integrate
from sympy import oo

if __name__ == "__main__":
    x = Symbol('x')
    cases = [
        "integrate(exp(-x**2), (x, -oo, oo))",
        "integrate(1/(1+x**2), (x, -oo, oo))"
    ]

    for lhs in cases:
        print("%s = %s" % (lhs, eval(lhs)))
    print("")
