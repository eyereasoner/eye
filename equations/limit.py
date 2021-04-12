# See https://en.wikipedia.org/wiki/Limit_(mathematics)

from sympy import Symbol
from sympy import sin
from sympy import limit
from sympy import oo

if __name__ == "__main__":
    x = Symbol('x')
    cases = [
        "limit(sin(x)/x, x, oo)",
        "limit(1/x, x, 0)",
        "limit(1/x, x, 0, dir='-')"
    ]

    for c in cases:
        print("%s = %s" % (c, eval(c)))
    print("")
