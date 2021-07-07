# See https://en.wikipedia.org/wiki/Limit_(mathematics)

from sympy import sin, limit, oo
from sympy.abc import x

if __name__ == "__main__":
    cases = [
        "limit(sin(x)/x, x, oo)",
        "limit(1/x, x, 0)",
        "limit(1/x, x, 0, dir='-')"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
