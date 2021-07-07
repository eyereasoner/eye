# See https://en.wikipedia.org/wiki/Complex_number

from sympy import sqrt, exp, log, I, pi, cos, sin, acos, asin, N
from sympy.abc import x

if __name__ == "__main__":
    cases = [
        "sqrt(-1)",
        "exp(I*pi) + 1",
        "log(-1)",
        "log(-I)",
        "log(I*x)",
        "cos(I*x)",
        "sin(I*x)",
        "asin(I*x)",
        "N(sqrt(I))",
        "N(acos(2))",
        "N(asin(2))",
        "N(I**I, 136)"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
