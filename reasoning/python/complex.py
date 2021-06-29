# See https://en.wikipedia.org/wiki/Complex_number

from sympy import sqrt, exp, log, I, pi, cos, sin, asin, N
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
        "N(asin(2))"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
    print("")
