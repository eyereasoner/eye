# See https://en.wikipedia.org/wiki/Complex_number

from sympy import sqrt, exp, log, I, pi, cos, sin
from sympy.abc import x

if __name__ == "__main__":
    cases = [
        "sqrt(-1)",
        "sqrt(I)",
        "exp(I*pi) + 1",
        "log(I*x)",
        "cos(I*x)",
        "sin(I*x)"
    ]

    for c in cases:
        print('[] :see "%s = %s".' % (c, eval(c)))
    print("")
