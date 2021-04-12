# See https://en.wikipedia.org/wiki/Complex_number

from sympy import E
from sympy import I
from sympy import pi
from sympy import sqrt

if __name__ == "__main__":
    cases = [
        "sqrt(-1)",
        "E**(I*pi) + 1"
    ]

    for c in cases:
        print("%s = %s" % (c, eval(c)))
    print("")
