# See https://en.wikipedia.org/wiki/Complex_number

from sympy import E, I, pi,sqrt

if __name__ == "__main__":
    cases = [
        "sqrt(-1)",
        "E**(I*pi) + 1"
    ]

    for c in cases:
        print('[ :sympy-statement "%s = %s"].' % (c, eval(c)))
    print("")
