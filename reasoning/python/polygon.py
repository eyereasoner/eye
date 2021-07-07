# See https://en.wikipedia.org/wiki/Polygon

from sympy import Polygon

if __name__ == "__main__":
    cases = [
        "Polygon((0, 0), (2, 0), (0, 1), (0, 0)).area",
        "Polygon((3, 2), (6, 2), (7, 6), (4, 6), (5, 5), (5, 3), (3, 2)).area"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
