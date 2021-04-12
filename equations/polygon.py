# See https://en.wikipedia.org/wiki/Polygon

from sympy import Polygon

if __name__ == "__main__":
    cases = [
        "Polygon((3, 2), (6, 2), (7, 6), (4, 6), (5, 5), (5, 3), (3, 2)).area"
    ]

    for lhs in cases:
        print("%s = %s" % (lhs, eval(lhs)))
    print("")
