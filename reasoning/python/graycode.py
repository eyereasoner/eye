# See https://en.wikipedia.org/wiki/Gray_code

from sympy.combinatorics.graycode import GrayCode

if __name__ == "__main__":
    cases = [
        "list(GrayCode(2).generate_gray())",
        "list(GrayCode(3).generate_gray())",
        "list(GrayCode(4).generate_gray())"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
