# See https://en.wikipedia.org/wiki/Matrix_(mathematics)

from sympy.matrices import Matrix

if __name__ == "__main__":
    cases = [
        "Matrix([[1, 3], [-2, 3]])*Matrix([[0, 3], [0, 7]])",
        "Matrix([[1, 3], [-2, 3]])**2",
        "Matrix([[1, 3], [-2, 3]])**-1",
        "Matrix([[1, 0, 1], [2, -1, 3], [4, 3, 2]]).det()",
        "Matrix([[3, -2,  4, -2], [5,  3, -3, -2], [5, -2,  2, -2], [5, -2, -3,  3]]).eigenvals()"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
