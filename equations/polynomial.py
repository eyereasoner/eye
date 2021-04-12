# See https://en.wikipedia.org/wiki/Polynomial

from sympy import Symbol
from sympy import I
from sympy import solve
from sympy import solve_poly_system

if __name__ == "__main__":
    x = Symbol('x')
    y = Symbol('y')
    cases = [
        "solve(x**4-10*x**3+35*x**2-50*x+24, x)",
        "solve(x**4+(-9-5*I)*x**3+(14+33*I)*x**2+(24-44*I)*x-26, x)",
        "solve_poly_system([x*y - 2*y, 2*y**2 - x**2], x, y)"
    ]

    for c in cases:
        print("%s = %s" % (c, eval(c)))
    print("")
