# See https://en.wikipedia.org/wiki/Equation_solving

from sympy import solve, cos, sin, linsolve, nonlinsolve
from sympy.abc import x, y, z

if __name__ == "__main__":
    cases = [
        "solve(x**2 - 1, x)",
        "solve(x**2 + 1, x)",
        "solve(x**2 - 3*x + 2, x)",
        "solve(cos(x)*(1 - 2*sin(x)), x)",
        "solve(2**(2*x) - 5*2**(x + 1) + 16, x)",
        "linsolve([3*x + 2*y - z - 1, 2*x - 2*y + 4*z + 2, - x + y/2 - z], x, y, z)",
        "nonlinsolve([x*y - 1, 4*x**2 + y**2 - 5], x, y)"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
