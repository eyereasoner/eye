# See https://en.wikipedia.org/wiki/Derivative

from sympy import diff, cos, exp, log, hessian
from sympy.matrices import Matrix
from sympy.abc import x, y, z

if __name__ == "__main__":
    cases = [
        "diff(cos(x), x)",
        "diff(x**4, x, x, x)",
        "diff(exp(x*y*z), x, y, y, z, z, z, z)",
        "diff((x + 1)*((x**2 + 2)*(x**3 + 3)), x)",
        "diff(x/x/x/x/x/x/x/x/x/x, x)",
        "diff(log(log(log(log(log(log(log(log(log(log(x)))))))))), x)",
        "diff(x*x*x*x*x*x*x*x*x*x, x)",
        "Matrix([x**3 + 5*y**4 - 9*z, exp(x*y*z)]).jacobian([x, y, z])",
        "hessian(x**3 + 5*y**4 - 9*z, [x, y, z])"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
