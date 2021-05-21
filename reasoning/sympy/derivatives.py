# See https://en.wikipedia.org/wiki/Integral

from sympy import diff, cos, exp, log
from sympy.abc import x, y, z

if __name__ == "__main__":
    cases = [
        "diff(cos(x), x)",
        "diff(x**4, x, x, x)",
        "diff(exp(x*y*z), x, y, y, z, z, z, z)",
        "diff((x+1)*((x**2+2)*(x**3+3)), x)",
        "diff(x/x/x/x/x/x/x/x/x/x, x)",
        "diff(log(log(log(log(log(log(log(log(log(log(x)))))))))), x)",
        "diff(x*x*x*x*x*x*x*x*x*x, x)"
    ]

    for c in cases:
        print('[ :sympy-statement "%s = %s"].' % (c, eval(c)))
    print("")
