# See https://en.wikipedia.org/wiki/Integral

from sympy import *

if __name__ == "__main__":
    x = Symbol('x')
    print("integrate(exp(-x**2), (x, -oo, oo)) = %s" % (integrate(exp(-x**2), (x, -oo, oo))))
    print("integrate(1/(1+x**2), (x, -oo, oo)) = %s" % (integrate(1/(1+x**2), (x, -oo, oo))))
    print("")
