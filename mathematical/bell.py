# See https://en.wikipedia.org/wiki/Gaussian_function

from sympy import *

if __name__ == "__main__":
    print("# running bell.py")
    x = Symbol('x')
    print("bell_integral = 'integrate(exp(-x**2), (x, -oo, oo)) = %s'" % (integrate(exp(-x**2), (x, -oo, oo))))
    print("")
