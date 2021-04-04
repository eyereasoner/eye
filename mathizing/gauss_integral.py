# See https://en.wikipedia.org/wiki/Gaussian_function

from sympy import *

if __name__ == "__main__":
    print("# mathizing gauss_integral.py")
    x = Symbol('x')
    print("gauss_integral = 'integrate(exp(-x**2), (x, -oo, oo)) = %s'" % (integrate(exp(-x**2), (x, -oo, oo))))
    print("")
