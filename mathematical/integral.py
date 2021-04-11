# See https://en.wikipedia.org/wiki/Integral

from sympy import *

if __name__ == "__main__":
    print("# running integral.py")
    x = Symbol('x')
    print("bell_integral = 'integrate(exp(-x**2), (x, -oo, oo)) = %s'" % (integrate(exp(-x**2), (x, -oo, oo))))
    print("bee1_integral = 'integrate(1/(1+x**2), (x, -oo, oo)) = %s'" % (integrate(1/(1+x**2), (x, -oo, oo))))
    print("")
