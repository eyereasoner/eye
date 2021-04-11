# See https://en.wikipedia.org/wiki/Euler%27s_totient_function

from sympy import *

if __name__ == "__main__":
    print("totient(%d) = %d" % (271828182845904, totient(271828182845904)))
    print("")
