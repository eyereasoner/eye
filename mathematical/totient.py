# See https://en.wikipedia.org/wiki/Euler%27s_totient_function

from sympy import *

if __name__ == "__main__":
    print("totient(%d) = %d" % (271, totient(271)))
    print("totient(%d) = %d" % (2718281, totient(2718281)))
    print("totient(%d) = %d" % (27182818284, totient(27182818284)))
    print("totient(%d) = %d" % (271828182845904, totient(271828182845904)))
    print("")
