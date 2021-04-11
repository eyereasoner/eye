# See https://en.wikipedia.org/wiki/Fast_Fourier_transform

from sympy import *

if __name__ == "__main__":
    print("fft(%s) = %s" % ([0, 1, 2, 3, 4, 5, 6, 7], fft([0, 1, 2, 3, 4, 5, 6, 7])))
    print("fft(%s) = %s" % ([0, 1, 2, 3, 0, 1, 2, 3], fft([0, 1, 2, 3, 0, 1, 2, 3])))
    print("fft(%s) = %s" % ([0, 1, 0, 1, 0, 1, 0, 1], fft([0, 1, 0, 1, 0, 1, 0, 1])))
    print("fft(%s) = %s" % ([0, 0, 0, 0, 0, 0, 0, 0], fft([0, 0, 0, 0, 0, 0, 0, 0])))
    print("")
