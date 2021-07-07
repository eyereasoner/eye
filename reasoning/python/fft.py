# See https://en.wikipedia.org/wiki/Fast_Fourier_transform

from sympy import fft

if __name__ == "__main__":
    cases = [
        "fft([0, 1, 2, 3, 4, 5, 6, 7])",
        "fft([0, 1, 2, 3, 0, 1, 2, 3])",
        "fft([0, 1, 0, 1, 0, 1, 0, 1])",
        "fft([0, 0, 0, 0, 0, 0, 0, 0])"
    ]

    for c in cases:
        print('[] :python-result "%s = %s".' % (c, eval(c)))
