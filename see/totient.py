# See https://en.wikipedia.org/wiki/Euler%27s_totient_function

from sympy import totient

if __name__ == "__main__":
    cases = [
        "totient(271)",
        "totient(2718281)",
        "totient(27182818284)",
        "totient(271828182845904)"
    ]

    for c in cases:
        print('[ :sympy-expression "%s"; :sympy-evaluation "%s"].' % (c, eval(c)))
    print("")
