# See https://en.wikipedia.org/wiki/Fibonacci_number

from sympy import fibonacci

if __name__ == "__main__":
    cases = [
        "fibonacci(0)",
        "fibonacci(1)",
        "fibonacci(6)",
        "fibonacci(91)",
        "fibonacci(283)",
        "fibonacci(3674)",
        "fibonacci(29821)"
    ]

    for lhs in cases:
        print("%s = %s" % (lhs, eval(lhs)))
    print("")
