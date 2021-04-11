# See https://en.wikipedia.org/wiki/Fibonacci_number

def fibonacci(n, c = {0:1, 1:1}):
    if n not in c:
        x = n // 2
        c[n] = fibonacci(x-1)*fibonacci(n-x-1) + fibonacci(x)*fibonacci(n-x)
    return c[n]

if __name__ == "__main__":
    for i in [0, 1, 6, 91, 283, 3674, 29821, 754011]:
        print("fibonacci(%d) = %d" % (i, fibonacci(i)))
    print("")
