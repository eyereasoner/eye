# See https://en.wikipedia.org/wiki/Fibonacci_number

def fibonacci(n, c = {0:1, 1:1}):
    if n not in c:
        x = n // 2
        c[n] = fibonacci(x-1)*fibonacci(n-x-1) + fibonacci(x)*fibonacci(n-x)
    return c[n]

if __name__ == "__main__":
    print("# main of fibonacci.py")
    for i in [0, 91, 283, 3674, 1000000]:
        print("fibonacci_%d = %d" % (i, fibonacci(i)))
    print("")
