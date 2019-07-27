def fibonacci(n, c = {0:1, 1:1}):
    if n not in c:
        x = n // 2
        c[n] = fibonacci(x-1)*fibonacci(n-x-1) + fibonacci(x)*fibonacci(n-x)
    return c[n]

if __name__ == "__main__":
    print('PREFIX : <http://josd.github.io/eye/provision/fibonacci#>')
    print('')
    for i in [0, 91, 283]:
        print('[] :fibonacci-number "fibonacci(%d) = %d".' % (i, fibonacci(i)))
