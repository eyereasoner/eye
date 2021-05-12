# See https://en.wikipedia.org/wiki/Fibonacci

fibonacci(n) = ([1 1 ; 1 0] ^ n)[1, 2]

cases = [
    :(fibonacci(big(0)))
    :(fibonacci(big(1)))
    :(fibonacci(big(6)))
    :(fibonacci(big(91)))
    :(fibonacci(big(283)))
    :(fibonacci(big(3674)))
]

for case in cases
    println("[ :julia-statement \"", case, " = ", eval(case), "\"].")
end
