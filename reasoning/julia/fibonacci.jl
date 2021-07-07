# See https://en.wikipedia.org/wiki/Fibonacci

fibonacci(n) = ([1 1 ; 1 0] ^ n)[1, 2]

expressions = [
    :(fibonacci(0))
    :(fibonacci(1))
    :(fibonacci(6))
    :(fibonacci(91))
    :(fibonacci(big(283)))
    :(fibonacci(big(3674)))
]

for expr in expressions
    println("[] :julia-result \"", escape_string(string(expr)), " = ", eval(expr), "\".")
end
