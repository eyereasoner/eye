using Polynomials, Primes

fibonacci(n) = ([1 1 ; 1 0] ^ n)[1, 2]

# test cases
cases = [
    :(sqrt(-1+0im)),
    :(exp(π * im)),
    :(fibonacci(0)),
    :(fibonacci(1)),
    :(fibonacci(6)),
    :(fibonacci(big(283))),
    :(fibonacci(big(3674))),
    :(roots(Polynomial([24 + 7im, -50, 35, -10, 1]))),
    :(roots(Polynomial([-26, 24 - 44im, 14 + 33im, -9 - 5im, 1]))),
    :(primes(0, 100)),
    :(primes(1000000, 1000100)),
    :(isprime(6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151)),
    :(nextprime(6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151)),
    :(totient(271)),
    :(totient(2718281)),
    :(totient(27182818284)),
    :(totient(271828182845904))
]

results = [string("[ :julia-statement \"", case, " = ", eval(case), "\"].") for case in cases]

println("PREFIX : <http://josd.github.io/eye/reasoning#>")
println("")
for result in results
    println(result)
end

# run again after warming-up
print("#")
@time results = [string("[ :julia-statement \"", case, " = ", eval(case), "\"].") for case in cases]
