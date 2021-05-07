fibonacci(n) = ([1 1 ; 1 0]^n)[1,2]

cases = [
    :(fibonacci(big(3674)))
]

for case in cases
    println("[ :julia-statement \"\"\"", case, " = ", eval(case), "\"\"\"].")
end
