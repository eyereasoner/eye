cases = [
    :(exp(π * im) + 1)
]

for case in cases
    println("[ :julia-statement \"\"\"", case, " = ", eval(case), "\"\"\"].")
end
