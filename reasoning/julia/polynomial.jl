using Polynomials

cases = [
    :(roots(Polynomial([24+7im, -50, 35, -10, 1]))),
    :(roots(Polynomial([-26, 24-44im, 14+33im, -9-5im, 1])))
]

for case in cases
    println("[ :julia-statement \"", case, " = ", eval(case), "\"].")
end
