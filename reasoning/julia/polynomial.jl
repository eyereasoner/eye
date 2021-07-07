# See https://en.wikipedia.org/wiki/Polynomial

using Polynomials

expressions = [
    :(roots(Polynomial([24 + 7im, -50, 35, -10, 1])))
    :(roots(Polynomial([-26, 24 - 44im, 14 + 33im, -9 - 5im, 1])))
]

for expr in expressions
    println("[] :julia-result \"", escape_string(string(expr)), " = ", eval(expr), "\".")
end
