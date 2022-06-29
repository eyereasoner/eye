# See https://en.wikipedia.org/wiki/Derivative
# Examples from https://github.com/JuliaSymbolics/Symbolics.jl

using Symbolics

@variables x y z

expressions = [
    :(Symbolics.derivative(x + x^2, x))
    :(Symbolics.derivative(x + 2x^2 + 6x^3 - 5x^4, x))
    :(Symbolics.derivative(cos(x) * sin(x), x))
    :(Symbolics.derivative(exp(x), x))
    :(Symbolics.derivative(log(x), x))
    :(Symbolics.jacobian([x + x*y, x^4 + y], [x, y]))
    :(Symbolics.hessian(x^4 + z, [x, y, z]))
]

for expr in expressions
    println("[] :julia-result \"", escape_string(string(expr)), " = ", eval(expr), "\".")
end
