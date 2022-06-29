# See https://en.wikipedia.org/wiki/Complex_number

expressions = [
    :(sqrt(-1+0im))
    :(exp(π * im) + 1)
    :(sin(im))
    :(acos(2+0im))
    :(asin(2+0im))
    :(atan(1+2im))
    :(log(-1+0im))
    :(log(-im))
]

for expr in expressions
    println("[] :julia-result \"", escape_string(string(expr)), " = ", eval(expr), "\".")
end
