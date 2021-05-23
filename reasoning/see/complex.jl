# See https://en.wikipedia.org/wiki/Complex_number

expressions = [
    :(sqrt(-1+0im))
    :(exp(π * im) + 1)
    :(sin(im))
]

for expr in expressions
    println("[] :expression \"", replace(repr(expr), "\\" => "\\\\"), "\"; :evaluation \"", eval(expr), "\".")
end
println("")
