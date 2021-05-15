# See https://en.wikipedia.org/wiki/Complex_number

cases = [
    :(sqrt(-1+0im)),
    :(exp(π * im))
]

for case in cases
    println("[ :case \"", case, " = ", eval(case), "\"].")
end
