# See https://en.wikipedia.org/wiki/Padovan_sequence
# Inspired by https://math.stackexchange.com/questions/3798107/is-there-a-better-way-to-calculate-padovan-sequence-than-on

padovan(n) = ([0 1 0; 0 0 1; 1 1 0]^n)[3, 2]

expressions = [
    :(padovan(1))
    :(padovan(2))
    :(padovan(3))
    :(padovan(4))
    :(padovan(5))
    :(padovan(6))
    :(padovan(7))
    :(padovan(8))
    :(padovan(9))
    :(padovan(10))
    :(padovan(91))
    :(padovan(big(283)))
    :(padovan(big(3674)))
]

for expr in expressions
    println("[] :julia-result \"", escape_string(string(expr)), " = ", eval(expr), "\".")
end
