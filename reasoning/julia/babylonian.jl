# See https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method

function sqrt(x)
    t = 1
    for i in 1:10
        t = (t + x / t) / 2
    end
    t
end

expressions = [
    :(sqrt(2))
    :(sqrt(81))
    :(sqrt(62232491515607091882574410635924603070626544377175485625797))
]

for expr in expressions
    println("[] :julia-result \"", escape_string(string(expr)), " = ", eval(expr), "\".")
end
