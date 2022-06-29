# See https://en.wikipedia.org/wiki/Tower_of_Hanoi
# Original code from https://rosettacode.org/wiki/Towers_of_Hanoi#Julia

function hanoi(n, from, to, via)
    if n != 1
        hanoi(n - 1, from, via, to)
        hanoi(1, from, to, via)
        hanoi(n - 1, via, to, from)
    end
    true
end

expressions = [
    :(hanoi(4, 1, 2, 3))
    :(hanoi(24, 1, 2, 3))
]

for expr in expressions
    println("[] :julia-result \"", escape_string(string(expr)), " = ", eval(expr), "\".")
end
