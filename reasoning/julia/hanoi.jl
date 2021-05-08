# See https://en.wikipedia.org/wiki/Tower_of_Hanoi
# Original code from https://rosettacode.org/wiki/Towers_of_Hanoi#Julia

function hanoi(n::Integer, from::Integer, to::Integer, via::Integer)
    if n != 1
        hanoi(n - 1, from, via, to)
        hanoi(1, from, to, via)
        hanoi(n - 1, via, to, from)
    end
    true
end
 
cases = [
    :(hanoi(4, 1, 2, 3))
    :(hanoi(24, 1, 2, 3))
]

for case in cases
    println("[ :julia-statement \"\"\"", case, " = ", eval(case), "\"\"\"].")
end
