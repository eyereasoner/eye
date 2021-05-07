# Towers of Hanoi
# Original code from https://rosettacode.org/wiki/Towers_of_Hanoi#Julia

function solve(n::Integer, from::Integer, to::Integer, via::Integer)
    if n == 1
        global cnt += 1
    else
        solve(n - 1, from, via, to)
        solve(1, from, to, via)
        solve(n - 1, via, to, from)
    end
    cnt
end
 
cases = [
    :(cnt = 0; solve(4, 1, 2, 3))
    :(cnt = 0; solve(24, 1, 2, 3))
]

for case in cases
    println("[ :julia-statement \"\"\"", case, " = ", eval(case), "\"\"\"].")
end
