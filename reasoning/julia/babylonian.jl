function sqrt(x)
    t=1
    for i in 1:10
        t = (t+x/t)/2
    end
    t
end

cases = [
    :(sqrt(2))
]

for case in cases
    println("[ :julia-statement \"", case, " = ", eval(case), "\"].")
end
