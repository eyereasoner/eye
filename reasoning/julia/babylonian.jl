function sqrt(x)
    t=1
    for i in 1:10
        t = (t+x/t)/2
    end
    t
end

println("[ :julia-statement \"sqrt(2) = ", sqrt(2), "\"].")
