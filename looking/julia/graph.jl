# See https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)

using Julog

clauses = @julog [
    oneway(paris, orleans) <<= true,
    oneway(paris, chartres) <<= true,
    oneway(paris, amiens) <<= true,
    oneway(orleans, blois) <<= true,
    oneway(orleans, bourges) <<= true,
    oneway(blois, tours) <<= true,
    oneway(chartres, lemans) <<= true,
    oneway(lemans, angers) <<= true,
    oneway(lemans, tours) <<= true,
    oneway(angers, nantes) <<= true,

    path(X, Y) <<= oneway(X, Y),
    path(X, Y) <<= oneway(X, Z) & path(Z, Y)
]

cases = [
    :(resolve(@julog(path(X, angers)), clauses)),
    :(resolve(@julog(path(angers, X)), clauses)),
    :(resolve(@julog(path(paris, nantes)), clauses))
]

for case in cases
    println("[ :julia-result \"", case, " = ", eval(case), "\"].")
end

