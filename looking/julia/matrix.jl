# See https://en.wikipedia.org/wiki/Matrix_(mathematics)
# Examples from https://docs.julialang.org/en/v1/stdlib/LinearAlgebra/

using LinearAlgebra

expressions = [
    :(inv([1 2 3; 4 1 6; 7 8 1]))
    :(det([1 2 3; 4 1 6; 7 8 1]))
    :(eigvals([-4. -17.; 2. 2.]))
    :(eigvecs([-4. -17.; 2. 2.]))
    :(factorize([1.5 2 -4; 3 -1 -6; -10 2.3 4]))
    :(factorize([1.5 2 -4; 2 -1 -3; -4 -3 5]))
    :(([1 0; 1 -2] \ [32; -4]))
    :(([1 0; 1 -2] * [32.0, 18.0]))
    :(dot([1; 1], [2; 3]))
    :(cross([0; 1; 0], [0; 0; 1]))
    :(UpperHessenberg([1 2 3 4; 5 6 7 8; 9 10 11 12; 13 14 15 16]))
    :(cholesky([4. 12. -16.; 12. 37. -43.; -16. -43. 98.]).U)
    :(cholesky([4. 12. -16.; 12. 37. -43.; -16. -43. 98.]).L)
]

for expr in expressions
    println("[] :julia-result \"", escape_string(string(expr)), " = ", eval(expr), "\".")
end
