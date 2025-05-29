:- op(1200, xfx, :+).

answer(derivative((x+1)*((x^2+2)*(x^3+3)), x, (x^2+2)*(x^3+3)+(x+1)*(2*x*(x^3+3)+(x^2+2)*(3*x^2)))).
answer(derivative(x/x/x/x/x/x/x/x/x/x, x, (((((((((x-x)/x^2*x-x/x)/x^2*x-x/x/x)/x^2*x-x/x/x/x)/x^2*x-x/x/x/x/x)/x^2*x-x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x/x/x)/x^2)).
answer(derivative(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, 1/x/log(x)/log(log(x))/log(log(log(x)))/log(log(log(log(x))))/log(log(log(log(log(x)))))/log(log(log(log(log(log(x))))))/log(log(log(log(log(log(log(x)))))))/log(log(log(log(log(log(log(log(x))))))))/log(log(log(log(log(log(log(log(log(x))))))))))).
answer(derivative(x*x*x*x*x*x*x*x*x*x, x, ((((((((x+x)*x+x*x)*x+x*x*x)*x+x*x*x*x)*x+x*x*x*x*x)*x+x*x*x*x*x*x)*x+x*x*x*x*x*x*x)*x+x*x*x*x*x*x*x*x)*x+x*x*x*x*x*x*x*x*x)).

step((true:+derivative((x+1)*((x^2+2)*(x^3+3)), x, _)), derivative((x+1)*((x^2+2)*(x^3+3)), x, (x^2+2)*(x^3+3)+(x+1)*(2*x*(x^3+3)+(x^2+2)*(3*x^2))), true).
step((true:+derivative(x/x/x/x/x/x/x/x/x/x, x, _)), derivative(x/x/x/x/x/x/x/x/x/x, x, (((((((((x-x)/x^2*x-x/x)/x^2*x-x/x/x)/x^2*x-x/x/x/x)/x^2*x-x/x/x/x/x)/x^2*x-x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x/x/x)/x^2), true).
step((true:+derivative(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, _)), derivative(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, 1/x/log(x)/log(log(x))/log(log(log(x)))/log(log(log(log(x))))/log(log(log(log(log(x)))))/log(log(log(log(log(log(x))))))/log(log(log(log(log(log(log(x)))))))/log(log(log(log(log(log(log(log(x))))))))/log(log(log(log(log(log(log(log(log(x)))))))))), true).
step((true:+derivative(x*x*x*x*x*x*x*x*x*x, x, _)), derivative(x*x*x*x*x*x*x*x*x*x, x, ((((((((x+x)*x+x*x)*x+x*x*x)*x+x*x*x*x)*x+x*x*x*x*x)*x+x*x*x*x*x*x)*x+x*x*x*x*x*x*x)*x+x*x*x*x*x*x*x*x)*x+x*x*x*x*x*x*x*x*x), true).
