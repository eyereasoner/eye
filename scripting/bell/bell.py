from sympy import *

if __name__ == "__main__":
    x = Symbol('x')
    print('PREFIX : <http://josd.github.io/eye/scripting/bell#>')
    print('')
    print('[] :bell-integral "integrate(exp(-x**2), (x, -oo, oo)) = %s".' % (integrate(exp(-x**2), (x, -oo, oo))))
