% exponentiation
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:A', '_:B', '_:C', '_:D', '_:E', '_:F', '_:R', '_:T', '_:Z1', '_:Z2', '_:Z3', '_:Z4', '_:Z5', '_:Z6', '_:Z7', '_:Z8', '_:Z9', '_:Z10'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/complex#exponentiation>'([['_:A', '_:B'], ['_:C', '_:D']], ['_:E', '_:F'])
        ),
        '<http://eyereasoner.github.io/eye/complex#polar>'(['_:A', '_:B'], ['_:R', '_:T']),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:R', '_:C'], '_:Z1'),
        '<http://www.w3.org/2000/10/swap/math#negation>'('_:D', '_:Z2'),
        '<http://www.w3.org/2000/10/swap/math#product>'(['_:Z2', '_:T'], '_:Z3'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'([2.718281828459045, '_:Z3'], '_:Z4'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'([2.718281828459045, '_:Z5'], '_:R'),
        '<http://www.w3.org/2000/10/swap/math#product>'(['_:D', '_:Z5'], '_:Z6'),
        '<http://www.w3.org/2000/10/swap/math#product>'(['_:C', '_:T'], '_:Z7'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:Z6', '_:Z7'], '_:Z8'),
        '<http://www.w3.org/2000/10/swap/math#cos>'('_:Z8', '_:Z9'),
        '<http://www.w3.org/2000/10/swap/math#product>'(['_:Z1', '_:Z4', '_:Z9'], '_:E'),
        '<http://www.w3.org/2000/10/swap/math#sin>'('_:Z8', '_:Z10'),
        '<http://www.w3.org/2000/10/swap/math#product>'(['_:Z1', '_:Z4', '_:Z10'], '_:F')
    )
).

% asin
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:A', '_:B', '_:C', '_:D', '_:E', '_:F', '_:Z1', '_:Z2', '_:Z3', '_:Z4', '_:Z5', '_:Z6', '_:Z7', '_:Z8', '_:Z9', '_:Z10', '_:Z11', '_:Z12', '_:Z13', '_:Z14', '_:Z15'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/complex#asin>'(['_:A', '_:B'], ['_:C', '_:D'])
        ),
        '<http://www.w3.org/2000/10/swap/math#sum>'([1, '_:A'], '_:Z1'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z1', 2], '_:Z2'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:B', 2], '_:Z3'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:Z2', '_:Z3'], '_:Z4'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z4', 0.5], '_:Z5'),
        '<http://www.w3.org/2000/10/swap/math#difference>'([1, '_:A'], '_:Z6'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z6', 2], '_:Z7'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:Z7', '_:Z3'], '_:Z8'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z8', 0.5], '_:Z9'),
        '<http://www.w3.org/2000/10/swap/math#difference>'(['_:Z5', '_:Z9'], '_:Z10'),
        '<http://www.w3.org/2000/10/swap/math#quotient>'(['_:Z10', 2], '_:E'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:Z5', '_:Z9'], '_:Z11'),
        '<http://www.w3.org/2000/10/swap/math#quotient>'(['_:Z11', 2], '_:F'),
        '<http://www.w3.org/2000/10/swap/math#asin>'('_:E', '_:C'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:F', 2], '_:Z12'),
        '<http://www.w3.org/2000/10/swap/math#difference>'(['_:Z12', 1], '_:Z13'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z13', 0.5], '_:Z14'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:F', '_:Z14'], '_:Z15'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'([2.718281828459045, '_:D'], '_:Z15')
    )
).

% acos
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:A', '_:B', '_:C', '_:D', '_:E', '_:F', '_:U', '_:Z1', '_:Z2', '_:Z3', '_:Z4', '_:Z5', '_:Z6', '_:Z7', '_:Z8', '_:Z9', '_:Z10', '_:Z11', '_:Z12', '_:Z13', '_:Z14', '_:Z15'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/complex#acos>'(['_:A', '_:B'], ['_:C', '_:D'])
        ),
        '<http://www.w3.org/2000/10/swap/math#sum>'([1, '_:A'], '_:Z1'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z1', 2], '_:Z2'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:B', 2], '_:Z3'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:Z2', '_:Z3'], '_:Z4'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z4', 0.5], '_:Z5'),
        '<http://www.w3.org/2000/10/swap/math#difference>'([1, '_:A'], '_:Z6'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z6', 2], '_:Z7'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:Z7', '_:Z3'], '_:Z8'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z8', 0.5], '_:Z9'),
        '<http://www.w3.org/2000/10/swap/math#difference>'(['_:Z5', '_:Z9'], '_:Z10'),
        '<http://www.w3.org/2000/10/swap/math#quotient>'(['_:Z10', 2], '_:E'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:Z5', '_:Z9'], '_:Z11'),
        '<http://www.w3.org/2000/10/swap/math#quotient>'(['_:Z11', 2], '_:F'),
        '<http://www.w3.org/2000/10/swap/math#acos>'('_:E', '_:C'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:F', 2], '_:Z12'),
        '<http://www.w3.org/2000/10/swap/math#difference>'(['_:Z12', 1], '_:Z13'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z13', 0.5], '_:Z14'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:F', '_:Z14'], '_:Z15'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'([2.718281828459045, '_:U'], '_:Z15'),
        '<http://www.w3.org/2000/10/swap/math#negation>'('_:U', '_:D')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X', '_:Y', '_:R', '_:T', '_:Tp', '_:Z1', '_:Z2', '_:Z3', '_:Z4', '_:Z5'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/complex#polar>'(['_:X', '_:Y'], ['_:R', '_:Tp'])
        ),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:X', 2], '_:Z1'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Y', 2], '_:Z2'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:Z1', '_:Z2'], '_:Z3'),
        '<http://www.w3.org/2000/10/swap/math#exponentiation>'(['_:Z3', 0.5], '_:R'),
        '<http://www.w3.org/2000/10/swap/math#absoluteValue>'('_:X', '_:Z4'),
        '<http://www.w3.org/2000/10/swap/math#quotient>'(['_:Z4', '_:R'], '_:Z5'),
        '<http://www.w3.org/2000/10/swap/math#acos>'('_:Z5', '_:T'),
        '<http://eyereasoner.github.io/eye/complex#dial>'(['_:X', '_:Y', '_:T'], '_:Tp')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X', '_:Y', '_:T', '_:Tp'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/complex#dial>'(['_:X', '_:Y', '_:T'], '_:Tp')
        ),
        '<http://www.w3.org/2000/10/swap/math#notLessThan>'('_:X', 0),
        '<http://www.w3.org/2000/10/swap/math#notLessThan>'('_:Y', 0),
        '<http://www.w3.org/2000/10/swap/math#sum>'([0, '_:T'], '_:Tp')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X', '_:Y', '_:T', '_:Tp'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/complex#dial>'(['_:X', '_:Y', '_:T'], '_:Tp')
        ),
        '<http://www.w3.org/2000/10/swap/math#lessThan>'('_:X', 0),
        '<http://www.w3.org/2000/10/swap/math#notLessThan>'('_:Y', 0),
        '<http://www.w3.org/2000/10/swap/math#difference>'([3.141592653589793, '_:T'], '_:Tp')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X', '_:Y', '_:T', '_:Tp'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/complex#dial>'(['_:X', '_:Y', '_:T'], '_:Tp')
        ),
        '<http://www.w3.org/2000/10/swap/math#lessThan>'('_:X', 0),
        '<http://www.w3.org/2000/10/swap/math#lessThan>'('_:Y', 0),
        '<http://www.w3.org/2000/10/swap/math#sum>'([3.141592653589793, '_:T'], '_:Tp')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X', '_:Y', '_:T', '_:Tp'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/complex#dial>'(['_:X', '_:Y', '_:T'], '_:Tp')
        ),
        '<http://www.w3.org/2000/10/swap/math#notLessThan>'('_:X', 0),
        '<http://www.w3.org/2000/10/swap/math#lessThan>'('_:Y', 0),
        '<http://www.w3.org/2000/10/swap/math#product>'([3.141592653589793, 2], '_:Z1'),
        '<http://www.w3.org/2000/10/swap/math#difference>'(['_:Z1', '_:T'], '_:Tp')
    )
).

% query
'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A', '_:B'],
    '<http://eyereasoner.github.io/eye/complex#exponentiation>'([[-1, 0], [0.5, 0]], ['_:A', '_:B'])
).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A', '_:B'],
    '<http://eyereasoner.github.io/eye/complex#exponentiation>'([[2.718281828459045, 0], [0, 3.141592653589793]], ['_:A', '_:B'])
).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A', '_:B'],
    '<http://eyereasoner.github.io/eye/complex#exponentiation>'([[0, 1], [0, 1]], ['_:A', '_:B'])
).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A', '_:B'],
    '<http://eyereasoner.github.io/eye/complex#exponentiation>'([[2.718281828459045, 0], [-1.57079632679, 0]], ['_:A', '_:B'])
).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A', '_:B'],
    '<http://eyereasoner.github.io/eye/complex#asin>'([2, 0], ['_:A', '_:B'])
).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A', '_:B'],
    '<http://eyereasoner.github.io/eye/complex#acos>'([2, 0], ['_:A', '_:B'])
).
