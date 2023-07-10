% Calculate pi using Nilakantha series
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:N', '_:Pi', '_:P', '_:B'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning#pi>'(['_:N', '_:Pi'], true)
        ),
        '<http://eyereasoner.github.io/eye/reasoning#pi>'([1, '_:N', 0, '_:P', 1], true),
        '<http://www.w3.org/2000/10/swap/math#product>'([4, '_:P'], '_:B'),
        '<http://www.w3.org/2000/10/swap/math#sum>'([3, '_:B'], '_:Pi')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:N', '_:P', '_:S'],
    '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
        '<http://eyereasoner.github.io/eye/reasoning#pi>'(['_:N', '_:N', '_:P', '_:P', '_:S'], true)
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:K', '_:N', '_:P0', '_:P', '_:S', '_:K1', '_:K2', '_:P1', '_:S1', '_:B2', '_:B3', '_:B4', '_:B5'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning#pi>'(['_:K', '_:N', '_:P0', '_:P', '_:S'], true)
        ),
        '<http://www.w3.org/2000/10/swap/math#notEqualTo>'('_:K', '_:N'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:K', 1], '_:K1'),
        '<http://www.w3.org/2000/10/swap/math#product>'([2, '_:K'], '_:K2'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:K2', 1], '_:B2'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:K2', 2], '_:B3'),
        '<http://www.w3.org/2000/10/swap/math#product>'(['_:K2', '_:B2', '_:B3'], '_:B4'),
        '<http://www.w3.org/2000/10/swap/math#quotient>'(['_:S', '_:B4'], '_:B5'),
        '<http://www.w3.org/2000/10/swap/math#sum>'(['_:P0', '_:B5'], '_:P1'),
        '<http://www.w3.org/2000/10/swap/math#negation>'('_:S', '_:S1'),
        '<http://eyereasoner.github.io/eye/reasoning#pi>'(['_:K1', '_:N', '_:P1', '_:P', '_:S1'], true)
    )
).

% query
'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:Pi'],
    '<http://eyereasoner.github.io/eye/reasoning#pi>'([1000, '_:Pi'], true)
).
