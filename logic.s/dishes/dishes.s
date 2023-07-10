:- dynamic('<urn:example:contains>'/2).
:- dynamic('<urn:example:does>'/2).
:- dynamic('<urn:example:is>'/2).

% Bob doesn't do the dishes
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
    '<urn:example:does>'('<urn:example:Bob>', '<urn:example:Dishes>')
).

% When anyone cant do dishes, it is because the sink doens't contain water
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X'],
    (
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
            '<urn:example:does>'('_:X', '<urn:example:Dishes>')
        ),
        '<urn:example:contains>'('<urn:example:Sink>', '<urn:example:Water>')
    )
).

% If the faucet is open, then the sink contains water
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
    (
        '<urn:example:is>'('<urn:example:Faucet>', '<urn:example:Open>'),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
            '<urn:example:contains>'('<urn:example:Sink>', '<urn:example:Water>')
        )
    )
).

% Test that we don't have an open faucet
'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'([],
    '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
        '<urn:example:is>'('<urn:example:Faucet>', '<urn:example:Open>')
    )
).
