:- dynamic('<http://example.org/ns#is>'/2).

% beetle is a car
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<http://example.org/ns#beetle>', '<http://example.org/ns#Car>').

% all cars are green or blue
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:A'],
    (
        '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:A', '<http://example.org/ns#Car>'),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
            '<http://example.org/ns#is>'('_:A', '<http://example.org/ns#green>')
        ),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
            '<http://example.org/ns#is>'('_:A', '<http://example.org/ns#blue>')
        )
    )
).

% green things are beautiful
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:A'],
    (
        '<http://example.org/ns#is>'('_:A', '<http://example.org/ns#green>'),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
            '<http://example.org/ns#is>'('_:A', '<http://example.org/ns#beautiful>')
        )
    )
).

% blue things are beautiful
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:A'],
    (
        '<http://example.org/ns#is>'('_:A', '<http://example.org/ns#blue>'),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
            '<http://example.org/ns#is>'('_:A', '<http://example.org/ns#beautiful>')
        )
    )
).

% query
'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:S', '_:O'],
    '<http://example.org/ns#is>'('_:S', '_:O')
).
