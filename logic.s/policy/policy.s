:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'/2).

% sample data
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<http://example.org/ns#RubenD>', '<http://xmlns.com/foaf/0.1/Person>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<http://example.org/ns#JosD>', '<http://www.w3.org/2006/vcard/ns#Individual>').

% define mapping operation
'<http://www.w3.org/2000/10/swap/log#onNeutralSurface>'([],
    (
        % mapping foaf:Person and vcard:Individual on each other
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:S'],
            (
                '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:S', '<http://xmlns.com/foaf/0.1/Person>'),
                '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
                    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:S', '<http://www.w3.org/2006/vcard/ns#Individual>')
                )
            )
        ),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:S'],
            (
                '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:S', '<http://www.w3.org/2006/vcard/ns#Individual>'),
                '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
                    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:S', '<http://xmlns.com/foaf/0.1/Person>')
                )
            )
        )
    )
).

% enact
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:S'],
    (
        '<http://www.w3.org/2000/10/swap/log#onNeutralSurface>'([], '_:S'),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([], '_:S')
    )
).

% query
'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A', '_:B'],
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:A', '_:B')
).
