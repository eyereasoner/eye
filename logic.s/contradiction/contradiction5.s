% See https://github.com/eyereasoner/Notation3-By-Example/blob/main/log/blogic/contradiction5:FAIL.n3

:- dynamic('<http://example.org/ns#hates>'/2).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:x'],
    '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'('_:x',
        '<http://example.org/ns#hates>'('<http://example.org/ns#Alice>', '<http://example.org/ns#Quiche>')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:a'],
    '<http://example.org/ns#hates>'('<http://example.org/ns#Alice>', '<http://example.org/ns#Quiche>')
).

'<http://example.org/ns#is>'('<http://example.org/ns#test>', true).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'([],
    '<http://example.org/ns#is>'('<http://example.org/ns#test>', true)
).
