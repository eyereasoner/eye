:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'/2).

% ForAll x : x = y -> x a z 
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:x'],(
        '<http://www.w3.org/2000/10/swap/log#equalTo>'('_:x',':y'),
        '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
           '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:x',':z')        
        )
    )
).

% ForAll x : x in [y] -> ~(x a z)
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:x'],(
        '<http://www.w3.org/2000/10/swap/list#in>'('_:x',[':y']),
        '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('_:x',':z')        
    )
).