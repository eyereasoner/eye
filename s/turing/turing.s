% interpreter for Univeral Turing Machine
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:OutTape', '_:Machine', '_:I'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#compute>'([], '_:OutTape')
        ),
        '<http://example.org/ns#start>'('_:Machine', '_:I'),
        '<http://example.org/ns#find>'(['_:I', [], #, []], '_:OutTape')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:List', '_:OutTape', '_:Head', '_:Tail', '_:Machine', '_:I'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#compute>'('_:List', '_:OutTape')
        ),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:List', ['_:Head', '_:Tail']),
        '<http://example.org/ns#start>'('_:Machine', '_:I'),
        '<http://example.org/ns#find>'(['_:I', [], '_:Head', '_:Tail'], '_:OutTape')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:State', '_:Left', '_:Cell', '_:Right', '_:OutTape', '_:Write', '_:Move', '_:Next', '_:A', '_:B', '_:C'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#find>'(['_:State', '_:Left', '_:Cell', '_:Right'], '_:OutTape')
        ),
        '<http://example.org/ns#tape>'(['_:State', '_:Cell', '_:Write', '_:Move'], '_:Next'),
        '<http://example.org/ns#move>'(['_:Move', '_:Left', '_:Write', '_:Right', '_:A', '_:B', '_:C'], true),
        '<http://example.org/ns#continue>'(['_:Next', '_:A', '_:B', '_:C'], '_:OutTape')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:Left', '_:Cell', '_:Right', '_:OutTape', '_:R', '_:List'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#continue>'(['<http://example.org/ns#halt>', '_:Left', '_:Cell', '_:Right'], '_:OutTape')
        ),
        '<http://example.org/ns#reverse>'('_:Left', '_:R'),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:List', ['_:Cell', '_:Right']),
        '<http://www.w3.org/2000/10/swap/list#append>'(['_:R', '_:List'], '_:OutTape')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:State', '_:Left', '_:Cell', '_:Right', '_:OutTape'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#continue>'(['_:State', '_:Left', '_:Cell', '_:Right'], '_:OutTape')
        ),
        '<http://example.org/ns#find>'(['_:State', '_:Left', '_:Cell', '_:Right'], '_:OutTape')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:Cell', '_:Right', '_:L'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#move>'(['<http://example.org/ns#left>', [], '_:Cell', '_:Right', [], #, '_:L'], true)
        ),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:L', ['_:Cell', '_:Right'])
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:List', '_:Cell', '_:Right', '_:Tail', '_:Head', '_:L'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#move>'(['<http://example.org/ns#left>', '_:List', '_:Cell', '_:Right', '_:Tail', '_:Head', '_:L'], true)
        ),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:List', ['_:Head', '_:Tail']),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:L', ['_:Cell', '_:Right'])
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:Left', '_:Cell', '_:Right'],
    '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
        '<http://example.org/ns#move>'(['<http://example.org/ns#stop>', '_:Left', '_:Cell', '_:Right', '_:Left', '_:Cell', '_:Right'], true)
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:Left', '_:Cell', '_:L'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#move>'(['<http://example.org/ns#right>', '_:Left', '_:Cell', [], '_:L', #, []], true)
        ),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:L', ['_:Cell', '_:Left'])
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:Left', '_:Cell', '_:List', '_:L', '_:Head', '_:Tail'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#move>'(['<http://example.org/ns#right>', '_:Left', '_:Cell', '_:List', '_:L', '_:Head', '_:Tail'], true)
        ),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:List', ['_:Head', '_:Tail']),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:L', ['_:Cell', '_:Left'])
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'([],
    '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
        '<http://example.org/ns#reverse>'([], [])
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:List', '_:Reverse', '_:Head', '_:Tail', '_:R'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://example.org/ns#reverse>'('_:List', '_:Reverse')
        ),
        '<http://www.w3.org/2000/10/swap/list#firstRest>'('_:List', ['_:Head', '_:Tail']),
        '<http://example.org/ns#reverse>'('_:Tail', '_:R'),
        '<http://www.w3.org/2000/10/swap/list#append>'(['_:R', ['_:Head']], '_:Reverse')
    )
).

% a Turing machine to add 1 to a binary number
'<http://example.org/ns#start>'('<http://example.org/ns#add1>', 0).

'<http://example.org/ns#tape>'([0, 0, 0, '<http://example.org/ns#right>'], 0).
'<http://example.org/ns#tape>'([0, 1, 1, '<http://example.org/ns#right>'], 0).
'<http://example.org/ns#tape>'([0, #, #, '<http://example.org/ns#left>'], 1).
'<http://example.org/ns#tape>'([1, 0, 1, '<http://example.org/ns#stop>'], '<http://example.org/ns#halt>').
'<http://example.org/ns#tape>'([1, 1, 0, '<http://example.org/ns#left>'], 1).
'<http://example.org/ns#tape>'([1, #, 1, '<http://example.org/ns#stop>'], '<http://example.org/ns#halt>').

% query
'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A'],
    '<http://example.org/ns#compute>'([1, 0, 1, 0, 0, 1], '_:A')
).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A'],
    '<http://example.org/ns#compute>'([1, 0, 1, 1, 1, 1], '_:A')
).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A'],
    '<http://example.org/ns#compute>'([1, 1, 1, 1, 1, 1], '_:A')
).

'<http://www.w3.org/2000/10/swap/log#onQuerySurface>'(['_:A'],
    '<http://example.org/ns#compute>'([], '_:A')
).
