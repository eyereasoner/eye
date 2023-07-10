:- dynamic('<http://eyereasoner.github.io/eye/reasoning/dqc#sdcoding>'/2).

% |R) = |0, 0) + |1, 1)
'<http://eyereasoner.github.io/eye/reasoning/dqc#r>'(false,false).
'<http://eyereasoner.github.io/eye/reasoning/dqc#r>'(true,true).

% |S) = |0, 1) + |1, 0)
'<http://eyereasoner.github.io/eye/reasoning/dqc#s>'(false,true).
'<http://eyereasoner.github.io/eye/reasoning/dqc#s>'(true,false).

% |U) = |0, 0) + |1, 0) + |1, 1)
'<http://eyereasoner.github.io/eye/reasoning/dqc#u>'(false,false).
'<http://eyereasoner.github.io/eye/reasoning/dqc#u>'(true,false).
'<http://eyereasoner.github.io/eye/reasoning/dqc#u>'(true,true).

% |V ) = |0, 0) + |0, 1) + |1, 0)
'<http://eyereasoner.github.io/eye/reasoning/dqc#v>'(false,false).
'<http://eyereasoner.github.io/eye/reasoning/dqc#v>'(false,true).
'<http://eyereasoner.github.io/eye/reasoning/dqc#v>'(true,false).

% I |0) = |0)
'<http://eyereasoner.github.io/eye/reasoning/dqc#id>'(false,false).

% I |1) = |1)
'<http://eyereasoner.github.io/eye/reasoning/dqc#id>'(true,true).

% G |0) = |1)
'<http://eyereasoner.github.io/eye/reasoning/dqc#g>'(false,true).

% G |1) = |0)
'<http://eyereasoner.github.io/eye/reasoning/dqc#g>'(true,false).

% K |0) = |0)
'<http://eyereasoner.github.io/eye/reasoning/dqc#k>'(false,false).

% K |1) = |0) + |1)
'<http://eyereasoner.github.io/eye/reasoning/dqc#k>'(true,false).
'<http://eyereasoner.github.io/eye/reasoning/dqc#k>'(true,true).

% KG
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y','_:Z'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#kg>'('_:X','_:Y')
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#g>'('_:X','_:Z'),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#k>'('_:Z','_:Y')
    )
).

% GK
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y','_:Z'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#gk>'('_:X','_:Y')
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#k>'('_:X','_:Z'),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#g>'('_:Z','_:Y')
    )
).

% Alice
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#alice>'(0,['_:X','_:Y'])
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#id>'('_:X','_:Y')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#alice>'(1,['_:X','_:Y'])
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#g>'('_:X','_:Y')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#alice>'(2,['_:X','_:Y'])
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#k>'('_:X','_:Y')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#alice>'(3,['_:X','_:Y'])
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#kg>'('_:X','_:Y')
    )
).

% Bob
'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#bob>'(['_:X','_:Y'],0)
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#gk>'('_:X','_:Y')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#bob>'(['_:X','_:Y'],1)
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#k>'('_:X','_:Y')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#bob>'(['_:X','_:Y'],2)
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#g>'('_:X','_:Y')
    )
).

'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'(['_:X','_:Y'],
    (
        '<http://www.w3.org/2000/10/swap/log#negativeTriple>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#bob>'(['_:X','_:Y'],3)
        ),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#id>'('_:X','_:Y')
    )
).

% question
'<http://www.w3.org/2000/10/swap/log#onQuestionSurface>'(['_:N','_:A','_:B','_:M','_:X','_:Y','_:Z','_:L','_:S','_:I'],
    (
        '<http://eyereasoner.github.io/eye/reasoning/dqc#alice>'('_:N','_:A'),
        '<http://eyereasoner.github.io/eye/reasoning/dqc#bob>'('_:B','_:M'),
        '<http://www.w3.org/2000/10/swap/log#collectAllIn>'(
            [
                1,
                (
                    '<http://eyereasoner.github.io/eye/reasoning/dqc#r>'('_:X','_:Y'),
                    '<http://eyereasoner.github.io/eye/reasoning/dqc#alice>'('_:N',['_:X','_:Z']),
                    '<http://eyereasoner.github.io/eye/reasoning/dqc#bob>'(['_:Z','_:Y'],'_:M')
                ),
                '_:L'
            ],
            '_:S'
        ),

        % remove answers that appear an even number of times
        '<http://www.w3.org/2000/10/swap/list#length>'('_:L','_:I'),
        '<http://www.w3.org/2000/10/swap/math#remainder>'(['_:I',2],1),

        '<http://www.w3.org/2000/10/swap/log#onAnswerSurface>'([],
            '<http://eyereasoner.github.io/eye/reasoning/dqc#sdcoding>'('_:N','_:M')
        )
    )
).
