% --------------------
% eyelet -- Jos De Roo
% --------------------

:- use_module(library(between)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(terms)).

:- op(1200, xfx, :+).

:- dynamic((:+)/2).
:- dynamic(answer/1).
:- dynamic(brake/0).
:- dynamic(closure/1).
:- dynamic(count/2).
:- dynamic(fuse/1).
:- dynamic(limit/1).
:- dynamic(step/3).

version('eyelet v1.4.0 (2025-04-22)').

% main goal
main :-
    assertz(closure(0)),
    assertz(limit(-1)),
    assertz(count(fm, 0)),
    assertz(count(mf, 0)),
    (   (_ :+ _)
    ->  true
    ;   version(Version),
        format(user_error, "~w~n", [Version]),
        halt(0)
    ),
    forall(
        (Conc :+ Prem),
        dynify((Conc :+ Prem))
    ),
    catch(eyelet, E,
        (   E = halt(Exit)
        ->  true
        ;   format(user_error, "*** ~w~n", [E]),
            Exit = 1
        )
    ),
    count(fm, Fm),
    (   Fm = 0
    ->  true
    ;   format(user_error, "*** fm=~w~n", [Fm])
    ),
    count(mf, Mf),
    (   Mf = 0
    ->  true
    ;   format(user_error, "*** mf=~w~n", [Mf])
    ),
    (   Exit = 0
    ->  true
    ;   true
    ),
    halt(Exit).

%
% eyelet
%
% 1/ select rule Conc :+ Prem
% 2/ prove Prem and if it fails backtrack to 1/
% 3/ if Conc = true assert answer + step
%    else if Conc = false output fuse + steps and stop
%    else if ~Conc assert Conc + step and retract brake
% 4/ backtrack to 2/ and if it fails go to 5/
% 5/ if brake
%       if not stable start again at 1/
%       else output answers + steps and stop
%    else assert brake and start again at 1/
%

eyelet :-
    (   (Conc :+ Prem),                         % 1/
        copy_term((Conc :+ Prem), Rule),
        Prem,                                   % 2/
        (   Conc = true                         % 3/
        ->  assert_conj(answer(Prem)),
            assert_conj(step(Rule, Prem, Conc))
        ;   (   Conc = false
            ->  format(":- op(1200, xfx, :+).~n~n", []),
                portray_clause(fuse(Prem)),
                (   step(_, _, _),
                    nl
                ->  forall(
                        step(R, P, C),
                        portray_clause(step(R, P, C))
                    )
                ;   true
                ),
                throw(halt(2))
            ;   (   Conc \= (_ :+ _)
                ->  skolemize(Conc, 0, _)
                ;   true
                ),
                \+ Conc,
                assert_conj(Conc),
                assert_conj(step(Rule, Prem, Conc)),
                retract(brake)
            )
        ),
        fail                                    % 4/
    ;   (   brake                               % 5/
        ->  (   closure(Closure),
                limit(Limit),
                Closure < Limit,
                NewClosure is Closure+1,
                becomes(closure(Closure), closure(NewClosure)),
                eyelet
            ;   format(":- op(1200, xfx, :+).~n~n", []),
                forall(
                    answer(P),
                    portray_clause(answer(P))
                ),
                (   step(_, _, _),
                    nl
                ->  forall(
                        step(R, P, C),
                        portray_clause(step(R, P, C))
                    )
                ;   true
                )
            )
        ;   assertz(brake),
            eyelet
        )
    ).

%
% built-ins
%

% graph
'http://www.w3.org/2000/10/swap/graph#list'(A, B) :-
    conj_list(A, B).

% list
'http://www.w3.org/2000/10/swap/list#append'(A, B) :-
    nonvar(A),
    append(A, B).

'http://www.w3.org/2000/10/swap/list#first'(A, B) :-
    nonvar(A),
    A = [B|_].

'http://www.w3.org/2000/10/swap/list#firstRest'([A|B], [A, B]).

'http://www.w3.org/2000/10/swap/list#in'(A, B) :-
    nonvar(B),
    member(A, B).

'http://www.w3.org/2000/10/swap/list#iterate'(A, [B, C]) :-
    nonvar(A),
    nth0(B, A, C).

'http://www.w3.org/2000/10/swap/list#last'(A, B) :-
    nonvar(A),
    append(_, [B], A).

'http://www.w3.org/2000/10/swap/list#length'(A, B) :-
    nonvar(A),
    length(A, B).

'http://www.w3.org/2000/10/swap/list#map'([A, B], C) :-
    nonvar(A),
    nonvar(B),
    findall(E,
        (   member(F, A),
            G =.. [B, F, E],
            G
        ),
        C
    ).

'http://www.w3.org/2000/10/swap/list#member'(A, B) :-
    nonvar(A),
    member(B, A).

'http://www.w3.org/2000/10/swap/list#memberAt'([A, B], C) :-
    nonvar(A),
    nth0(B, A, C).

'http://www.w3.org/2000/10/swap/list#remove'([A, B], C) :-
    nonvar(A),
    nonvar(B),
    findall(I,
        (   member(I, A),
            I \= B
        ),
        C
    ).

'http://www.w3.org/2000/10/swap/list#removeAt'([A, B], C) :-
    nonvar(A),
    nth0(B, A, D),
    findall(I,
        (   member(I, A),
            I \= D
        ),
        C
    ).

'http://www.w3.org/2000/10/swap/list#removeDuplicates'(A, B) :-
    nonvar(A),
    list_to_set(A, B).

'http://www.w3.org/2000/10/swap/list#rest'(A, B) :-
    nonvar(A),
    A = [_|B].

'http://www.w3.org/2000/10/swap/list#sort'(A, B) :-
    nonvar(A),
    sort(A, B).

%log
'http://www.w3.org/2000/10/swap/log#bound'(X, Y) :-
    (   nonvar(X)
    ->  Y = true
    ;   Y = false
    ).

'http://www.w3.org/2000/10/swap/log#call'(A, B) :-
    call(A),
    catch(call(B), _, false).

'http://www.w3.org/2000/10/swap/log#callWithCleanup'(A, B) :-
    call_cleanup(A, B).

'http://www.w3.org/2000/10/swap/log#callWithOptional'(A, B) :-
    call(A),
    (   \+catch(call(B), _, false)
    ->  true
    ;   catch(call(B), _, false)
    ).

'http://www.w3.org/2000/10/swap/log#collectAllIn'([A, B, C], D) :-
    stable(D),
    nonvar(B),
    catch(findall(A, B, E), _, E = []),
    E = C.

'http://www.w3.org/2000/10/swap/log#conjunction'(A, B) :-
    nonvar(A),
    conjoin(A, B).

'http://www.w3.org/2000/10/swap/log#equalTo'(X, Y) :-
    X = Y.

'http://www.w3.org/2000/10/swap/log#forAllIn'([A, B], C) :-
    stable(C),
    nonvar(A),
    nonvar(B),
    forall(A, B).

'http://www.w3.org/2000/10/swap/log#ifThenElseIn'([A, B, C], D) :-
    stable(D),
    nonvar(A),
    nonvar(B),
    nonvar(C),
    if_then_else(A, B, C).

'http://www.w3.org/2000/10/swap/log#includes'(X, Y) :-
    stable(X),
    !,
    nonvar(Y),
    call(Y).
'http://www.w3.org/2000/10/swap/log#includes'(X, Y) :-
    nonvar(X),
    nonvar(Y),
    X \= [_, _],
    conj_list(X, A),
    conj_list(Y, B),
    includes(A, B).

'http://www.w3.org/2000/10/swap/log#notEqualTo'(X, Y) :-
    X \== Y.

'http://www.w3.org/2000/10/swap/log#notIncludes'(X, Y) :-
    stable(X),
    !,
    nonvar(Y),
    \+call(Y).
'http://www.w3.org/2000/10/swap/log#notIncludes'(X, Y) :-
    nonvar(X),
    nonvar(Y),
    X \= [_, _],
    conj_list(X, A),
    conj_list(Y, B),
    \+includes(A, B).

'http://www.w3.org/2000/10/swap/log#rawType'(A, B) :-
    raw_type(A, C),
    C = B.

'http://www.w3.org/2000/10/swap/log#repeat'(A, B) :-
    nonvar(A),
    C is A-1,
    between(0, C, B).

'http://www.w3.org/2000/10/swap/log#skolem'(A, B) :-
    (   skolem(A, B)
    ->  true
    ;   var(B),
        bb_get(genid, C),
        genlabel('#t', D),
        atom_chars(D, E),
        append(["http://knowledgeonwebscale.github.io/.well-known/genid/", C, E], F),
        atom_chars(B, F),
        assertz(skolem(A, B))
    ).

'http://www.w3.org/2000/10/swap/log#uri'(X, Y) :-
    atom_chars(X, Y).

'http://www.w3.org/2000/10/swap/log#uuid'(X, Y) :-
    ground(X),
    'http://www.w3.org/2000/10/swap/log#uri'(X, U),
    (   uuid(U, Y)
    ->  true
    ;   uuidv4_string(Y),
        assertz(uuid(U, Y))
    ).

% math
'http://www.w3.org/2000/10/swap/math#absoluteValue'(X, Y) :-
    nonvar(X),
    getnumber(X, U),
    Y is abs(U).

'http://www.w3.org/2000/10/swap/math#acos'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U),
        Y is acos(U),
        !
    ;   nonvar(Y),
        getnumber(Y, V),
        X is cos(V)
    ).

'http://www.w3.org/2000/10/swap/math#asin'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U),
        Y is asin(U),
        !
    ;   nonvar(Y),
        getnumber(Y, V),
        X is sin(V)
    ).

'http://www.w3.org/2000/10/swap/math#atan'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U),
        Y is atan(U),
        !
    ;   nonvar(Y),
        getnumber(Y, V),
        X is tan(V)
    ).

'http://www.w3.org/2000/10/swap/math#atan2'([X, Y], Z) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    Z is atan(U/V).

'http://www.w3.org/2000/10/swap/math#ceiling'(X, Y) :-
    nonvar(X),
    getnumber(X, U),
    Y is ceiling(U).

'http://www.w3.org/2000/10/swap/math#cos'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U),
        Y is cos(U),
        !
    ;   nonvar(Y),
        getnumber(Y, V),
        X is acos(V)
    ).

'http://www.w3.org/2000/10/swap/math#degrees'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U),
        Y is U*180/pi,
        !
    ;   nonvar(Y),
        getnumber(Y, V),
        X is V*pi/180
    ).

'http://www.w3.org/2000/10/swap/math#difference'([X, Y], Z) :-
    (   nonvar(X),
        nonvar(Y),
        getnumber(X, U),
        getnumber(Y, V),
        Z is U-V,
        !
    ;   nonvar(X),
        nonvar(Z),
        getnumber(X, U),
        getnumber(Z, W),
        Y is U-W,
        !
    ;   nonvar(Y),
        nonvar(Z),
        getnumber(Y, V),
        getnumber(Z, W),
        X is V+W
    ).

'http://www.w3.org/2000/10/swap/math#equalTo'(X, Y) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    U =:= V.

'http://www.w3.org/2000/10/swap/math#exponentiation'([X, Y], Z) :-
    nonvar(X),
    getnumber(X, U),
    (   nonvar(Y),
        getnumber(Y, V),
        Z is U**V,
        !
    ;   nonvar(Z),
        getnumber(Z, W),
        W =\= 0,
        U =\= 0,
        Y is log(W)/log(U)
    ).

'http://www.w3.org/2000/10/swap/math#floor'(X, Y) :-
    nonvar(X),
    getnumber(X, U),
    Y is floor(U).

'http://www.w3.org/2000/10/swap/math#greaterThan'(X, Y) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    U > V.

'http://www.w3.org/2000/10/swap/math#integerQuotient'([X, Y], Z) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    (   V =\= 0
    ->  Z is round(floor(U/V))
    ;   throw(zero_division('http://www.w3.org/2000/10/swap/math#integerQuotient'([X, Y], Z)))
    ).

'http://www.w3.org/2000/10/swap/math#lessThan'(X, Y) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    U < V.

'http://www.w3.org/2000/10/swap/math#logarithm'([X, Y], Z) :-
    nonvar(X),
    getnumber(X, U),
    (   nonvar(Y),
        getnumber(Y, V),
        U =\= 0,
        V =\= 0,
        Z is log(U)/log(V),
        !
    ;   nonvar(Z),
        getnumber(Z, W),
        Y is U**(1/W)
    ).

'http://www.w3.org/2000/10/swap/math#max'(X, Y) :-
    ground(X),
    list_max(X, Y).

'http://www.w3.org/2000/10/swap/math#memberCount'(X, Y) :-
    nonvar(X),
    length(X, Y).

'http://www.w3.org/2000/10/swap/math#min'(X, Y) :-
    ground(X),
    list_min(X, Y).

'http://www.w3.org/2000/10/swap/math#negation'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U)
    ->  Y is -U
    ;   (   nonvar(Y),
            getnumber(Y, V)
        ->  X is -V
        )
    ).

'http://www.w3.org/2000/10/swap/math#notEqualTo'(X, Y) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    U =\= V.

'http://www.w3.org/2000/10/swap/math#notGreaterThan'(X, Y) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    U =< V.

'http://www.w3.org/2000/10/swap/math#notLessThan'(X, Y) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    U >= V.

'http://www.w3.org/2000/10/swap/math#product'(X, Y) :-
    ground(X),
    product(X, Y).

'http://www.w3.org/2000/10/swap/math#quotient'([X, Y], Z) :-
    (   nonvar(X),
        nonvar(Y),
        getnumber(X, U),
        getnumber(Y, V),
        (   V =\= 0
        ->  Z is U/V
        ;   throw(zero_division('http://www.w3.org/2000/10/swap/math#quotient'([X, Y], Z)))
        ),
        !
    ;   nonvar(X),
        nonvar(Z),
        getnumber(X, U),
        getnumber(Z, W),
        (   W =\= 0
        ->  Y is U/W
        ;   throw(zero_division('http://www.w3.org/2000/10/swap/math#quotient'([X, Y], Z)))
        ),
        !
    ;   nonvar(Y),
        nonvar(Z),
        getnumber(Y, V),
        getnumber(Z, W),
        X is V*W
    ).

'http://www.w3.org/2000/10/swap/math#radians'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U),
        Y is U*pi/180,
        !
    ;   nonvar(Y),
        getnumber(Y, V),
        X is V*180/pi
    ).

'http://www.w3.org/2000/10/swap/math#remainder'([X, Y], Z) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    (   V =\= 0
    ->  Z is U-V*floor(U/V)
    ;   throw(zero_division('http://www.w3.org/2000/10/swap/math#remainder'([X, Y], Z)))
    ).

'http://www.w3.org/2000/10/swap/math#rounded'(X, Y) :-
    nonvar(X),
    getnumber(X, U),
    Y is round(round(U)).

'http://www.w3.org/2000/10/swap/math#roundedTo'([X, Y], Z) :-
    nonvar(X),
    nonvar(Y),
    getnumber(X, U),
    getnumber(Y, V),
    F is 10**floor(V),
    Z is round(round(U*F))/F.

'http://www.w3.org/2000/10/swap/math#sin'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U),
        Y is sin(U),
        !
    ;   nonvar(Y),
        getnumber(Y, V),
        X is asin(V)
    ).

'http://www.w3.org/2000/10/swap/math#sum'(X, Y) :-
    ground(X),
    sum(X, Y).

'http://www.w3.org/2000/10/swap/math#tan'(X, Y) :-
    (   nonvar(X),
        getnumber(X, U),
        Y is tan(U),
        !
    ;   nonvar(Y),
        getnumber(Y, V),
        X is atan(V)
    ).

%
% support
%

% conj_list(?Conjunction,?List).
%   true if ?List contains all items of the ?Conjunction
%   e.g. conj_list((1,2,3),[1,2,3])
conj_list(true, []).
conj_list(A, [A]) :-
    A \= (_, _),
    A \= false,
    !.
conj_list((A, B), [A|C]) :-
    conj_list(B, C).

% sum(+ListOfNumbers,-SumOfNumbers)
%   true when the sum of ListOfNumbers is SumOfNumbers.
%   e.g. sum([1,2],3).
sum([], 0) :-
    !.
sum([A|B], C) :-
    getnumber(A, D),
    sum(B, E),
    C is D+E.

% product(+ListOfNumbers,-ProductOfNumbers)
%   true when the product of ListOfNumbers is ProductOfNumbers.
%   e.g. product([2,4],8).
product([], 1) :-
    !.
product([A|B], C) :-
    getnumber(A, D),
    product(B, E),
    C is D*E.

% includes(?ListA,?ListB)
%   true when every item of ListB is in ListA
%   e.g. includes([1,2,[5]],[[5],2]).
includes(_, []) :-
    !.
includes(X, [Y|Z]) :-
    member(Y, X),
    includes(X, Z).

% genlabel(+OldAtom,-NewAtom)
%   for each invocation of this built-in a new label will
%   be created for OldAtom by appending it with an increasing number
%   e.g. genlabel('A','A_1'),genlabel('A','A_2'),...
genlabel(A, B) :-
    (   bb_get(A, C)
    ->  D is C+1,
        bb_put(A, D),
        taglabel(A, D, B)
    ;   bb_put(A, 1),
        taglabel(A, 1, B)
    ).

% taglabel(+Atom,+Number,-Tag)
%   Tag is the result of appending Number to Atom
%   e.g. taglabel('A',1,'A_1')
taglabel(A, B, C) :-
    atom_chars(A, D),
    number_chars(B, E),
    append([D, "_", E], F),
    atom_chars(C, F).

% raw_type(+Term,-Type)
raw_type(A, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#List') :-
    list_si(A),
    !.
raw_type(A, 'http://www.w3.org/2000/10/swap/log#Literal') :-
    number(A),
    !.
raw_type(true, 'http://www.w3.org/2000/10/swap/log#Formula').
raw_type(false, 'http://www.w3.org/2000/10/swap/log#Formula').
raw_type(A, 'http://www.w3.org/2000/10/swap/log#Literal') :-
    atom(A),
    \+ sub_atom(A, 0, 2, _, '_:'),
    \+ (sub_atom(A, 0, 1, _, ''), sub_atom(A, _, 1, 0, '')),
    !.
raw_type(literal(_, _), 'http://www.w3.org/2000/10/swap/log#Literal') :-
    !.
raw_type((_, _), 'http://www.w3.org/2000/10/swap/log#Formula') :-
    !.
raw_type(A, 'http://www.w3.org/2000/10/swap/log#Formula') :-
    functor(A, B, C),
    dif_si(B, :),
    C >= 2,
    !.
raw_type(A, 'http://www.w3.org/2000/10/swap/log#LabeledBlankNode') :-
    sub_atom(A, _, 2, _, '_:'),
    !.
raw_type(A, 'http://www.w3.org/2000/10/swap/log#SkolemIRI') :-
    sub_atom(A, _, 19, _, '/.well-known/genid/'),
    !.
raw_type(_, 'http://www.w3.org/2000/10/swap/log#Other').

% getnumber(+Literal,-Number)
getnumber(A, A) :-
    number(A),
    !.
getnumber(literal(A, _), B) :-
    ground(A),
    atom_chars(A, C),
    catch(number_chars(B, C), _, fail).

% intersect(+Set1, +Set2, -Set3)
intersect([], _, []) :-
    !.
intersect([A|B], C, D) :-
    (   memberchk(A, C)
    ->  D = [A|E],
        intersect(B, C, E)
    ;   intersect(B, C, D)
    ).

% conjoin(+List, -Conj)
conjoin([X], X) :-
    !.
conjoin([true|Y], Z) :-
    conjoin(Y, Z),
    !.
conjoin([X|Y], Z) :-
    conjoin(Y, U),
    conj_append(X, U, V),
    (   ground(V)
    ->  conj_list(V, A),
        list_to_set(A, B),
        conj_list(Z, B)
    ;   Z = V
    ).

% conjunction append
conj_append(A, true, A) :-
    !.
conj_append((A, B), C, (A, D)) :-
    conj_append(B, C, D),
    !.
conj_append(A, B, (A, B)).

% assert conjunction
assert_conj((B, C)) :-
    assert_conj(B),
    assert_conj(C).
assert_conj(A) :-
    (   \+ A
    ->  assertz(A)
    ;   true
    ).

% skolemize
skolemize(Term, N0, N) :-
    term_variables(Term, Vars),
    skolemize_(Vars, N0, N).

skolemize_([], N, N) :-
    !.
skolemize_([Sk|Vars], N0, N) :-
    number_chars(N0, C0),
    atom_chars(A0, C0),
    atom_concat('sk_', A0, Sk),
    N1 is N0+1,
    skolemize_(Vars, N1, N).

% stable(+Level)
%   fail if the deductive closure at Level is not yet stable
stable(Level) :-
    limit(Limit),
    (   Limit < Level
    ->  becomes(limit(Limit), limit(Level))
    ;   true
    ),
    closure(Closure),
    Level =< Closure.

% linear implication
becomes(A, B) :-
    catch(A, _, fail),
    conj_list(A, C),
    forall(
        member(D, C),
        retract(D)
    ),
    conj_list(B, E),
    forall(
        member(F, E),
        assertz(F)
    ).

% make dynamic predicates
dynify(A) :-
    var(A),
    !.
dynify(A) :-
    atomic(A),
    !.
dynify([]) :-
    !.
dynify([A|B]) :-
    !,
    dynify(A),
    dynify(B).
dynify(A) :-
    A =.. [B|C],
    length(C, N),
    (   current_predicate(B/N)
    ->  true
    ;   functor(T, B, N),
        catch((assertz(T), retract(T)), _, true)
    ),
    dynify(C).

% debugging tools
fm(A) :-
    format(user_error, "*** ~q~n", [A]),
    count(fm, B),
    C is B+1,
    becomes(count(fm, B), count(fm, C)).

mf(A) :-
    forall(
        catch(A, _, fail),
        (   format(user_error, "*** ", []),
            portray_clause(user_error, A),
            count(mf, B),
            C is B+1,
            becomes(count(mf, B), count(mf, C))
        )
    ).
