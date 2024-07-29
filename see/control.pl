% Examples of control systems.

% facts
'urn:example:measurement1'('urn:example:input1',[6,11]).
'urn:example:measurement1'('urn:example:disturbance2',[45,39]).

'urn:example:measurement2'('urn:example:input2',true).

'urn:example:measurement3'('urn:example:input3',56967).
'urn:example:measurement3'('urn:example:disturbance1',35766).

'urn:example:measurement4'('urn:example:output2',24).

'urn:example:query1'('urn:example:state1',80).

'urn:example:query2'('urn:example:state2',false).

'urn:example:query3'('urn:example:state3',22).

'urn:example:target2'('urn:example:output2',29).

% control system
'urn:example:control1'('urn:example:actuator1',A) :-
    'urn:example:measurement10'('urn:example:input1',B),
    'urn:example:measurement2'('urn:example:input2',true),
    'urn:example:measurement3'('urn:example:disturbance1',C),
    D is B*19.6,            % proportial part
    E is log(C)/log(10),    % compensation part
    A is D-E.               % simple feedforward control

'urn:example:control1'('urn:example:actuator2',A) :-
    'urn:example:measurement3'('urn:example:input3',_),
    'urn:example:query3'('urn:example:state3',B),
    'urn:example:measurement4'('urn:example:output2',C),
    'urn:example:target2'('urn:example:output2',D),
    E is D-C,               % error
    F is B-C,               % differential error
    G is 5.8*E,             % proportial part
    H is 7.3/E,             % nonlinear factor
    I is H*F,               % nonlinear differential part
    A is G+I.               % PND feedback control

'urn:example:measurement10'(A,B) :-
    'urn:example:measurement1'(A,[C,D]),
    C<D,
    B is (D-C)^0.5.

'urn:example:measurement10'(A,B) :-
    'urn:example:measurement1'(A,[B,C]),
    B >= C.

% query
query('urn:example:control1'(_A,_B)).

test :-
    query(Q),
    Q,
    write_term(Q,[numbervars(true),quoted(true),double_quotes(true)]),
    write('.\n'),
    fail;
    halt.
