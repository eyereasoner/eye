% data negotiation including policies

:- op(1200, xfx, :+).

:- dynamic('urn:example:hasData'/2).

% facts representing the data each agent has
'urn:example:hasData'('urn:example:agent1', ['urn:example:data1', 'urn:example:data2', 'urn:example:data3']).
'urn:example:hasData'('urn:example:agent2', ['urn:example:data4', 'urn:example:data5', 'urn:example:data6']).

% policies for agents
% agent1 can only request data4 or data6
'urn:example:policy'('urn:example:agent1', ['urn:example:request', Data]) :-
    member(Data, ['urn:example:data4', 'urn:example:data6']).

% agent2 can not accept requests for data5
'urn:example:policy'('urn:example:agent2', ['urn:example:accept', Data]) :-
    Data \= 'urn:example:data5'.

% rules for negotiation
% agentA requests data from agentB
request_data(AgentA, AgentB, Data) :+
    'urn:example:hasData'(AgentA, DataListA),
    'urn:example:hasData'(AgentB, DataListB),
    member(Data, DataListB),
    \+ member(Data, DataListA),
    'urn:example:policy'(AgentA, ['urn:example:request', Data]).

% agentB accepts the request if it has the data data and the policy allows it
accept_request(AgentB, AgentA, Data) :+
    'urn:example:hasData'(AgentB, DataListB),
    member(Data, DataListB),
    'urn:example:policy'(AgentB, ['urn:example:accept', Data]),
    % simulate data exchange by adding the data data to agentA's data list
    becomes('urn:example:hasData'(AgentA, DataListA), 'urn:example:hasData'(AgentA, [Data|DataListA])).

% agentB rejects the request if it does not have the data data or the policy does not allow it
reject_request(AgentB, _, Data) :+
    'urn:example:hasData'(AgentB, DataListB),
    (   \+ member(Data, DataListB)
    ;   \+ 'urn:example:policy'(AgentB, ['urn:example:accept', Data])
    ).

% negotiation process
'urn:example:negotiate'(AgentA, [AgentB, Data]) :+
    request_data(AgentA, AgentB, Data),
    (   accept_request(AgentB, AgentA, Data)
    ;   reject_request(AgentB, AgentA, Data)
    ).

% query
true :+ 'urn:example:negotiate'('urn:example:agent1', ['urn:example:agent2', 'urn:example:data4']).
true :+ 'urn:example:negotiate'('urn:example:agent1', ['urn:example:agent2', 'urn:example:data5']).
true :+ 'urn:example:negotiate'('urn:example:agent1', ['urn:example:agent2', 'urn:example:data7']).
