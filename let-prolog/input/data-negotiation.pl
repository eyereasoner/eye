% data negotiation including policies

:- op(1200, xfx, :+).

:- dynamic(hasData/2).

% facts representing the data each agent has
hasData(agent1, [data1, data2, data3]).
hasData(agent2, [data4, data5, data6]).

% policies for agents
% agent1 can only request data4 or data6
policy(agent1, [request, Data]) :-
    member(Data, [data4, data6]).

% agent2 can not accept requests for data5
policy(agent2, [accept, Data]) :-
    Data \= data5.

% rules for negotiation
% agentA requests data from agentB
request_data(AgentA, AgentB, Data) :+
    hasData(AgentA, DataListA),
    hasData(AgentB, DataListB),
    member(Data, DataListB),
    \+ member(Data, DataListA),
    policy(AgentA, [request, Data]).

% agentB accepts the request if it has the data data and the policy allows it
accept_request(AgentB, AgentA, Data) :+
    hasData(AgentB, DataListB),
    member(Data, DataListB),
    policy(AgentB, [accept, Data]),
    % simulate data exchange by adding the data data to agentA's data list
    becomes(hasData(AgentA, DataListA), hasData(AgentA, [Data|DataListA])).

% agentB rejects the request if it does not have the data data or the policy does not allow it
reject_request(AgentB, _, Data) :+
    hasData(AgentB, DataListB),
    (   \+ member(Data, DataListB)
    ;   \+ policy(AgentB, [accept, Data])
    ).

% negotiation process
negotiate(AgentA, [AgentB, Data]) :+
    request_data(AgentA, AgentB, Data),
    (   accept_request(AgentB, AgentA, Data)
    ;   reject_request(AgentB, AgentA, Data)
    ).

% query
true :+ negotiate(agent1, [agent2, data4]).
true :+ negotiate(agent1, [agent2, data5]).
true :+ negotiate(agent1, [agent2, data7]).
