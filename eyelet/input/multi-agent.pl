% multi-agent example
% obligations, permissions, and prohibitions depend on the roles, goals and interactions between agents

:- op(1200, xfx, :+).

:- dynamic('urn:example:completed'/1).

% define agents
'urn:example:agent'('urn:example:agent1').
'urn:example:agent'('urn:example:agent2').

% define roles
'urn:example:role'('urn:example:agent1', 'urn:example:manager').
'urn:example:role'('urn:example:agent2', 'urn:example:employee').

% define tasks and deadlines
'urn:example:task'('urn:example:task1').
'urn:example:deadline'('urn:example:task1', 10).  % Deadline at time 10

% current time
'urn:example:time:current'(15).

% task assignment
'urn:example:assigned'('urn:example:task1', 'urn:example:agent2').

% task reporting
'urn:example:reported'('urn:example:task1', 'urn:example:agent2').

% obligations
'urn:example:obligatory'('urn:example:assign:task'(Manager, Employee, Task)) :-
    'urn:example:role'(Manager, 'urn:example:manager'),
    'urn:example:role'(Employee, 'urn:example:employee'),
    'urn:example:task'(Task),
    \+'urn:example:assigned'(Task, Employee).

'urn:example:obligatory'('urn:example:report:progress'(Employee, Task)) :-
    'urn:example:assigned'(Task, Employee),
    \+'urn:example:reported'(Task, Employee).

'urn:example:obligatory'('urn:example:complete:task'(Employee, Task)) :-
    'urn:example:assigned'(Task, Employee).

'urn:example:obligatory'('urn:example:escalate:task'(Manager, Task)) :-
    'urn:example:role'(Manager, 'urn:example:manager'),
    'urn:example:assigned'(Task, _Employee),
    'urn:example:deadline'(Task, Time),
    'urn:example:time:current'(T),
    T >= Time,
    \+'urn:example:completed'(Task).

% permissions
'urn:example:permitted'('urn:example:execute:task'(Employee, Task)) :-
    'urn:example:role'(Employee, 'urn:example:employee'),
    'urn:example:assigned'(Task, Employee).

% prohibitions
'urn:example:forbidden'('urn:example:modify:task'(Agent, Task)) :-
    \+'urn:example:assigned'(Agent, Task).

% conflict detection
'urn:example:conflict'(Action) :-
    'urn:example:obligatory'(Action),
    'urn:example:forbidden'(Action).

% resolve conflicts with priority
'urn:example:resolve:conflict'(Action) :-
    'urn:example:role'(_Agent, 'urn:example:manager'),
    'urn:example:conflict'(Action),
    'urn:example:obligatory'(Action).

% violations
'urn:example:violation'(Task) :-
    'urn:example:obligatory'('urn:example:complete:task'(_Employee, Task)),
    'urn:example:time:current'(T),
    'urn:example:deadline'(Task, Time),
    T > Time,
    \+'urn:example:completed'(Task).

% sanctions
'urn:example:sanction'(Employee) :-
    'urn:example:violation'(Task),
    'urn:example:assigned'(Task, Employee).

% query
true :+ 'urn:example:obligatory'(_Action).
true :+ 'urn:example:permitted'(_Action).
true :+ 'urn:example:forbidden'(_Action).
true :+ 'urn:example:violation'(_Task).
true :+ 'urn:example:sanction'(_Employee).
