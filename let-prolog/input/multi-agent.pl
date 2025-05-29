% multi-agent example
% obligations, permissions, and prohibitions depend on the roles, goals and interactions between agents

:- op(1200, xfx, :+).

:- dynamic(completed/1).

% define agents
agent(agent1).
agent(agent2).

% define roles
role(agent1, manager).
role(agent2, employee).

% define tasks and deadlines
task(task1).
deadline(task1, 10).  % Deadline at time 10

% current time
time_current(15).

% task assignment
assigned(task1, agent2).

% task reporting
reported(task1, agent2).

% obligations
obligatory(assign_task(Manager, Employee, Task)) :-
    role(Manager, manager),
    role(Employee, employee),
    task(Task),
    \+assigned(Task, Employee).

obligatory(report_progress(Employee, Task)) :-
    assigned(Task, Employee),
    \+reported(Task, Employee).

obligatory(complete_task(Employee, Task)) :-
    assigned(Task, Employee).

obligatory(escalate_task(Manager, Task)) :-
    role(Manager, manager),
    assigned(Task, _Employee),
    deadline(Task, Time),
    time_current(T),
    T >= Time,
    \+completed(Task).

% permissions
permitted(execute_task(Employee, Task)) :-
    role(Employee, employee),
    assigned(Task, Employee).

% prohibitions
forbidden(modify_task(Agent, Task)) :-
    \+assigned(Agent, Task).

% conflict detection
conflict(Action) :-
    obligatory(Action),
    forbidden(Action).

% resolve conflicts with priority
resolve_conflict(Action) :-
    role(_Agent, manager),
    conflict(Action),
    obligatory(Action).

% violations
violation(Task) :-
    obligatory(complete_task(_Employee, Task)),
    time_current(T),
    deadline(Task, Time),
    T > Time,
    \+completed(Task).

% sanctions
sanction(Employee) :-
    violation(Task),
    assigned(Task, Employee).

% query
true :+ obligatory(_Action).
true :+ permitted(_Action).
true :+ forbidden(_Action).
true :+ violation(_Task).
true :+ sanction(_Employee).
