% Demo Expert System for Wind Turbine Maintenance

:- op(1200, xfx, :+).

% Facts about specific turbines (id, age_yrs, last_maintenance_months, issues, efficiency)
turbine(t123, 5, 8, [], 92).
turbine(t456, 12, 15, [vibration], 78).
turbine(t789, 8, 7, [noise], 85).
turbine(t101, 15, 5, [], 65).

% Maintenance Rules
needs_maintenance(Turbine, time_based_maintenance) :+
    turbine(Turbine, _, LastMaint, _, _),
    LastMaint >= 12.    % Rule 1: Time-based maintenance

needs_maintenance(Turbine, age_based_maintenance) :+
    turbine(Turbine, Age, LastMaint, _, _),
    Age > 10,           % Rule 2: Age-based maintenance
    LastMaint >= 6.

needs_maintenance(Turbine, problem_based_maintenance) :+
    turbine(Turbine, _, _, Issues, _),
    Issues \= [].       % Rule 3: Problem-based maintenance

needs_maintenance(Turbine, performance_based_maintenance) :+
    turbine(Turbine, _, _, _, Efficiency),
    Efficiency < 80.    % Rule 4: Performance-based maintenance

% query
true :+ needs_maintenance(_, _).
