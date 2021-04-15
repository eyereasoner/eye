% calculate easter date

easter(Year, Month, Day) :-
    U is (19*(Year rem 19)+Year//100-Year//400-(Year//100-(Year//100+8)//25+1)//3+15) rem 30,
    V is (32+2*Year//100 rem 4+2*(Year rem 100//4)-U-Year rem 100 rem 4) rem 7,
    Month is (U+V-7*((Year rem 19+11*U+22*V)//451)+114)//31,
    Day is (U+V-7*((Year rem 19+11*U+22*V)//451)+114) rem 31+1.

% test cases
case(easter(Year, _, _)) :-
    between(2021, 2050, Year).

test :-
    case(A),
    A,
    write('[ :trealla-predicate "'),
    write(A),
    write('"].'),
    nl,
    fail.
test :-
    halt.
