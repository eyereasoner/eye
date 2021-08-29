% Test results for EYE Lateral

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
el(colors(el(this),[el(p1)-el(c4),el(p2)-el(c3),el(p3)-el(c2),el(p4)-el(c1),el(p5)-el(c1)])).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
el(exp([[-1,0],[0.5,0]],[6.123233995736766e-17,1.0])).
el(exp([[e,0],[0,pi]],[-1.0,1.224646799147353e-16])).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
el(derivative([(x+1)*((x^2+2)*(x^3+3)),x],(x^2+2)*(x^3+3)+ (x+1)*(2*x*(x^3+3)+ (x^2+2)*(3*x^2)))).
el(derivative([x/x/x/x/x/x/x/x/x/x,x],(((((((((x-x)/x^2*x-x/x)/x^2*x-x/x/x)/x^2*x-x/x/x/x)/x^2*x-x/x/x/x/x)/x^2*x-x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x/x)/x^2*x-x/x/x/x/x/x/x/x/x)/x^2)).
el(derivative([log(log(log(log(log(log(log(log(log(log(x)))))))))),x],1/x/log(x)/log(log(x))/log(log(log(x)))/log(log(log(log(x))))/log(log(log(log(log(x)))))/log(log(log(log(log(log(x))))))/log(log(log(log(log(log(log(x)))))))/log(log(log(log(log(log(log(log(x))))))))/log(log(log(log(log(log(log(log(log(x))))))))))).
el(derivative([x*x*x*x*x*x*x*x*x*x,x],((((((((x+x)*x+ x*x)*x+ x*x*x)*x+ x*x*x*x)*x+ x*x*x*x*x)*x+ x*x*x*x*x*x)*x+ x*x*x*x*x*x*x)*x+ x*x*x*x*x*x*x*x)*x+ x*x*x*x*x*x*x*x*x)).
el(integral([1*6*x^5,x],x^6)).
el(integral([1*sqrt(pi)/2*exp(- (x^2)),x],erf(x))).

wrapper(rdf,http://www.w3.org/1999/02/22-rdf-syntax-ns#).
wrapper(el,https://josd.github.io/eye/lateral/ns#).
rdf(type(el(z),el(n1))).
rdf(type(el(z),el(n10))).
rdf(type(el(z),el(n100))).
rdf(type(el(z),el(n1000))).
rdf(type(el(z),el(n10000))).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
el(easter(2021,[4,4])).
el(easter(2022,[4,17])).
el(easter(2023,[4,9])).
el(easter(2024,[3,31])).
el(easter(2025,[4,20])).
el(easter(2026,[4,5])).
el(easter(2027,[3,28])).
el(easter(2028,[4,16])).
el(easter(2029,[4,1])).
el(easter(2030,[4,21])).
el(easter(2031,[4,13])).
el(easter(2032,[3,28])).
el(easter(2033,[4,17])).
el(easter(2034,[4,9])).
el(easter(2035,[3,25])).
el(easter(2036,[4,13])).
el(easter(2037,[4,5])).
el(easter(2038,[4,25])).
el(easter(2039,[4,10])).
el(easter(2040,[4,1])).
el(easter(2041,[4,21])).
el(easter(2042,[4,6])).
el(easter(2043,[3,29])).
el(easter(2044,[4,17])).
el(easter(2045,[4,9])).
el(easter(2046,[3,25])).
el(easter(2047,[4,14])).
el(easter(2048,[4,5])).
el(easter(2049,[4,18])).
el(easter(2050,[4,10])).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
solve_equation(x^2-3*x+2=0,x,x=(- -3+sqrt(1))/(2*1)).
solve_equation(x^2-3*x+2=0,x,x=(- -3-sqrt(1))/(2*1)).
solve_equation(cos(x)*(1-2*sin(x))=0,x,x=acos(0)).
solve_equation(cos(x)*(1-2*sin(x))=0,x,x= - acos(0)).
solve_equation(cos(x)*(1-2*sin(x))=0,x,x=asin((1-0)/2)).
solve_equation(cos(x)*(1-2*sin(x))=0,x,x= 180-asin((1-0)/2)).
solve_equation(2^(2*x)-5*2^(x+1)+16=0,x,x=log((- -10+sqrt(36))/(2*1))/log(2)).
solve_equation(2^(2*x)-5*2^(x+1)+16=0,x,x=log((- -10-sqrt(36))/(2*1))/log(2)).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
fft([0,1,2,3,4,5,6,7],[n(64,op(+,49,63)),n(63,op(*,62,52)),n(62,w^7),n(61,op(+,42,60)),n(60,op(*,47,44)),n(59,op(+,31,58)),n(58,op(*,57,38)),n(57,w^5),n(56,op(+,11,55)),n(55,op(*,24,21)),n(54,op(+,49,53)),n(53,op(*,50,52)),n(52,op(+,34,51)),n(51,op(*,47,36)),n(50,w^3),n(49,op(+,26,48)),n(48,op(*,47,29)),n(47,w^6),n(46,op(+,42,45)),n(45,op(*,27,44)),n(44,op(+,15,43)),n(43,op(*,24,19)),n(42,op(+,5,41)),n(41,op(*,24,9)),n(40,op(+,31,39)),n(39,op(*,32,38)),n(38,op(+,34,37)),n(37,op(*,27,36)),n(36,op(+,16,35)),n(35,op(*,24,17)),n(34,op(+,12,33)),n(33,op(*,24,13)),n(32,w^1),n(31,op(+,26,30)),n(30,op(*,27,29)),n(29,op(+,6,28)),n(28,op(*,24,7)),n(27,w^2),n(26,op(+,1,25)),n(25,op(*,24,3)),n(24,w^4),n(23,op(+,11,22)),n(22,op(*,2,21)),n(21,op(+,15,20)),n(20,op(*,2,19)),n(19,op(+,16,18)),n(18,op(*,2,17)),n(17,a(7)),n(16,a(3)),n(15,op(+,12,14)),n(14,op(*,2,13)),n(13,a(5)),n(12,a(1)),n(11,op(+,5,10)),n(10,op(*,2,9)),n(9,op(+,6,8)),n(8,op(*,2,7)),n(7,a(6)),n(6,a(2)),n(5,op(+,1,4)),n(4,op(*,2,3)),n(3,a(4)),n(2,w^0),n(1,a(0))]).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
el(fibonacci(0,0)).
el(fibonacci(1,1)).
el(fibonacci(6,8)).
el(fibonacci(91,4660046610375530309)).
el(fibonacci(283,62232491515607091882574410635924603070626544377175485625797)).
el(fibonacci(3674,295872959797101479478634366815157108100573212705250690577871041398423606408217262643449728342664061812585639168722421830407677671667740585806703531229882783069925750619720511808616484846128237251921414441458265138672827487722512845223115526738192067144721087756159352711138340620702266509343657403678256247195010013499661223527119909308682062873140767135468966093474944529418214755911968500799987099146489838560114063096775586903976827512299123202488315139397181279903459556726060805948910609527571241968534269554079076649680403030083743420820438603816095671532163428933363322524736324029745871445486444623006627119156710782085648303485296149604974010598940800770684835758031137479033374229914629583184427269638360355586190323578625395157899987377625662075558684705457)).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
testgcc([1,1,1,1,1,1,1,1,1],s(0,0,0),[s(0,0,1),s(0,1,1),s(0,1,0),s(1,1,0),s(1,1,1),s(1,0,1),s(1,0,0),s(0,0,0),s(0,0,1)]).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
glass(p([[_3,b,c]],o)).
glass(p([[a,_3,_4],[a,_3,_4]],o)).
glass(p([[a,_3,_4],[_5,_3,_4]],o)).
glass(p([[r,_3,t]],o)).
glass(p(["def",[r,_3,t]],o)).
glass(p([[d,_3,f],"rst"],o)).
glass(p(["def","rst",[d,_3,f]],o)).
glass(p([[d,_3,f],[r,_4,t],"def"],o)).

wrapper(gps,'http://josd.github.io/eye/reasoning/gps/gps-schema#').
wrapper(el,'https://josd.github.io/eye/lateral/ns#').
gps(findpath(el(location(el(i1),el(oostende))),[[el(drive_gent_brugge),el(drive_brugge_oostende)],2400.0,0.01,0.9408,0.99,[5000.0,5.0,0.2,0.4,1]])).
gps(findpath(el(location(el(i1),el(oostende))),[[el(drive_gent_kortrijk),el(drive_kortrijk_brugge),el(drive_brugge_oostende)],4100.0,0.018,0.903168,0.9801,[5000.0,5.0,0.2,0.4,1]])).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
el(path(el(angers),el(nantes))).
el(path(el(paris),el(nantes))).
el(path(el(chartres),el(nantes))).
el(path(el(lemans),el(nantes))).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
move(14,left,centre,right).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
route([1,1],[9,8],[[[2,3],[4,5]],[[6,6],[8,8]]],[[1,1],[1,2],[2,2],[3,2],[4,2],[5,2],[5,3],[5,4],[5,5],[6,5],[7,5],[8,5],[9,5],[9,6],[9,7],[9,8]]).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
pi(100000,3.141592653589792).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
area([[3,2],[6,2],[7,6],[4,6],[5,5],[5,3],[3,2]],7.5).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
racines([[1,0],[-10,0],[35,0],[-50,0],[24,0]],[[4.000000007450581,0.0],[2.999999992549419,0.0],[1.999999992549419,0.0],[1.000000007450581,0.0]]).
racines([[1,0],[-9,-5],[14,33],[24,-44],[-26,0]],[[3.000000000000004,2.000000000000001],[5.000000000000005,0.9999999999999927],[-5.773159728050814e-15,1.000000000000003],[0.9999999999999962,1.000000000000004]]).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
primerange(0,100,[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]).
primerange(1000000,1000100,[1000003,1000033,1000037,1000039,1000081,1000099]).
totient(271,270).
totient(2718281,2718280).
totient(27182818284,8994800640).
totient(271828182845904,87459116512768).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
findall(p(_3,_4),p(_3,_4),[p(0,1),p(0,2),p(1,3),p(2,4)]),setof(_3,_4^p(_3,_4),[0,1,2]).
findall(p(_3,_4),p(_3,_4),[p(0,1),p(0,2),p(1,3),p(2,4)]),setof(_3,p(0,_3),[1,2]).
findall(p(_3,_4),p(_3,_4),[p(0,1),p(0,2),p(1,3),p(2,4)]),setof(g(_3,_4),_6^(p(_3,_6),p(_6,_4)),[g(0,3),g(0,4)]).

wrapper(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
wrapper(el,'https://josd.github.io/eye/lateral/ns#').
rdf(type(el(socrates),el(mortal))).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
tak(34,13,8,13).

wrapper(el,'https://josd.github.io/eye/lateral/ns#').
el(compute([1,0,1,0,0,1],[1,0,1,0,1,0,#])).
el(compute([1,0,1,1,1,1],[1,1,0,0,0,0,#])).
el(compute([1,1,1,1,1,1],[1,0,0,0,0,0,0,#])).
el(compute([],[1,#])).

