flag('quantify', 'http://josd.github.io/.well-known/genid/ojGNO_4HsZ:Tcfu0J2K:7vBLdRo#').
scope('<http://josd.github.io/eye/reasoning/n3p/simple.ttl>').
pfx((:),
                                                   '<http://example.org/try#>').
'<http://example.org/try#p1>'('<http://example.org/try#s1>', '<http://example.org/try#o1>').
'<http://example.org/try#p2>'('<http://example.org/try#s2>', literal('We', type('<http://www.w3.org/2001/XMLSchema#string>'))).
'<http://example.org/try#p3>'('<http://example.org/try#s3>', literal(run, lang(en))).
'<http://example.org/try#p4>'('<http://example.org/try#s4>', literal('LV', type('<http://example.org/try#dt>'))).
'<http://example.org/try#p5>'('<http://example.org/try#s5>', 5).
'<http://example.org/try#p6>'('<http://example.org/try#s6>', ['<http://example.org/try#a>', '<http://example.org/try#b>', '<http://example.org/try#c>']).
'<http://example.org/try#p7>'('<http://example.org/try#s7>', '<http://josd.github.io/.well-known/genid/ojGNO_4HsZ:Tcfu0J2K:7vBLdRo#bn_1>').
'<http://example.org/try#p8>'('<http://josd.github.io/.well-known/genid/ojGNO_4HsZ:Tcfu0J2K:7vBLdRo#bn_2>', '<http://example.org/try#o7>').
scount(8).
end_of_file.
