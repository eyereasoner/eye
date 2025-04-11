:- op(1200, xfx, :+).

answer('urn:example:goldbach'(4, [2, 2])).
answer('urn:example:goldbach'(8, [3, 5])).
answer('urn:example:goldbach'(16, [3, 13])).
answer('urn:example:goldbach'(32, [3, 29])).
answer('urn:example:goldbach'(64, [3, 61])).
answer('urn:example:goldbach'(128, [19, 109])).
answer('urn:example:goldbach'(256, [5, 251])).
answer('urn:example:goldbach'(512, [3, 509])).
answer('urn:example:goldbach'(1024, [3, 1021])).
answer('urn:example:goldbach'(2048, [19, 2029])).
answer('urn:example:goldbach'(4096, [3, 4093])).
answer('urn:example:goldbach'(8192, [13, 8179])).
answer('urn:example:goldbach'(16384, [3, 16381])).
answer('urn:example:goldbach'(32768, [19, 32749])).
answer('urn:example:goldbach'(65536, [17, 65519])).
answer('urn:example:goldbach'(131072, [13, 131059])).
answer('urn:example:goldbach'(262144, [5, 262139])).
answer('urn:example:goldbach'(524288, [19, 524269])).
answer('urn:example:goldbach'(1048576, [3, 1048573])).
answer('urn:example:goldbach'(2097152, [19, 2097133])).
answer('urn:example:goldbach'(4194304, [3, 4194301])).
answer('urn:example:goldbach'(8388608, [37, 8388571])).
answer('urn:example:goldbach'(16777216, [3, 16777213])).
answer('urn:example:goldbach'(33554432, [61, 33554371])).
answer('urn:example:goldbach'(67108864, [5, 67108859])).
answer('urn:example:goldbach'(134217728, [79, 134217649])).
answer('urn:example:goldbach'(268435456, [89, 268435367])).
answer('urn:example:goldbach'(536870912, [3, 536870909])).
answer('urn:example:goldbach'(1073741824, [41, 1073741783])).
answer('urn:example:goldbach'(2147483648, [19, 2147483629])).
answer('urn:example:goldbach'(4294967296, [5, 4294967291])).
answer('urn:example:goldbach'(8589934592, [79, 8589934513])).
answer('urn:example:goldbach'(17179869184, [41, 17179869143])).
answer('urn:example:goldbach'(34359738368, [31, 34359738337])).
answer('urn:example:goldbach'(68719476736, [5, 68719476731])).

step((true:+'urn:example:goldbach'(4, [_, _])), 'urn:example:goldbach'(4, [2, 2]), true).
step((true:+'urn:example:goldbach'(8, [_, _])), 'urn:example:goldbach'(8, [3, 5]), true).
step((true:+'urn:example:goldbach'(16, [_, _])), 'urn:example:goldbach'(16, [3, 13]), true).
step((true:+'urn:example:goldbach'(32, [_, _])), 'urn:example:goldbach'(32, [3, 29]), true).
step((true:+'urn:example:goldbach'(64, [_, _])), 'urn:example:goldbach'(64, [3, 61]), true).
step((true:+'urn:example:goldbach'(128, [_, _])), 'urn:example:goldbach'(128, [19, 109]), true).
step((true:+'urn:example:goldbach'(256, [_, _])), 'urn:example:goldbach'(256, [5, 251]), true).
step((true:+'urn:example:goldbach'(512, [_, _])), 'urn:example:goldbach'(512, [3, 509]), true).
step((true:+'urn:example:goldbach'(1024, [_, _])), 'urn:example:goldbach'(1024, [3, 1021]), true).
step((true:+'urn:example:goldbach'(2048, [_, _])), 'urn:example:goldbach'(2048, [19, 2029]), true).
step((true:+'urn:example:goldbach'(4096, [_, _])), 'urn:example:goldbach'(4096, [3, 4093]), true).
step((true:+'urn:example:goldbach'(8192, [_, _])), 'urn:example:goldbach'(8192, [13, 8179]), true).
step((true:+'urn:example:goldbach'(16384, [_, _])), 'urn:example:goldbach'(16384, [3, 16381]), true).
step((true:+'urn:example:goldbach'(32768, [_, _])), 'urn:example:goldbach'(32768, [19, 32749]), true).
step((true:+'urn:example:goldbach'(65536, [_, _])), 'urn:example:goldbach'(65536, [17, 65519]), true).
step((true:+'urn:example:goldbach'(131072, [_, _])), 'urn:example:goldbach'(131072, [13, 131059]), true).
step((true:+'urn:example:goldbach'(262144, [_, _])), 'urn:example:goldbach'(262144, [5, 262139]), true).
step((true:+'urn:example:goldbach'(524288, [_, _])), 'urn:example:goldbach'(524288, [19, 524269]), true).
step((true:+'urn:example:goldbach'(1048576, [_, _])), 'urn:example:goldbach'(1048576, [3, 1048573]), true).
step((true:+'urn:example:goldbach'(2097152, [_, _])), 'urn:example:goldbach'(2097152, [19, 2097133]), true).
step((true:+'urn:example:goldbach'(4194304, [_, _])), 'urn:example:goldbach'(4194304, [3, 4194301]), true).
step((true:+'urn:example:goldbach'(8388608, [_, _])), 'urn:example:goldbach'(8388608, [37, 8388571]), true).
step((true:+'urn:example:goldbach'(16777216, [_, _])), 'urn:example:goldbach'(16777216, [3, 16777213]), true).
step((true:+'urn:example:goldbach'(33554432, [_, _])), 'urn:example:goldbach'(33554432, [61, 33554371]), true).
step((true:+'urn:example:goldbach'(67108864, [_, _])), 'urn:example:goldbach'(67108864, [5, 67108859]), true).
step((true:+'urn:example:goldbach'(134217728, [_, _])), 'urn:example:goldbach'(134217728, [79, 134217649]), true).
step((true:+'urn:example:goldbach'(268435456, [_, _])), 'urn:example:goldbach'(268435456, [89, 268435367]), true).
step((true:+'urn:example:goldbach'(536870912, [_, _])), 'urn:example:goldbach'(536870912, [3, 536870909]), true).
step((true:+'urn:example:goldbach'(1073741824, [_, _])), 'urn:example:goldbach'(1073741824, [41, 1073741783]), true).
step((true:+'urn:example:goldbach'(2147483648, [_, _])), 'urn:example:goldbach'(2147483648, [19, 2147483629]), true).
step((true:+'urn:example:goldbach'(4294967296, [_, _])), 'urn:example:goldbach'(4294967296, [5, 4294967291]), true).
step((true:+'urn:example:goldbach'(8589934592, [_, _])), 'urn:example:goldbach'(8589934592, [79, 8589934513]), true).
step((true:+'urn:example:goldbach'(17179869184, [_, _])), 'urn:example:goldbach'(17179869184, [41, 17179869143]), true).
step((true:+'urn:example:goldbach'(34359738368, [_, _])), 'urn:example:goldbach'(34359738368, [31, 34359738337]), true).
step((true:+'urn:example:goldbach'(68719476736, [_, _])), 'urn:example:goldbach'(68719476736, [5, 68719476731]), true).
