:- op(1200, xfx, :+).

answer(goldbach(4, [2, 2])).
answer(goldbach(8, [3, 5])).
answer(goldbach(16, [3, 13])).
answer(goldbach(32, [3, 29])).
answer(goldbach(64, [3, 61])).
answer(goldbach(128, [19, 109])).
answer(goldbach(256, [5, 251])).
answer(goldbach(512, [3, 509])).
answer(goldbach(1024, [3, 1021])).
answer(goldbach(2048, [19, 2029])).
answer(goldbach(4096, [3, 4093])).
answer(goldbach(8192, [13, 8179])).
answer(goldbach(16384, [3, 16381])).
answer(goldbach(32768, [19, 32749])).
answer(goldbach(65536, [17, 65519])).
answer(goldbach(131072, [13, 131059])).
answer(goldbach(262144, [5, 262139])).
answer(goldbach(524288, [19, 524269])).
answer(goldbach(1048576, [3, 1048573])).
answer(goldbach(2097152, [19, 2097133])).
answer(goldbach(4194304, [3, 4194301])).
answer(goldbach(8388608, [37, 8388571])).
answer(goldbach(16777216, [3, 16777213])).
answer(goldbach(33554432, [61, 33554371])).
answer(goldbach(67108864, [5, 67108859])).
answer(goldbach(134217728, [79, 134217649])).
answer(goldbach(268435456, [89, 268435367])).
answer(goldbach(536870912, [3, 536870909])).
answer(goldbach(1073741824, [41, 1073741783])).
answer(goldbach(2147483648, [19, 2147483629])).
answer(goldbach(4294967296, [5, 4294967291])).
answer(goldbach(8589934592, [79, 8589934513])).
answer(goldbach(17179869184, [41, 17179869143])).
answer(goldbach(34359738368, [31, 34359738337])).

step((true:+goldbach(4, [_, _])), goldbach(4, [2, 2]), true).
step((true:+goldbach(8, [_, _])), goldbach(8, [3, 5]), true).
step((true:+goldbach(16, [_, _])), goldbach(16, [3, 13]), true).
step((true:+goldbach(32, [_, _])), goldbach(32, [3, 29]), true).
step((true:+goldbach(64, [_, _])), goldbach(64, [3, 61]), true).
step((true:+goldbach(128, [_, _])), goldbach(128, [19, 109]), true).
step((true:+goldbach(256, [_, _])), goldbach(256, [5, 251]), true).
step((true:+goldbach(512, [_, _])), goldbach(512, [3, 509]), true).
step((true:+goldbach(1024, [_, _])), goldbach(1024, [3, 1021]), true).
step((true:+goldbach(2048, [_, _])), goldbach(2048, [19, 2029]), true).
step((true:+goldbach(4096, [_, _])), goldbach(4096, [3, 4093]), true).
step((true:+goldbach(8192, [_, _])), goldbach(8192, [13, 8179]), true).
step((true:+goldbach(16384, [_, _])), goldbach(16384, [3, 16381]), true).
step((true:+goldbach(32768, [_, _])), goldbach(32768, [19, 32749]), true).
step((true:+goldbach(65536, [_, _])), goldbach(65536, [17, 65519]), true).
step((true:+goldbach(131072, [_, _])), goldbach(131072, [13, 131059]), true).
step((true:+goldbach(262144, [_, _])), goldbach(262144, [5, 262139]), true).
step((true:+goldbach(524288, [_, _])), goldbach(524288, [19, 524269]), true).
step((true:+goldbach(1048576, [_, _])), goldbach(1048576, [3, 1048573]), true).
step((true:+goldbach(2097152, [_, _])), goldbach(2097152, [19, 2097133]), true).
step((true:+goldbach(4194304, [_, _])), goldbach(4194304, [3, 4194301]), true).
step((true:+goldbach(8388608, [_, _])), goldbach(8388608, [37, 8388571]), true).
step((true:+goldbach(16777216, [_, _])), goldbach(16777216, [3, 16777213]), true).
step((true:+goldbach(33554432, [_, _])), goldbach(33554432, [61, 33554371]), true).
step((true:+goldbach(67108864, [_, _])), goldbach(67108864, [5, 67108859]), true).
step((true:+goldbach(134217728, [_, _])), goldbach(134217728, [79, 134217649]), true).
step((true:+goldbach(268435456, [_, _])), goldbach(268435456, [89, 268435367]), true).
step((true:+goldbach(536870912, [_, _])), goldbach(536870912, [3, 536870909]), true).
step((true:+goldbach(1073741824, [_, _])), goldbach(1073741824, [41, 1073741783]), true).
step((true:+goldbach(2147483648, [_, _])), goldbach(2147483648, [19, 2147483629]), true).
step((true:+goldbach(4294967296, [_, _])), goldbach(4294967296, [5, 4294967291]), true).
step((true:+goldbach(8589934592, [_, _])), goldbach(8589934592, [79, 8589934513]), true).
step((true:+goldbach(17179869184, [_, _])), goldbach(17179869184, [41, 17179869143]), true).
step((true:+goldbach(34359738368, [_, _])), goldbach(34359738368, [31, 34359738337]), true).
