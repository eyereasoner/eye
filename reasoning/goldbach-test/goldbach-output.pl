:- op(1200, xfx, :+).

answer('<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(8, [3, 5])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(16, [3, 13])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(32, [3, 29])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(64, [3, 61])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(128, [19, 109])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(256, [5, 251])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(512, [3, 509])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(1024, [3, 1021])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(2048, [19, 2029])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(4096, [3, 4093])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(8192, [13, 8179])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(16384, [3, 16381])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(32768, [19, 32749])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(65536, [17, 65519])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(131072, [13, 131059])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(262144, [5, 262139])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(524288, [19, 524269])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(1048576, [3, 1048573])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(2097152, [19, 2097133])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(4194304, [3, 4194301])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(8388608, [37, 8388571])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(16777216, [3, 16777213])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(33554432, [61, 33554371])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(67108864, [5, 67108859])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(134217728, [79, 134217649])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(268435456, [89, 268435367])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(536870912, [3, 536870909])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(1073741824, [41, 1073741783])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(2147483648, [19, 2147483629])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(4294967296, [5, 4294967291])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(8589934592, [79, 8589934513])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(17179869184, [41, 17179869143])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(34359738368, [31, 34359738337])).
answer('<https://eyereasoner.github.io/ns#goldbach>'(68719476736, [5, 68719476731])).

% proof steps
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(4, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(4, [2, 2]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(8, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(8, [3, 5]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(16, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(16, [3, 13]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(32, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(32, [3, 29]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(64, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(64, [3, 61]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(128, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(128, [19, 109]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(256, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(256, [5, 251]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(512, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(512, [3, 509]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(1024, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(1024, [3, 1021]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(2048, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(2048, [19, 2029]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(4096, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(4096, [3, 4093]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(8192, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(8192, [13, 8179]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(16384, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(16384, [3, 16381]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(32768, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(32768, [19, 32749]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(65536, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(65536, [17, 65519]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(131072, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(131072, [13, 131059]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(262144, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(262144, [5, 262139]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(524288, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(524288, [19, 524269]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(1048576, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(1048576, [3, 1048573]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(2097152, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(2097152, [19, 2097133]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(4194304, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(4194304, [3, 4194301]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(8388608, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(8388608, [37, 8388571]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(16777216, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(16777216, [3, 16777213]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(33554432, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(33554432, [61, 33554371]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(67108864, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(67108864, [5, 67108859]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(134217728, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(134217728, [79, 134217649]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(268435456, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(268435456, [89, 268435367]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(536870912, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(536870912, [3, 536870909]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(1073741824, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(1073741824, [41, 1073741783]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(2147483648, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(2147483648, [19, 2147483629]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(4294967296, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(4294967296, [5, 4294967291]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(8589934592, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(8589934592, [79, 8589934513]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(17179869184, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(17179869184, [41, 17179869143]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(34359738368, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(34359738368, [31, 34359738337]), true).
step((true:+'<https://eyereasoner.github.io/ns#goldbach>'(68719476736, [_, _])), '<https://eyereasoner.github.io/ns#goldbach>'(68719476736, [5, 68719476731]), true).
