-module(vecttest).
-export([run/2]).

run(Sleep, Jitter)->
    %%four classmates in SEDS!
    %Log = mylogger:start([zhanbo, ziheng, tianyu, chengyang]),
    Log = vectlogger:start([zhanbo, ziheng, tianyu, chengyang]),
    A = vectworker:start(zhanbo, Log, 10,Sleep, Jitter),
    B = vectworker:start(ziheng, Log, 20, Sleep, Jitter),
    C = vectworker:start(tianyu, Log, 30, Sleep, Jitter),
    D = vectworker:start(chengyang, Log, 40, Sleep, Jitter),
    %%send the list of peers to each node
    vectworker:peers(A, [B,C,D]),
    vectworker:peers(B, [A,C,D]),
    vectworker:peers(C, [A,B,D]),
    vectworker:peers(D, [A,B,C]),
    timer:sleep(500),
    vectlogger:stop(Log),
    vectworker:stop(A),
    vectworker:stop(B),
    vectworker:stop(C),
    vectworker:stop(D).
