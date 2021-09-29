-module(test).
-export([run/2]).

run(Sleep, Jitter)->
    %%four classmates in SEDS!
    Log = mylogger:start([zhanbo, ziheng, tianyu, chengyang]),
    %Log = vectlogger:start([zhanbo, ziheng, tianyu, chengyang]),
    A = worker:start(zhanbo, Log, 10,Sleep, Jitter),
    B = worker:start(ziheng, Log, 20, Sleep, Jitter),
    C = worker:start(tianyu, Log, 30, Sleep, Jitter),
    D = worker:start(chengyang, Log, 40, Sleep, Jitter),
    %%send the list of peers to each node
    worker:peers(A, [B,C,D]),
    worker:peers(B, [A,C,D]),
    worker:peers(C, [A,B,D]),
    worker:peers(D, [A,B,C]),
    timer:sleep(5000),
    mylogger:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).
