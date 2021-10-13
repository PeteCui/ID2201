-module(test2).

-compile(export_all).

-define(Timeout, 10000).

%one node only in the ring and let the four test machines 
%add N elements to the ring and then do a lookup of the
%elements
performance1(N)->
    N1 = test:start(node2),
    timer:sleep(500),
    spawn(fun() ->add_lookup(1, N, N1) end),
    spawn(fun() ->add_lookup(2, N, N1) end),
    spawn(fun() ->add_lookup(3, N, N1) end),
    spawn(fun() ->add_lookup(4, N, N1) end),
    N1.

%one machine to handle 4000 elements
performance2(N)->
    N1 = test:start(node2),
    timer:sleep(500),
    spawn(fun() ->add_lookup(N, N, N1) end),
    N1.

free_test(M, N, P)->
    Ms = lists:seq(1,M),
    lists:foreach(fun(Id) -> spawn(fun() ->add_lookup(Id, N, P) end) end, Ms).

%N test Machine add N elements into one known node
add_lookup(Id, N, P)->
    %wait for stabilization of ring
    T1 = now(),
    io:format("Tets Machine~w:~w add ~w elements~n",[Id,self(),N]),
    %add
    Keys = test:keys(N),
    test:add(Keys, P),
    %look_up
    test:check(Keys, P, 0 ,0),
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    io:format("Tets Machine~w: finish in ~w ms ~n", [Id, Done]).

hand_fail_demo()->
    N1 = test:start(node3),
    N2 = test:start(node3,N1),
    N3 = test:start(node3,N1),
    N4 = test:start(node3,N1),
    N5 = test:start(node3,N1),
    {N1,N2,N3,N4,N5}.

