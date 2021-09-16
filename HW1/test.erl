-module(test).
-export([bench/3, bench/5]).

%% Simulate sending requests from a client
bench(Host, Port, Test_time) ->
    bench(Host, Port, 1, 100, Test_time).
%% Simulate sending requests from many clients
bench(Host, Port, C, N, Test_time) ->
    Ini = 0,
    T = Test_time,
    bench(Host, Port, C, N, T, Test_time, Ini).

bench(Host, Port, C, N, T, Test_time, P) ->
    if
	T == 0->
	    io:format("Average time is ~w micro_seconds~n",[P div Test_time]);
	true ->
	    Start = erlang:system_time(micro_seconds),
	    parallel(Host, Port, C, N, self()),
	    collect(C),
	    Finish = erlang:system_time(micro_seconds),
	    Time = Finish -Start,
	   % io:format("Finish in ~w micro_seconds~n",[Time]),
	    Sum = P + Time,
	    bench(Host, Port, C, N, T-1,Test_time, Sum)
    end.
    
parallel(_, _, 0, _, _)->
    ok;
parallel(Host, Port, C, N, Ctrl)->
    spawn(fun()->run(N, Host, Port, Ctrl) end),
    parallel(Host, Port, C-1, N, Ctrl).

collect(0) ->
    ok;

collect(C) ->    
    receive 
	ok ->
	    collect(C-1)
    end.

run(N, Host, Port, Ctrl) ->
    if
        N == 0 ->
            Ctrl!ok; %tell the main process all requests are finished.
        true ->        
            request(Host, Port),
           % io:format("~w request ~w~n",[self(),N]),
            run(N-1, Host, Port,Ctrl)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("zhanbo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
	{ok, _} ->
	    ok;
	{error, Error} ->
	    io:format("test: error: ~w~n", [Error])
         end,
         gen_tcp:close(Server).
