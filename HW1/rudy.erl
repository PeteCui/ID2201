-module(rudy).
-export([start/2, stop/0]).

start(Port, N) ->
    register(rudy, spawn(fun() -> init(Port, N) end)).%generate a process which init by a function.

stop() ->
    %exit(whereis(rudy), "time to die!").%whereis fun can get the pid of rudy.
    rudy ! stop.
init(Port, N)->
    Opt = [list,{active, false}, {reuseaddr, true}, {backlog,64}],
    case gen_tcp:listen(Port, Opt) of 
	{ok, Listen}->
	    create_handlers(Listen,N),
	    wait(),
	    gen_tcp:close(Listen),
	    ok;
	{error, Error} ->
	    io:format("rudy: error: ~w~n", [Error])
    end.

wait()->
    receive
	stop->
	    ok
    end.

create_handlers(Listen, N)->
    case N of
	0 ->
	    ok;
	N ->
	    spawn(fun()->handler(Listen) end),
	    create_handlers(Listen, N-1)
    end.
    
handler(Listen)->
    case gen_tcp:accept(Listen) of
	{ok, Client} ->
	    request(Client),
	    handler(Listen);
	{error, Error} ->
	    io:format("rudy: Listen socket error: ~w~n", [Error])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
	{ok, Str}->
	    Request = http:parse_request(Str),
	    Response = reply(Request),
	    gen_tcp:send(Client, Response),
	    gen_tcp:close(Client);
	{error, Error} ->
	    io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok(URI).


    


