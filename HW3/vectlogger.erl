%%author: Zhanbo
-module(vectlogger).
-export([start/1, stop/1]).

start(_)->
    %%atomic operation and link the calling process with new process
    spawn_link(fun()->init() end).

stop(Logger)->
    Logger ! stop.

init()->
    Logclock = vect:clock(),
    Holdback_queue = [],
    loop(Logclock, Holdback_queue, 0, 0).

loop(Logclock, Holdback_queue, Max, Count)->
    receive{log, From, Time, Msg}->
	    %%when a new log message arrives it should update the clock
	    Newlogclock = vect:update(From,Time,Logclock),
	    io:format("~w loop: Current Clock:~w ~n", [Count,Newlogclock]),
	    %%add the message to the hold-back queue
	    Newtime = lists:keysort(1,Time),
	    Added_queue = lists:append([{From,Newtime, Msg}],Holdback_queue),
	    %%go through the queue to find messages that are now safe to print
	    New_queue = checkQueue(Added_queue, Newlogclock),
	    %%save the maximum length
	    Newmax = max(Max,erlang:length(New_queue)),
	    Newcount = Count + 1,
	    loop(Newlogclock,New_queue,Newmax,Newcount);
	stop ->
	    io:format("The maximum length of queue is :~w~n", [Max]),
	    ok
    end.

log(From,Time, Msg)->
     io:format("Output Timestamp:~w From:~w ~n~p~n~n", [Time, From, Msg]).

checkQueue([],_)->
    [];
checkQueue(Queue, Logclock)->
    %%compare every message in the queue
    [{From, Time, Msg}|T] = Queue,
    %io:format("Time ~w and Clock ~w ~n",[Time,Logclock]),
    case vect:safe(Time, Logclock) of
	true ->
	    log(From, Time, Msg),
	    checkQueue(T, Logclock);
	%%return the queue which contains all unsafe message 
	false ->
	    Unsafe_queue = lists:append(T,[{From, Time, Msg}]),
	    Unsafe_queue
    end.
