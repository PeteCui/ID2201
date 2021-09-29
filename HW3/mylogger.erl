-module(mylogger).
-export([start/1, stop/1]).

start(Nodes)->
    %%atomic operation and link the calling process with new process
    spawn_link(fun()->init(Nodes) end).

stop(Logger)->
    Logger ! stop.

init(Nodes)->
    Logclock = time:clock(Nodes),
    Holdback_queue = [],
    loop(Logclock, Holdback_queue, 0).

loop(Logclock, Holdback_queue, Max)->
    receive{log, From, Time, Msg}->
	    %%when a new log message arrives it should update the clock
	    Newlogclock = time:update(From,Time,Logclock),
	    %%add the message to the hold-back queue
	    Added_queue = lists:append(Holdback_queue,[{From,Time, Msg}]),
	    %%go through the queue to find messages that are now safe to print
	    New_queue = checkQueue(Added_queue, Newlogclock),
	    %%save the maximum length
	    Newmax = max(Max,erlang:length(New_queue)),
	    loop(Newlogclock,New_queue,Newmax);
	stop ->
	    io:format("The maximum length of queue is :~w~n", [Max]),
	    ok
    end.

log(From, Time, Msg)->
    io:format("log: Time:~w From:~w ~p~n", [Time, From, Msg]).

checkQueue([],_)->
    [];
checkQueue(Queue, Logclock)->
    %%sort the queue
    Sorted_queue = lists:keysort(2,Queue),
    %%pick up the message with minimum timestamp
    [{From, Time, Msg}|T] = Sorted_queue,
    case time:safe(Time, Logclock) of
	true ->
	    log(From, Time, Msg),
	    checkQueue(T, Logclock);
	%%return the queue which contains all unsafe message 
	false ->
	    Unsafe_queue = lists:append([{From, Time, Msg}],T),
	    Unsafe_queue
    end.
