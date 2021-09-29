%%send a log entry to the logger everr time when send or receive a message
-module(worker).
-export([start/5, stop/1, peers/2]).

%%Sleep: how active the worker is sending messages
%%Jitter: a random delay between the sending of message and log entry
start(Name, Logger, Seed, Sleep, Jitter)->
    spawn_link(fun()->init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker)->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter)->
    %%Seeds random number
    rand:seed(exs1024s,[Seed, Seed, Seed]),
    receive
	{peers, Peers}->
	    MyTime = time:zero(),
	    loop(Name, Log, Peers, Sleep, Jitter, MyTime);
	stop ->
	    ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, MyTime)->
    %%returns a random integer uniformly distributed between 1 and N
    Wait = rand:uniform(Sleep),
    receive
	{msg, Time, Msg}->
	    %%compare Mytime with Time received
	    NewTime = time:inc(Name, time:merge(MyTime,Time)),
	    %%send log entry to logger when receive 
	    Log ! {log, Name, NewTime, {received, Msg}},
	    loop(Name, Log, Peers, Sleep, Jitter, NewTime);
	stop ->
	    ok;
	Error ->
	    Log ! {log, Name, time, {error, Error}}
    %%after Time -> if overtime, the following code will be executed
    after Wait ->
	    %%select a peer process to send a message
	    Selected = select(Peers),
	    NewTime = time:inc(Name, MyTime),
	    Message = {hello, rand:uniform(100)},
	    Selected ! {msg, NewTime, Message},
	    %%introduce some delay for experiemnt
	    jitter(Jitter),
	    Log ! {log, Name, NewTime, {sending, Message}},
	    loop(Name, Log, Peers, Sleep, Jitter, NewTime)
    end.

select(Peers)->
    %%pick up a peer randomly
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0)->
    ok;
jitter(Jitter) ->
    timer:sleep(rand:uniform(Jitter)).


	    
		
		
