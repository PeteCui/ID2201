%%author: Zhanbo Cui
-module(time).
-export([zero/0,inc/2,merge/2,leq/2,clock/1,update/3,safe/2]).

%%return an initial Lamport value
zero()->
    0.

%%return the time T incremented by one
inc(Name, T)->
    T+1.

%%take the maximum Lamport time stamp
merge(Ti, Tj)->
    max(Ti,Tj).
    
%%return true if Ti is less than or equal to Tj    
leq(Ti, Tj)->
    case Ti =< Tj of
	true->
	    true;
	false ->
	    false
    end.

%%return a clock that can keep tack of the timestamp of each ndoe
clock(Nodes)->
    initclock(Nodes,[]).

%%init mylogger clock recursively 
initclock([],Clock)->
    Clock;
initclock([H|T], Clock)->
    %%pick up the first node in the list
    NewClock=lists:append(Clock,[{H,0}]),
    initclock(T,NewClock).

%%return a clock that has been updated
update(Node, Time, Clock)->    
    NewClock = lists:keyreplace(Node, 1, Clock, {Node, Time}),
    NewClock.

%%compare the minimum timestamp in logger clock with the message timestamp
safe(Time,Clock)->
    [{_,Minimum}|_] = lists:keysort(2,Clock),
    %% +1 means every previous message has arrived, it is safe now.
    Threshold = Minimum +1,
    leq(Time, Threshold).



