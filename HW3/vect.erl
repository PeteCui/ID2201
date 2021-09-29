%%author: Zhanbo Cui
-module(vect).
%-export([zero/1,inc/2,merge/2,leq/2,clock/1,update/3,safe/2]).
-export([zero/1,zero/0,inc/2,merge/2,leq/2,clock/0,update/3,safe/2]).

%%init the vector clock
zero() ->
    [].
zero(Nodes)->
    Time = [{X,0} || X<- Nodes],
    Time.

inc(Name, Time)->
    %%increase the timestamp if find 
    case lists:keyfind(Name,1,Time) of
	{Name,Ti}->
	    lists:keyreplace(Name,1,Time,{Name,Ti+1});
    %%add the timestamp if not find
	false ->
	    [{Name,0}|Time]
    end.

%%create the maximum vect timestamp
merge([], Time)->
    Time;
merge([{Name,Ti}|Rest], Time)->
    case lists:keyfind(Name,1,Time) of
	%%if find entry in the timestamp
	{Name, Tj}->
	    %%compare each tuple and make up a new timestamp list
	    [{Name,max(Ti,Tj)}|merge(Rest, lists:keydelete(Name, 1, Time))];
	 false->
	    [{Name,Ti}|merge(Rest,Time)]
    end.

%%every tuple is compared
leq([],_)->
    true;
leq([{Name, Ti}|Rest],Time)->
    case lists:keyfind(Name, 1, Time) of
	{Name, Tj}->
	    if
		%%continue to compare the rest
		Ti =< Tj->
		    leq(Rest,lists:keydelete(Name, 1, Time));
		true ->
		    false
	    end;
	false ->
	    false
    end.

%%The initial clock will thus reflect that we have seen no messages at all
clock()->
    [].

%%update the clock
update(From, Time, Clock)->
    %%pick up the timestamp from the message
    Ti = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
	{From,_}->
	    lists:keyreplace(From, 1, Clock, Ti);
	 false->
	    [Ti|Clock]
    end.

%%the entry in Time is less than or equal to the entry in the CLock- then it is safe to log
safe(Time, Clock)->
    leq(Time, Clock).
    
				 


