-module(hist).

-export([new/1,update/3]).

%%return a new history: [{Name, counter},_Entry2,_Entry3]
new(Name)->
   [{Name, 0}].

%%check if message is old or new
%%return old or {new, Updated}
update(Node, N, History)->
    case lists:keyfind(Node, 1, History) of
	%%No history entry
	false->
	    {new, [{Node,N}] ++ History};
	
        {Name,Counter}->
	    if
		N > Counter->
		    Updated = lists:keyreplace(Name, 1, History, {Name, N}),
		    {new, Updated};
		 true ->
		    old
	    end
    end.
	
