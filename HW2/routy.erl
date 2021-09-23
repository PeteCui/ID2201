-module(routy).

-export([start/2, stop/1, print/1]).

%%register the router process under a unique name
start(Pid, Name) ->
    {Reg,_} = Pid, 
    register(Reg, spawn(fun() -> init(Name, Pid) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name, Pid) ->
    %%initialize interface list
    Intf = intf:new(),
    %%NewIntf = Intf,
    %%add itself int interface list
    Ref = erlang:monitor(process, Pid),
    %%%add Ref into Interface list
    NewIntf = intf:add(Name, Ref, Pid, Intf),
    %%%
    %%initialize map
    Map = map:new(),
    %%initialize routing table, Intf records the information about the gateways(interface)
    Table = dijkstra:table(NewIntf, Map),
    %%initialize history
    Hist = hist:new(Name),
    io:format("~w is created!~n",[Name]),
    %%initialize the router
    router(Name, 0, Hist, NewIntf, Table, Map).

router(Name, N, Hist, Intf, Table, Map)->
    receive
	%% monitor a node and add a new entry to inter set
	{add, Node, Pid}->
	    %% return a reference
	    Ref = erlang:monitor(process, Pid),
	    %%add Ref into Interface list
	    NewIntf = intf:add(Node, Ref, Pid, Intf),
	    router(Name, N, Hist, NewIntf, Table, Map);
	
	{remove, Node}->
	    %% find the reference
	    {ok, Ref} = intf:ref(Node, Intf),
	    %% demonitor
	    erlang:demonitor(Ref),
	    %% remove this Node in the Interface list
	    NewIntf = intf:remove(Node, Intf),
	    router(Name, N, Hist, NewIntf, Table, Map);
	
	{'DOWN', Ref, process, _Pid2, _Reason}->
	    {ok, DownName} = intf:name(Ref, Intf),
	    Time = erlang:system_time(micro_seconds),
	    io:format("~w: exit received from ~w at ~w~n", [Name, DownName, Time]),
	    %%remove this node in the interface list
	    NewIntf = intf:remove(DownName, Intf),
	    router(Name, N, Hist, NewIntf, Table, Map);
	
	{status, From} ->
	    From ! {status,{Name, N, Hist, Intf, Table, Map}},
	    router(Name, N, Hist, Intf, Table, Map);

	{links, Node, R, Links}->	    
	    case hist:update(Node, R, Hist) of
		{new, NewHist}->
		    %%redirect this message
		    intf:broadcast({links, Node, R, Links}, Intf),
		    %%update map
		    NewMap = map:update(Node, Links, Map),
		    %%(add)update routing table
		    NewTable = dijkstra:table(intf:list(Intf), NewMap),
		    router(Name, N, NewHist, Intf, NewTable, NewMap);
		old->
		    %%ignore this link-state message
		    router(Name, N, Hist, Intf, Table, Map)
		end;

	%%update the routing table
	updateR->
	    NewTable = dijkstra:table(intf:list(Intf), Map),
	    router(Name, N, Hist, Intf, NewTable, Map);
	%%(add to prevent the head node can not receive the broadcast message )update map
	updateM->
	    NewMap = map:update(Name, intf:list(Intf), Map),
	    router(Name, N, Hist, Intf, Table, NewMap);
	%%manually order router to broadcast a link-state message
	broadcast->
	    Message = {links, Name, N, intf:list(Intf)},
	    intf:broadcast(Message, Intf),
	    router(Name, N+1, Hist, Intf, Table, Map);

	%%if this node is the final destination 
	{route, Name, From, Message}->
	    io:format("~w: received message ~w from ~w ~n", [Name, Message, From]),
	    router(Name, N, Hist, Intf, Table, Map);

	%%if the node is not the final destination
	{route, To, From, Message}->
	    io:format("~w: routing message (~w)~n",[Name, Message]),
	    case dijkstra:route(To, Table) of
		{ok, Gw}->
		    case intf:lookup(Gw, Intf) of
			{ok, Pid}->
			    Pid ! {route, To, From, Message};
			notfound->
			    ok
		    end;
		notfound->
		    ok
	    end,
	    router(Name, N, Hist, Intf, Table, Map);

	%%send message
	{send, To, Message} ->
	    self() ! {route, To, Name, Message},
	    router(Name, N, Hist, Intf, Table, Map);

	%%stop
	stop ->
	    Time = erlang:system_time(micro_seconds),
	    io:format("~w is terminated at ~w!~n",[Name, Time]),
	    ok
    end.

print(Reg)->
    Reg ! {status, self()},
    receive
	{status,{Name, N, Hist, Intf ,Table, Map}}->
	    io:format("Node name:~w~n",[Name]),
	    io:format("Counter: ~w~n",[N]),
	    io:format("History: ~w~n",[Hist]),
	    io:format("Intf:~w~n",[Intf]),
	    io:format("Table:~w~n",[Table]),
	    io:format("Map:~w~n",[Map]),
	    ok
    end.
	    
   
