%%author Zhanbo Cui
-module(gms1).
-compile([export_all]).

-define(arghh, 100000).

leader(Id, Master, Slaves, Group)->
    receive
	%%a message either from its own master or from a peer node
	{mcast, Msg}->
	    %%multicast to all peers
		io:format("Leader:~w bcast message: ~w to ~w~n", [Id, Msg, Slaves]),
	    bcast(Id, {msg, Msg}, Slaves),
        %%send to application layer
	    Master ! Msg,
	    leader(Id, Master, Slaves, Group);
	%%a message from a peer or the master, that is a request from a node to join the group
	{join, Wrk, Peer} ->
	    io:format("leader ~w receive join request from its master_pid~w~n", [Id,Master]),
		%%add the new node and the master of this node at the end of the list of peers/group
	    NewSlaves = lists:append(Slaves,[Peer]),
	    NewGroup = lists:append(Group, [Wrk]),
		io:format("leader ~w announce: New member join!~nNewGroup ~w~nNewSlave~w~n", [Id, NewGroup,NewSlaves]),
		Master ! {view, NewGroup, [self()|NewSlaves]},
	    io:format("leader ~w bcast new view to all slaves~n", [Id]),
		bcast(Id,{view, [self()|NewSlaves], NewGroup}, NewSlaves),
	    leader(Id, Master, NewSlaves, NewGroup);
	stop ->
	    ok
    end.

slave(Id, Master, Leader, Slaves, Group)->
    receive
	{mcast, Msg}->
	    %%a request from the master to multicast a message, the message is forwarded to the leader
		Leader ! {mcast, Msg},
	    slave(Id, Master, Leader, Slaves, Group);
	
	{join, Wrk, Peer} ->
	    %%a request from the master to allow a new node to join the group, the message is forwarded to the leader
	    Leader ! {join, Wrk, Peer},
	    slave(Id, Master, Leader, Slaves, Group);
	{msg, Msg}->
	    %%a multicasted message from the leader, the message is sent to the master
		Master ! Msg,
	    slave(Id, Master, Leader, Slaves, Group);
	{view, [Leader|NewSlaves], NewGroup}->
	    %%a multicasted view from the leader, the view is delivered to the master process
		Master ! {view, NewGroup},
	    slave(Id, Master, Leader, NewSlaves, NewGroup);
	{state_request,Msg}->
		io:format("slave ~w receive a state_request from his master~n", [Id]),
		io:format("slave ~w send this state_request to leader~n", [Id]),
		Leader ! {mcast, Msg},
	    slave(Id, Master, Leader, Slaves, Group);
	stop ->
	    ok
   end.


%%initialize the first node as leader
start(Id)->
    Self = self(),
    {ok, spawn_link(fun()->init(Id, Self) end)}.

init(Id, Master)->
	io:format("slave ~w is the leader~n", [Id]),
    leader(Id, Master,[],[Master]).

%%initialize a node which join an existing group
start(Id, Group)->
    Self = self(),
    {ok, spawn_link(fun()->init(Id, Group, Self) end)}.

init(Id, Group, Master)->
    Self = self(),
	io:format("slave ~w send join to worker_pid:~w ~n", [Id, Group]),
    Group ! {join, Master, Self, Id},
    %%wait for the response
	io:format("slave ~w wait for view ~n", [Id]),
    receive
	{view, [Leader|NewSlaves], NewGroup}->
		io:format("slave ~w: got view ~n", [Id]),
	    %%send view to itself master process
		Master ! {view, NewGroup},
		io:format("slave ~w: send view to its master~n", [Id]),
	    slave(Id, Master, Leader, NewSlaves, NewGroup);
	Error ->
    	io:format("slave:~w:receive strange message: ~w~n", [Id, Error])
    end.

bcast(_, Msg, Nodes)->
	lists:foreach(fun(Node)-> Node ! Msg end, Nodes).




	    
	    



	    
	    
	    
	    
