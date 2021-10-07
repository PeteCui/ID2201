%%author Zhanbo Cui
-module(gms4).
-compile([export_all]).

-define(timeout,100000).
-define(arghh, 100).

leader(Id, Master, N, Slaves, Group)->
    receive
	%%a message either from its own master or from a peer node
	{mcast, Msg}->
	    %%multicast to all peers
		io:format("~w leader:~w bcast message ~w: ~w to ~w~n", [self(),Id, N, Msg, Slaves]),
	    %bcast(Id, {msg, N, Msg}, Slaves),
        ackbcast(N, Msg, Slaves),
		%%send to application layer
	    Master ! Msg,
	    leader(Id, Master, N+1, Slaves, Group);
	%%a message from a peer or the master, that is a request from a node to join the group
	{join, Wrk, Peer} ->
	    io:format("leader ~w receive join request from its master_pid~w~n", [Id,Master]),
		%%add the new node and the master of this node at the end of the list of peers/group
	    NewSlaves = lists:append(Slaves,[Peer]),
	    NewGroup = lists:append(Group, [Wrk]),
		io:format("leader ~w announce: New member join!~nNewGroup ~w~nNewSlave~w~n", [Id, NewGroup,NewSlaves]),
		Master ! {view, NewGroup},
	    io:format("leader ~w bcast new view to all slaves~n", [Id]),
		bcast(Id,{view, N, [self()|NewSlaves], NewGroup}, NewSlaves),
	    leader(Id, Master, N+1, NewSlaves, NewGroup);
	stop ->
	    ok
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group)->
    receive
	{mcast, Msg}->
	    %%a request from the master to multicast a message, the message is forwarded to the leader
		Leader ! {mcast, Msg},
	    slave(Id, Master, Leader, N, Last, Slaves, Group);
	
	{join, Wrk, Peer} ->
	    %%a request from the master to allow a new node to join the group, the message is forwarded to the leader
	    Leader ! {join, Wrk, Peer},
	    slave(Id, Master, Leader, N, Last, Slaves, Group);
	%%old message
	{msg, I, _} when I < N->
		slave(Id, Master, Leader, N, Last, Slaves, Group);
	%%new message
	{msg, I, Msg} ->
		NewLast = {msg, I, Msg},
	    %%a multicasted message from the leader, the message is sent to the master
		Master ! Msg,
	    slave(Id, Master, Leader, I + 1, NewLast, Slaves, Group);
	
	{ack, I, _, Channel} when I < N->
		Channel ! {ack},
		slave(Id, Master, Leader, N, Last, Slaves, Group);
	%%new message
	{ack, N, Msg , Channel} ->
		%%send ack to channel
		Channel ! {ack},
		NewLast = {msg, N, Msg},
	    %%a multicasted message from the leader, the message is sent to the master
		Master ! Msg,
	    slave(Id, Master, Leader, N + 1, NewLast, Slaves, Group);
	
	{view, I, _, _} when I < N->
		slave(Id, Master, Leader, N, Last, Slaves, Group);
	{view, I, [Leader|NewSlaves], NewGroup}->
	    NewLast = {view, I, [Leader|NewSlaves], NewGroup},
		%%a multicasted view from the leader, the view is delivered to the master process
		Master ! {view, NewGroup},
	    slave(Id, Master, Leader, N + 1, NewLast, NewSlaves, NewGroup);
	{state_request,Msg}->
		io:format("slave ~w receive a state_request from his master~n", [Id]),
		io:format("slave ~w send this state_request to leader~n", [Id]),
		Leader ! {mcast, Msg},
	    slave(Id, Master, Leader, N, Last, Slaves, Group);
	{'DOWN', _Ref, process, Leader, _Reason}->
		election(Id, Master, N, Last, Slaves, Group);
	Error ->
    	io:format("slave ~w: strange message: ~w~n", [Id,Error]);
	stop ->
	    ok
   end.


%%initialize the first node as leader
start(Id)->
	Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()->init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master)->
	rand:seed(exs1024s, [Rnd, Rnd, Rnd]),
	io:format("slave ~w is the leader~n", [Id]),
    leader(Id, Master, 0, [], [Master]).

%%initialize a node which join an existing group
start(Id, Group)->
	Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()->init(Id, Group, Self, Rnd) end)}.

init(Id, Group, Master, Rnd)->
	rand:seed(exs1024s, [Rnd, Rnd, Rnd]),
    Self = self(),
	io:format("slave ~w send join to worker_pid:~w ~n", [Id, Group]),
    Group ! {join, Master, Self, Id},
    %%wait for the response
	io:format("slave ~w wait for view ~n", [Id]),
    receive
	{view, N, [Leader|NewSlaves], NewGroup}->
		Last = {view, N, [Leader|NewSlaves], NewGroup},
		io:format("slave ~w: got view ~n", [Id]),
	    %%send view to itself master process
		Master ! {view, NewGroup},
		io:format("slave ~w: send view to its master~n", [Id]),
		%% this node begain to monitor the Leader
		erlang:monitor(process, Leader),
	    slave(Id, Master, Leader, N+1, Last, NewSlaves, NewGroup);
	Error ->
    	io:format("slave:~w:receive strange message: ~w~n", [Id, Error])
    after ?timeout->
		Master ! {error, "no reply from leader!"}
	end.

bcast(Id, Msg, Nodes)->
	lists:foreach(fun(Node)-> Node ! Msg, crash(Id) end, Nodes).

%%create a new process for sending and wait ACK from slave node
ackbcast(_, _, [])->
		ok;
ackbcast(I, Msg, [H|T])->
	%%create new process for waiting to improve concurrency!!!
	spawn(fun() -> waitack(I, H, Msg) end),
	ackbcast(I, Msg, T).

waitack( I, Node, Msg)->
	Node ! {ack, I, Msg, self()},
	receive
	{ack} ->
		io:format("~w receive ack！~n",[self()]);
	Error ->
    	io:format("strange message: ~w~n", [Error])
	after 5000->
		waitack( I, Node, Msg)
	end.

crash(Id)->
	case rand:uniform(?arghh) of
		?arghh ->
			io:format("leader ~w: crash ~n", [Id]),
			exit(no_luck);
		_->
			ok
	end.

%%Select the first node in its lists of peers and elect this as the leader
%%[_|Group] the first element in Group list is the master of previous leader, so we dont care it 
election(Id, Master, N, Last, Slaves, [_|NewGroup])->
	Self = self(),
	case Slaves of
		%%if the first candidate is itself
		[Self|Rest]->
			%%resend the last message to all nodes in the group
			bcast(Id, Last, Rest),
			bcast(Id,{view, N,[Self|Rest], NewGroup}, Rest),
			Master ! {view, NewGroup},
			io:format("leader ~w: I am the president now! ~n", [Id]),
			%%update the sequence number
			leader(Id, Master, N+1, Rest, NewGroup);
		[Leader|Rest]->
			erlang:monitor(process, Leader),
			io:format("slave ~w: I am still a resident now! ~n", [Id]),
			slave(Id, Master, Leader, N, Last, Rest, NewGroup)
		end.



	    
	    



	    
	    
	    
	    
