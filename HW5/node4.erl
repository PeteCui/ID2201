-module(node4).
-compile([export_all]).

-define(Stabilize, 100).
-define(Timeout, 10000).

%In the presentation of Chord will keep a list of potential successor, but here pnly keep track of one more successor
%Qref = {Key, Pid}
node(Id, Predecessor, Successor, Store, Next, Replica) ->
    receive
        % a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next, Replica);
        % a new node informs us it might be our predecessor
        {notify, New} ->
            {Pred, {KeptStore, KeptReplic}} = notify(New, Id, Predecessor, Store, Replica),
            node(Id, Pred, Successor, KeptStore, Next, KeptReplic);
        % a predecessor request our predecessor
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Store, Next, Replica);
        % our successor informs us about its predecessor and next
        %the phase of handling the return status from successor
        {status, Pred, Nx} ->
            {Succ, NewNext} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Store, NewNext, Replica);
        %receive a stabilize message
        stabilize ->
            % send a request message to successor
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next, Replica);

        % create a probe
        probe->
            create_probe(Id, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next, Replica);
        %receive the probe
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next, Replica);
        %forward the probe
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next, Replica);
        
        %request to add a tuplue
        {add, Key, Value, Qref, Client} ->
            Added = addS(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next, Replica);

        %receive replicate from our predecessor
        {replicate, Key, Value, Qref, Client}->
            NewReplica = addR(Key, Value, Qref, Client, Id, Replica),
            node(Id, Predecessor, Successor, Store, Next, NewReplica);

        %request to lookup a tuple
        {lookup, Key, Qref, Client}->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next, Replica);

        %take over part of the responsibility
        {handover, {ElementsS, ElementsR}} ->
            MergedS = storage:merge(Store, ElementsS),
            MergedR = storage:merge(Store, ElementsR),
            node(Id, Predecessor, Successor, MergedS, Next, MergedR);

        %handling failures
        {'DOWN', Ref, process,_,_}->
            %return new {Predecessor, Successor, Next}
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            NewStore = recoverStore(Store, Replica),
            node(Id, Pred, Succ, NewStore, Nxt, storage:create());
        
        status ->
            io:format(" Predecessor: ~w~n
                       Successor: ~w~n
                       Next: ~w~n
                       Store: ~w~n
                       Replica: ~w~n",[Predecessor, Successor, Next, Store, Replica]),
            node(Id, Predecessor, Successor, Store, Next, Replica);
            
        % strange messagess
        Error ->
    	    io:format("node:~w:receive strange message: ~w~n", [Id, Error]),
            node(Id, Predecessor, Successor, Store, Next, Replica)
    end.

% we are the first node in the ring
start(Id) ->
    start(Id, nil).
% we’re connecting to an existing ring
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

% init the ring, set the predecessor and connect to our successor and schedule the stabilizing procedure
init(Id, Peer) ->
    Predecessor = nil,
    Next = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create(), Next, storage:create()).

%case 1: the successor is ourself
connect(Id, nil) ->
    {ok, {Id, nil, self()}};
%case 2: the successor is an existing node in the ring
connect(_, Peer)->
    Qref = make_ref(),
    %get the key by communicating
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey}->
            {ok,{Skey, monitor(Peer), Peer}}
        after ?Timeout ->
            io:format("Time out: no response~n",[])
        end.

%set up a timer and send the request message to the successor after a predefined interval
schedule_stabilize()->
    timer:send_interval(?Stabilize, self(), stabilize).

% send a request message to its successor
stabilize({_,_,Spid}) ->
    Spid ! {request, self()}.

%send the predecessor and next to the peers
%{Key, Ref, Pid}
request(Peer, Predecessor, Successor)->
    {Skey,_,Spid} = Successor,
    case Predecessor of
        %% if the predecessor of this node is nil
        nil->
            %we still send only the two element tuple 
            %{Key, Pid} since the receiving node has 
            %no use of the reference element of the 
            %sending node
            Peer ! {status, nil, {Skey, Spid}};
        %% if there exist a predecessor
        {Pkey,_,Ppid}->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
        end.

% A notify B means A propose that it might be B's proper predecessor.
% B node be notified should check by itself according to the proposal
notify({Nkey, Npid}, Id, Predecessor, Store, Replicate)->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Replicate, Nkey, Npid),
            Newref = monitor(Npid),
            {{Nkey, Newref, Npid}, Keep};
        {Pkey,Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    %predecessor is changed! Approval of proposal
                    Keep = handover(Id, Store, Replicate, Nkey, Npid),
                    drop(Pref),
                    Nref = monitor(Npid),
                    {{Nkey,Nref,Npid}, Keep};
                false->
                    %Rejection of proposal
                    {Predecessor, {Store,Replicate}}
            end
    end.

handover(Id, Store, Replicate, Nkey, Npid)->
    %k-v in (Nkey, Id] is "keep"
    %k-v in (Pkey, Nkey] is "NotKeep"
    {KeepS, NotKeepS} = storage:split(Nkey, Id, Store),
    {KeepR, NotKeepR} = storage:split(Nkey, Id, Replicate),
    Npid ! {handover, {NotKeepS,NotKeepR}},
    {KeepS, KeepR}.

% check if the predecessor of this node's successor is this node
stabilize(Pred, Nx, Id, Successor)->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        %% If this is nil we should of course inform it about our existence
        nil->
            %notify
            Spid ! {notify, {Id, self()}},
            %successor is not changed
            {Successor, Nx};
        %% If the predecessor of this node's successor is this node
        {Id, _}->
            %%successor is not changed
            {Successor, Nx};
        %% If the predecessor of this node's successor is the successor
        {Skey,_}->
            %notify
            Spid ! {notify, {Id, self()}},
            %%successor is not changed
            {Successor, Nx};

        %%If there is a new node as predecessor of this node's successor
        {Xkey, Xpid} ->
            %%check if this new node should be this node's successor
            case key:between(Xkey, Id, Skey) of
                % Id > Xkey > Skey
                true->
                    drop(Sref),
                    Newref = monitor(Xpid),
                    %% run stabilization again
                    stabilize({Xkey, Newref, Xpid}),
                    %%successor is changed to the new node
                    {{Xkey, Newref, Xpid}, {Skey, Spid}};
                % Xkey > Id > Skey
                false->
                    %%notify
                    Spid ! {notify, {Id, self()}},
                    %%successor is not changed
                    {Successor, Nx}
            end
end.

%create probe
create_probe(Id, Successor, Store) ->
    {_,_,Spid} = Successor,
    %Start = erlang:system_time(micro_seconds),
    Start = now(),
    %io:format("Start: ~w micro seconds~n", [Start]),
    %io:format("Node ~w store~w~n", [Id, Store]),
    Spid ! {probe, Id, [Id], Start}.

%forward probe
forward_probe(Ref, T, Nodes, Id, Successor, Store) ->
    {_,_,Spid} = Successor,
    %%my laptop is so powerful, Ihave to add some delay for testing
    %timer:sleep(1),
    %add our own process identifier to the list of nodes
    %io:format("Node ~w store~w~n", [Id, Store]),
    Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

%receive probe
remove_probe(Start, Nodes)->
    %Now = erlang:system_time(micro_seconds),
    %Diff = Now - Start,
    End =now(),
    Diff = (timer:now_diff(End, Start) div 1000),
    io:format("End: ~w ms~n", [Diff]),
    %io:format("End: ~w micro seconds~n", [Diff]),
    %Diff = timer:now_diff(Now,Start),
    %io:format("Probe: ~w micro seconds~n", [Diff]),
    io:format("Ring structure: ~w~n Ring length ~w~n", [Nodes,erlang:length(Nodes)]).


%(Key, Value, Qref, Client, Id, Predecessor, Successor, Store)
addS(Key, Value, Qref, Client, Id, {Pkey,_,_}, {_,_,Spid}, Store)->
    %(From, To]
    case key:between(Key, Pkey, Id) of
        %(Pkey, Id]
        true->
            Client ! {Qref, Id},
            %add new k-v into store 
            Added = storage:add(Key, Value, Store),
            %send replicate to our successor
            Spid ! {replicate, Key, Value, Qref, Client},
            
            Added;
        
        false->
            %forward this request to successor
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

%(Key, Value, Qref, Client, Replica)
addR(Key, Value, Qref, Client, Id, Replica)->
    Added = storage:add(Key, Value, Replica),
    Client ! {Qref, Id},
    Added.

%Key, Qref, Client, Id, Predecessor, Successor, Store)
lookup(Key, Qref, Client, Id, {Pkey,_,_}, {_, _,Spid}, Store)->
    %(From, To]
    %io:format("~w receive a lookup request for ~w~n", [Id,Key]),
    case key:between(Key, Pkey, Id) of
        true->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            %forward this request to successor
            Spid ! {lookup, Key, Qref, Client}
    end.

monitor(Pid) ->
    erlang:monitor(process, Pid).
%demonitor
drop(nil) ->
    ok;
drop(Pid)->
    erlang:demonitor(Pid, [flush]).

%match with Predecessor
down(Ref, {_, Ref, _}, Successor, Next) ->
    %Predecessor set to nil and wait 
    %other to request and notify me!
    {nil, Successor, Next};
%match with Successor
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    %Next become new Successor
    Nref = monitor(Npid),
    NewSuccessor = {Nkey,Nref,Npid},
    stabilize(NewSuccessor),
    {Predecessor, NewSuccessor, nil}.

%Replica should be merged with its own Store
recoverStore(Store, Replica)->
    storage:merge(Replica, Store).

