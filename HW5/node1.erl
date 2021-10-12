-module(node1).
-compile([export_all]).

-define(Stabilize, 100).
-define(Timeout, 10000).

%In the presentation of Chord will keep a list of potential successor, but here pnly keep track of one more successor
%Qref = {Key, Pid}
node(Id, Predecessor, Successor) ->
    receive
        % a peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        % a new node informs us it might be our predecessor
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        % a predecessor request our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        % our successor informs us about its predecessor
        %the phase of handling the return status from successor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        %receive a stabilize message
        stabilize ->
            % send a request message to its successor
            stabilize(Successor),
            node(Id, Predecessor, Successor);

        % create a probe
        probe->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        %receive the probe
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        %forward the probe
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
        % strange messagess
        Error ->
    	    io:format("node:~w:receive strange message: ~w~n", [Id, Error])
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
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

%case 1: the successor is ourself
connect(Id, nil) ->
    {ok, {Id, self()}};
%case 2: the successor is an existing node in the ring
connect(_, Peer)->
    Qref = make_ref(),
    %get the key by communicating
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey}->
            {ok,{Skey,Peer}}
        after ?Timeout ->
            io:format("Time out: no response~n",[])
        end.

%set up a timer and send the request message to the successor after a predefined interval
schedule_stabilize()->
    timer:send_interval(?Stabilize, self(), stabilize).

% send a request message to its successor
stabilize({_, Spid}) ->
    Spid ! {request, self()}.

%send the predecessor to the peers
request(Peer, Predecessor)->
    case Predecessor of
        %% if the predecessor of this node is nil
        nil->
            Peer ! {status, nil};
        %% if there exist a predecessor
        {Pkey, Ppid}->
            Peer ! {status, {Pkey, Ppid}}
        end.

% A notify B means A propose that it might be B's proper predecessor.
% A node be notified should check by itself according to the proposal
notify({Nkey, Npid}, Id, Predecessor)->
    case Predecessor of
        nil ->
            {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    %predecessor is changed! Approval of proposal
                    {Nkey,Npid};
                false->
                    %Rejection of proposal
                    Predecessor
            end
    end.

% check if the predecessor of this node's successor is this node
stabilize(Pred, Id, Successor)->
    {Skey, Spid} = Successor,
    case Pred of
        %% If this is nil we should of course inform it about our existence
        nil->
            %notify
            Spid ! {notify, {Id, self()}},
            %successor is not changed
            Successor;
        %% If the predecessor of this node's successor is this node
        {Id, _}->
            %%successor is not changed
            Successor;
        %% If the predecessor of this node's successor is the successor
        {Skey,_}->
            %notify
            Spid ! {notify, {Id, self()}},
            %%successor is not changed
            Successor;

        %%If there is a new node as predecessor of this node's successor
        {Xkey, Xpid} ->
            %%check if this new node should be this node's successor
            case key:between(Xkey, Id, Skey) of
                % Id > Xkey > Skey
                true->
                    %% run stabilization again
                    stabilize({Xkey, Xpid}),
                    %%successor is changed to the new node
                    {Xkey, Xpid};
                % Xkey > Id > Skey
                false->
                    %%notify
                    Spid ! {notify, {Id, self()}},
                    %%successor is not changed
                    Successor
            end
end.

%create probe
create_probe(Id, Successor) ->
    {_, Spid} = Successor,
    %Start = erlang:system_time(micro_seconds),
    Start = now(),
    io:format("Start: ~w micro seconds~n", [Start]),
    Spid ! {probe, Id, [Id], Start}.

%forward probe
forward_probe(Ref, T, Nodes, Id, Successor) ->
    {_, Spid} = Successor,
    %%my laptop is so powerful, Ihave to add some delay fro testing
    %timer:sleep(1),
    %add our own process identifier to the list of nodes
    Spid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

%receive probe
remove_probe(Start, Nodes)->
    %Now = erlang:system_time(micro_seconds),
    Now = now(),
    io:format("End: ~w micro seconds~n", [Now]),
    Diff = timer:now_diff(Now,Start),
    io:format("Probe: ~w micro seconds~n", [Diff]),
    io:format("Ring structure: ~w~n Ring length~w~n", [Nodes,erlang:length(Nodes)]).
