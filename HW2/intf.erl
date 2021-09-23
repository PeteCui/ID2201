-module(intf).

-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

%%return an empty set of interfaces
new()->
    [].

%%add a new entry to the set and return the new set of interfaces
%% Name: symbolic name
%% Ref: a process reference
%% Pid: a process identifier
%% Intf: the list to store
add(Name, Ref, Pid, Intf)->
    lists:keystore(Name, 1, Intf, {Name, Ref, Pid}).

%%remove an entry given a name of an interface
%%return a new set of interfaces
remove(Name, Intf)->
    lists:keydelete(Name, 1, Intf).

%%find the process identifier given a name
%%return {ok, Pid} or "notfound"
lookup(Name, Intf)->
    case lists:keyfind(Name, 1 ,Intf) of
	{_,_,Pid}->
	    {ok, Pid};
	false ->
	    notfound
    end.

%%find the reference given a name
%%return {ok, Ref} or "notfound"
ref(Name, Intf)->
    case lists:keyfind(Name, 1 ,Intf) of
	{_,Ref,_}->
	    {ok, Ref};
	false ->
	    notfound
    end.

%%find the name of an entry given a reference and
%%return {ok, Name} or "notfound"
name(Ref, Intf)->
    case lists:keyfind(Ref, 2 ,Intf) of
	{Name,_,_}->
	    {ok, Name};
	false ->
	    notfound
    end.

%%return a list of all names
list(Intf)->
    %%Use list comprehensions to filter
    [Name || {Name,_,_} <- Intf].

%%send the message to all interface processes
broadcast(Message, Intf)->
    %%call para1(Fun) for each element in para2(List)
    lists:foreach(fun({_, _, Pid})-> Pid ! Message end, Intf).
    
