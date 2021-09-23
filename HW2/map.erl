%%author Zhanbo Cui
-module(map).

-export([new/0,update/3,reachable/2,all_nodes/1]).

%%return an empty map(a empty list)
new()->
    [].

%%update the Map and remove old entry
update(Node, Links, Map)->
    NewTuple = {Node,Links},
    lists:keystore(Node,1,Map,NewTuple).

%%return the list of nodes directly reachable from Node
reachable(Node, Map)->
    case lists:keyfind(Node, 1, Map) of
	{_,NodeList}->
	    NodeList;
	false ->
	    []
    end.

%%returns a list of all nodes in the map.
all_nodes(Map)->
    lists:foldl(fun(X,Acc)->append_links(X,Acc) end, [], Map).

append_links({Node,Links},Acc)->
    %%Use List Comprehensions to generate a filter
    EntryList = [X || X <- lists:append([Node],Links), not lists:member(X, Acc)],
    lists:append([Acc,EntryList]).
    %%Be careful to use | which only merge when the header is element and tail is list.
    %%There are also a libarary function called flatten can help us handle deeplist problem! 
    

