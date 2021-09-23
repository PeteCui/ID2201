%%author: Zhanbo CUi
-module(dijkstra).

%-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).
-export([table/2, route/2]).

%%return the length of the shortest path to the node or 0
entry(Node, Sorted)->
    case lists:keyfind(Node,1,Sorted) of
	{_,Length,_}->
	    Length;
	false ->
	    0
    end.

%%Update new length and new gateway, return sorted list
replace(Node, N, Gateway, Sorted)->
    NewTuple = {Node, N, Gateway},
    %%Keyreplace function has some similarity with keystore, the only difference 
    %%is keystore will append the new tuple if there is no such tuple
    Unsorted = lists:keyreplace(Node,1,Sorted,NewTuple),
    %%I love erlang lists library! Too many tools, keysort help us sort the list conveniently
    lists:keysort(2,Unsorted).

%%update the list Sorted, if no entry is found then no new entry is added.
update(Node, N, Gateway, Sorted)->
    OldLength = entry(Node, Sorted),
    Output = if 
	OldLength > N ->
            replace(Node, N, Gateway, Sorted);
        OldLength =< N ->
	    Sorted
    end,
    Output.

%%construct a table given a sorted list of nodes, a map and a table constructed so far
%%case 1
iterate([],_,Table)->
    Table;
%%case 2
iterate([{_,inf,_}|_],_,Table)->
    Table;
%%case 3
iterate(Sorted, Map, Table)->
    %%take the first entry in the sorted list
    {FirstNode, N , Gate} = lists:nth(1,Sorted),
    %%find all nodes reachable from this entry in the map
    Nodes = map:reachable(FirstNode,Map),
    %%each of the node update the sorted list
    UpdatedSorted = lists:foldl(fun(X, Acc)->update(X, N+1, FirstNode, Acc) end, Sorted, Nodes),
    %%take the first entry and add into table
    NewTable = [{FirstNode, Gate}] ++  Table, %%The sequence affects the route table output
    %%delete the first entry in the sorted list
    NewSorted = lists:keydelete(FirstNode, 1, UpdatedSorted),
    iterate(NewSorted, Map, NewTable).

%%construct a routing table given the gateways and a map
table(Gateways, Map)->
    %%All nodes in map are set to dummy, length set to infinity, gateway set to unknown 
    IniList = lists:foldl(fun(X, Acc)-> Acc ++ [{X,inf,unknown}] end, [], map:all_nodes(Map)),
    %%update the entries of gateways in IniList
    Sorted = lists:foldl(fun(X, Acc)->update(X,0,X,Acc) end, IniList,Gateways),
    iterate(Sorted, Map,[]).

%%search the routing table and return the gateway suitable to route messages to a node
route(Node, Table)->
    case lists:keyfind(Node, 1, Table) of
	false->
	    notfound;
	{Node, Gateway} ->
	    {ok,Gateway}
    end.
    
    
    
    
    
    

	
    
