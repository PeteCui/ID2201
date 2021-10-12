-module(storage).
-compile(export_all).

% create a local storage
create()->
    [].

%add a {Key, Value} into list
add(Key, Value, Store)->
    lists:append(Store,[{Key, Value}]).

%return a tuple {Key, Value} form list
lookup(Key, Store)->
    lists:keyfind(Key, 1, Store).

%return a tuple {Updated, Rest} where the updated store only contains the key value pairs requested and the rest are found in a list of key-value pairs
split(From, To, Store)->
    Updated = [{Key, Value} || {Key, Value} <- Store, key:between(Key, From, To)],
    Rest = [{Key, Value} || {Key, Value} <- Store, not key:between(Key, From, To)],
    {Updated, Rest}.

%add a list of key-value pairs to a store
merge(Entries, Store)->
    Store ++ Entries.