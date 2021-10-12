-module(key).
-compile([export_all]).

-define(Bits,1000000000).

%%return a random number
generate()->
    rand:uniform(?Bits).

%%check if a Key is in (From, To]
between(Key, From, To)->
    if
        From == To->
            true;
        %From < To
        (From < Key) and (Key =< To) and (From < To)->
            true;
        %From > To
        ((Key > From) or (Key =< To)) and (To < From)->
            true;
         true ->
             false
    end.


