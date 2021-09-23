%%%% erl -sname sweden -setcookie routy -connect_all false

-module(test1).
-export([initring/0,addintf/0,broadcast/0,send/3]).

initring()->
%%     %build 3 cities in sweden           
    
    Pid1 = {r1,'sweden@ubuntu'},
    Name1 =stockholm,
    Pid2 = {r2,'sweden@ubuntu'},
    Name2 =uppsala,
    Pid3 = {r3,'sweden@ubuntu'},
    Name3 =lund,
    
    routy:start(Pid1, Name1),
    routy:start(Pid2, Name2),
    routy:start(Pid3, Name3).
%% add them into interface to construct network
addintf()->
    %%add into stockholm
    r1 ! {add ,uppsala, {r2, 'sweden@ubuntu'}},
    %%add into uppsala
    r2 ! {add ,lund, {r3, 'sweden@ubuntu'}},
    %%add into lund
    r3 ! {add ,stockholm, {r1, 'sweden@ubuntu'}}.
%%broadcast and automatic update
broadcast()->
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast.
%%send message
send(Reg,Dest,Message)->
    Reg ! {send,Dest,Message}.


    
    


