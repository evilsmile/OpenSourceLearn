-module(echo2).
-export([go/0, loop/0]).

go() ->
    register(echo2, spawn(echo2, loop, [])),
    register(go, self()),
    echo2 ! {self(), hello},
    receive
        {nowhere, _Msg} ->
            ok;
        {_Pid, Msg} ->
            io:format("~w~n", [Msg])
    end.

loop() ->
    receive
%%       {From, Msg} ->
%%           From ! {self(), Msg},
%%           loop();
%%       stop ->
%%           true
   after 
       3000 ->
            go ! {self(), wwwww}
    end.
