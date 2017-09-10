-module(tut16).
-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong ! finished,
    io:format("ping finished~n", []);

ping(N) ->
    pong ! {ping, self()},
    receive
        pong ->
            io:format("ping received pong~n", [])
    end,
    ping(N-1).


pong() ->
    receive
        finished ->
            io:format("pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong recevied ping from ~w~n", [Ping_PID]),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    %% Erlang provides a mechanism fro processes to be given names so that these names can be used as identifier instead of pids.
    %% That is 'register' BIF (Build-In function).
    register(pong, spawn(tut16, pong, [])),
    spawn(tut16, ping, [3]).
