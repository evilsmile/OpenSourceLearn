-module(facserver).
-export([server/0]).

server() ->
    register(facserver, self()),
    facLoop().

facLoop() ->
    receive 
        {Pid, N} ->
            Pid ! {ok, fac(N)}
    end,
    facLoop().

fac(0) ->
    1;
fac(N) when N > 0 ->
    N * fac(N-1).
