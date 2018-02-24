-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
    register(mutex, spawn(?MODULE, init, [])).

stop() ->
    mutex ! stop.

wait() ->
    mutex ! {wait, self()},
    receive f_ok -> w_ok end.

signal() ->
    mutex ! {signal, self()},
    s_ok.

init() ->
    free().

free() ->
    receive
        {wait, Pid} ->
            Pid ! f_ok,
            busy(Pid);
        stop ->
            terminate()
    end.

busy(Pid) ->
    receive 
        {signal, Pid} ->
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after
        0 -> t_ok
    end.
