-module(tut15).
-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N-1, Pong_PID).

pong() ->
    %% 'receive' construct is used to allow processes to wait for messages 
    %% from other processes. 
    %% It has the following format:
    %% receive
    %%      pattern1 ->
    %%         actions1 ;
    %%      pattern2 ->
    %%         actions2 ;
    %%      ...
    %%      patternN ->
    %%          actionsN
    %% end.
    %% Messages are simply valid Erlang terms. That is, lists, tuples, integers, atoms, pids, and so on.
    %% Each process has its own input queue for messages it receives. New messsages received are put at the end of the queue.
    %% When a process executes a 'receive', the first message in the queue is matched against the first pattern in the 'receive'.
    %% If this matches, the message is removed from the queue and the actions corresponding to the pattern are executed.
    %% But if there is no match, the first message is kept in the queue and sencond message is tries instead.
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            %% Pid ! Message 
            %% Message is sent to the process with identity Pid.
            Ping_PID ! pong,
            pong()
    end .

start() ->
    %% Create a process, and call pong. And it's pid is Pong_PID
    Pong_PID = spawn(tut15, pong, []),
    spawn(tut15, ping, [3, Pong_PID]).


