%%=========================================================
%% 单进程循环测试: LoopTimes是循环次数
%%
%% tc:ct(Module, Function, ArgsList, SpawnProcessesCount).
%%=========================================================

-module(tm).
-export([t/4, ct/4]).

tc(M, F, A) ->
    {Microsend, _} = timer:tc(M, F, A),
    Microsend.

distribution(List, Aver) ->
    distribution(List, Aver, 0, 0).

distribution([H|T], Aver, Greater, Less) ->
    case H > Aver of
       true ->
          distribution(T, Aver, Greater+1, Less);
        false ->
          distribution(T, Aver, Greater, Less+1)
    end;

distribution([], _Aver, Greater, Less) ->
    {Greater, Less}.


%%=========================================================
%% test: one process test N times
%%=========================================================
t(M, F, A, N) ->
    {Max, Min, Sum, Aver, Greater, Less} = loop({M, F, A}, N),
    io:format("==============================~n"),
    io:format("exeucte [~p] times of {~p, ~p, ~p}:~n", [N, M, F, A]),
    io:format("Maximum:~p(us)\t~p(s)~n", [Max, Max/1000000]),
    io:format("Maximum:~p(us)\t~p(s)~n", [Min, Min/1000000]),
    io:format("Sum:~p(us)\t~p(s)~n", [Sum, Sum/1000000]),
    io:format("Average:~p(us)\t~p(s)~n", [Aver, Aver/1000000]),
    io:format("Greater: ~p~nLess:~p~n", [Greater, Less]),
    io:format("==============================~n").

loop({M, F, A}, N) ->
    loop({M, F, A}, N, 1, 0, 0, 0, []).

loop({M, F, A}, N, I, Max, Min, Sum, List) when N >= I ->
    Microsend = tc(M, F, A),
    NewSum = Sum + Microsend,
    if 
        Max == 0 ->
            NewMax = NewMin = Microsend;
        Max < Microsend ->
            NewMax = Microsend,
            NewMin = Min;
        Min > Microsend ->
            NewMax = Max,
            NewMin = Microsend;
        true ->
            NewMax = Max,
            NewMin = Min
    end,
    loop({M, F, A}, N, I+1, NewMax, NewMin, NewSum, [Microsend|List]);

loop({_M, _F, _A}, N, _I, Max, Min, Sum, List) ->
    Aver = Sum / N,
    {Greater, Less} = distribution(List, Aver),
    {Max, Min, Sum, Aver, Greater, Less}.

%%=========================================================
%% Concurrency test: N processes each test one time
%%=========================================================
ct(M, F, A, N) ->
    {Max, Min, Sum, Aver, Greater, Less} = cloop({M, F, A}, N),
    io:format("==============================~n"),
    io:format("exeucte [~p] times of {~p, ~p, ~p}:~n", [N, M, F, A]),
    io:format("Maximum:~p(us)\t~p(s)~n", [Max, Max/1000000]),
    io:format("Maximum:~p(us)\t~p(s)~n", [Min, Min/1000000]),
    io:format("Sum:~p(us)\t~p(s)~n", [Sum, Sum/1000000]),
    io:format("Average:~p(us)\t~p(s)~n", [Aver, Aver/1000000]),
    io:format("Greater: ~p~nLess:~p~n", [Greater, Less]),
    io:format("==============================~n").


cloop({M, F, A}, N) ->
    CollectorPid = self(),
    ok = loop_spawn({M, F, A}, CollectorPid, N),
    collector(0, 0, 0, N, 1, []).

loop_spawn({M, F, A}, CollectorPid, N) when N>0 ->
    spawn_link(fun() -> worker({M, F, A}, CollectorPid) end),
    loop_spawn({M, F, A}, CollectorPid, N - 1);
loop_spawn(_, _, 0) ->
    ok.

collector(Max, Min, Sum, N, I, List) when N >= I ->
    receive
        {result, Microsend} ->
            NewSum = Sum + Microsend,
            if 
                Max == 0 ->
                    NewMax = NewMin = Microsend;
                Max < Microsend ->
                    NewMax = Microsend,
                    NewMin = Min;
                Min > Microsend ->
                    NewMax = Max,
                    NewMin = Microsend;
                true ->
                    NewMax = Max,
                    NewMin = Min
            end,
            collector(NewMax, NewMin, NewSum, N, I+1, [Microsend|List])
    after
        10000 ->
            ok
    end;
collector(Max, Min, Sum, N, _, List) ->
    Aver = Sum/N,
    {Greater, Less} = distribution(List, Aver),
    {Max, Min, Sum, Aver, Greater, Less}.

worker({M, F, A}, CollectorPid) ->
    Microsend = tc(M, F, A),
    CollectorPid ! {result, Microsend}.
