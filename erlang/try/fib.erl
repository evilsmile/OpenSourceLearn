%% 模块声明
-module(fib).
%% 包含要导出的函数列表，这里的定义给出了函数名和函数的元数
-export([fibo/1, printfibo/1]).

%% print fibo arg. and result. with function as parameter

printfibo(N) ->
    Res = fib:fibo(N),
    io:fwrite("~w ~w~n", [N, Res]).

%% -spec fibo(N::float()) -> N when is_subtype(N, float()).
-spec fibo(N::integer()) -> integer().
fibo(0) -> 0;
fibo(1) -> 1;
fibo(N) when N > 0 -> fibo(N-1) + fibo(N-2) .
