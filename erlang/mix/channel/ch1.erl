-module(ch1).

-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0]).

%% 启动频道服务
start() ->
    spawn(ch1, init, []).   %% 创建一个进程，调用init方法，参数为[]

%% 获取一个空闲的频道
alloc() ->
    ch1 ! {self(),  alloc},
    receive
        {ch1, Res} -> 
            Res
    end.

%% 释放一个频道
free(Ch) ->
    ch1 ! {free, Ch},
    ok.

%% 初始化频道
init() ->
    register(ch1, self()),   %% 注册名为ch1的进程
    Chs = channels(),    %% 调用channels方法获取 {_Allocated = [], _Free = [1,2,3..100]}. _Allocated表示已经占用的频道，_Free列表表示空闲的频道
    loop(Chs).

loop(Chs) ->
    receive
        {From, alloc} ->  %% 接收到一个频道消息
            {Ch, Chs2} = alloc(Chs),         %% 调用alloc/1方法获取一个未被占用的频道
            From ! {ch1, Ch},              %% 发送一个消息到调用者
            loop(Chs2);                    %% 继续等待消息
        {free, Ch} ->                      %% 接受一个释放频道Ch的消息
            Chs2 = free(Ch, Chs),          %% 调用free/2, 将该频道放入空闲频道列表 
            loop(Chs2)                     %% 继续等待消息
    end.

%% 初始化已被占用列表
channels() ->
    {_Allocated = [], _Free = lists:seq(1, 100)}.

%% 获取一个空闲列表，并放入已占用列表
alloc({Allocated, [H|T] = _Free}) ->
    {H, { [H|Allocated], T}}.


%% 将频道Ch从已占用频道列表中移除，放入空闲列表 
free(Ch, {Alloc, Free} = Channels) ->
    case lists:member(Ch, Alloc) of
        true ->
            {lists:delete(Ch, Alloc), [Ch|Free]};
        false ->
            Channels
    end.
