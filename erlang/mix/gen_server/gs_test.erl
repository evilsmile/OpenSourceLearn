-module(gs_test).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, test/0]).
-export([start_link/0]).
-export([alloc/0, free/1]).

%% gen_server:start_link是同步的。只有等到gen_server被完全初始化并准备接受请求之后都会返回
%% 如果gen_server是某棵监督树的一部分，即gen_serer是由一个督程启动的，那必须使用start_link().
%% 另外, 另外一个函数gen_server:start用于启动一个独立的gen_server，即不是某棵监督树一部分的gen_server
start_link() ->
    gen_server:start_link({local, gs_test}, gs_test, [init_param], [[param2, param3]]).

%% 注册名称成功之后，新的gen_server进程会调用init()
%% init返回{ok, State}，其中State是gen_server的内部状态。在这里就是可用的channels
init(_Arg) ->
    {ok, channels()}.

%% 同步请求alloc()用gen_server:call/2实现。gs_test是gen_server的名字，必须和启动时的一样。
%% alloc是实际的请求。此时，请求以消息的形式发送给这个gen_server。当收到请求之后，gen_server
%% 调用handle_call(Request,From, State)，它返回一个元组{reply, Reply, State1}。Reply是需要回馈
%% 给客户端的答复。同时State2是gen_server的状态的新值
alloc() ->
    gen_server:call(gs_test, alloc).

%% 异步请求free(ch)使用gen_server:cast/2实现
%% gs_test是gen_server的名称。{free, Ch} 是实际请求
free(Ch) ->
    gen_server:cast(gs_test, {free, Ch}).

handle_call(_Request, _From, State) ->
    {Ch, State2} = alloc(State),
    {reply, Ch, State2}.

%% handle_cast中的应答是分配了的频道Ch，然后gen_server将等待新的请求，并且现在保持了一个
%% 最新可用频道的列表
handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.

channels() ->
    {_Allocated=[], _Free=lists:seq(1,100)}.

alloc({Allocated, _Free=[H|T]}) ->
    {H, {[H|Allocated], T}}.

free(Ch, {Allocated, Free} = Channels) ->
    case lists:member(Ch, Allocated) of
        true ->
            {lists:delete(Ch, Allocated), [Ch|Free]};
        false ->
            Channels
    end.

test() ->
    NewAllocated = [alloc()],
    io:format("Alloc new channel ~p. Now free it~n", [NewAllocated]),
    NewAllocated2 = [alloc() | NewAllocated],
    io:format("Alloc new channel ~p. Now free it~n", [NewAllocated2]),
    lists:foreach(fun(X) -> 
                          io:format("free channel ~p~n", [X]),
                          free(X)
                  end, NewAllocated2),
    NewCh = alloc(),
    io:format("Last alloc new channel ~p.~n", [NewCh]).
