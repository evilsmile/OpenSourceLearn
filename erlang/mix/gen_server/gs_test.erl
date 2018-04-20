-module(gs_test).
%% gen_server进程不能自动trap退出信号，这必须在回调模块中显式初始化.
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([alloc/0, free/1]).
-export([test/0]).

%% gen_server:start_link是同步的。只有等到gen_server被完全初始化并准备接受请求之后都会返回
%% 如果gen_server是某棵监督树的一部分，即gen_serer是由一个督程启动的，那必须使用start_link().
%% 另外, 另外一个函数gen_server:start用于启动一个独立的gen_server，即不是某棵监督树一部分的gen_server
start_link() ->
    gen_server:start_link({local, gs_test}, gs_test, [init_param], [[param2, param3]]).

%% 注册名称成功之后，新的gen_server进程会调用init()
%% init返回{ok, State}，其中State是gen_server的内部状态。在这里就是可用的channels
init(_Arg) ->
    Pid = self(),
    io:format("Pid: ~p~n", [Pid]),
    {ok, channels()}.

%% 同步请求alloc()用gen_server:call/2实现。gs_test是gen_server的名字，必须和启动时的一样。
%% alloc是实际的请求。此时，请求以消息的形式发送给这个gen_server。当收到请求之后，gen_server
%% 调用handle_call(Request,From, State)，它返回一个元组{reply, Reply, State1}。Reply是需要回馈
%% 给客户端的答复。同时State2是gen_server的状态的新值
%% gen_server:call(ServerRef, Request, Timeout) -> Reply
alloc() ->
    gen_server:call(gs_test, alloc).
%%    gen_server:call(gs_test, allock).

%% 异步请求free(ch)使用gen_server:cast/2实现
%% gs_test是gen_server的名称。{free, Ch} 是实际请求
free(Ch) ->
    gen_server:cast(gs_test, {free, Ch}).
  

%% ===================================== gen_server callbacks ========================
%% 有返回值的调用
%% _From是一个{Pid, Ref}元组, 代表来源进程号和调用的标识
%% State是服务器的状态,这是由init或其它的handle函数生成的，可以根据需要修改后再放回返回值
%% 返回值 {reply, Reply, NewState}, Reply会作为call的返回值传递回去，NewState作为服务器的新状态
%% 还可以用{stop, Reason, State}中止服务器运行
%% NOTE: 两个进程不能相互call，不然会死锁
handle_call(Request, _From, State) ->
    io:format("handle_call=> Module[~p] Request:[~p] From:[~p] ~n", [?MODULE, Request, _From]),
    case Request of 
        alloc ->
            {Ch, State2} = alloc(State),
            {reply, Ch, State2};
        _ ->
            %% 此处stop返回会引发terminate/4的调用，参数2、3分别为Reason、State
            {stop, not_supported_call, not_supported_call}
    end.

%% handle_cast中的应答是分配了的频道Ch，然后gen_server将等待新的请求，
%% 并且现在保持了一个最新可用频道的列表.
%% 无返回值的调用，一般把它叫做通知，
%% 它是一个异步调用，调用会直接收到ok，无需等待返回，所以不需要设置超时参数
%% handle_cast(Msg, State) -> {noreply, NewState}
handle_cast({free, Ch} = Msg, Chs) ->
    io:format("free cast: ~p~n", [Msg]),
    Chs2 = free(Ch, Chs),
    {noreply, Chs2};
handle_cast(Msg, _Chs) ->
    io:format("normal cast: ~p~n", [Msg]),
    {Action, _Param} = Msg,
    case Action of 
        _ ->
            {noreply, not_supported_cast}
    end.

%% 用"!"直接向服务进程发消息
%% 同handle_cast，无返回值 
handle_info(Info, State) ->
    io:format("handle_info: ~p ~p~n", [Info, State]),
    {noreply, State}.

terminate(Reason, State) ->
    io:format("terminate: reason[~p] state[~p]~n", [Reason, State]),
    ok.

%% 用于代码版本替换
%% 是server热部署或代码升级时做callback修改进程状态
%% OldVsn: 旧版本
%% State: gen_server内部状态
%% Extra: 原封不动传递过来的更新指令
code_change(OldVsn, State, Extra) ->
    io:format("code_change: oldver[~p] state[~p] extra[~p]~n", [OldVsn, State, Extra]),
    {ok, State}.

%% =================================== Internal Functions ==========================
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

%% =================================== Test Functions ==========================
test() ->
    NewAllocated = [alloc()],
    io:format("Alloc new channel ~p. Now free it~n", [NewAllocated]),
    NewAllocated2 = [alloc() | NewAllocated],
    io:format("Alloc new channel ~p. Now free it~n", [NewAllocated2]),
    lists:foreach(fun(X) -> 
                          io:format("free channel ~p~n", [X]),
                          free(X)
                  end, NewAllocated2).
