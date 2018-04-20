-module(tcp_svr).
-export([server/0]).

server() ->
    case gen_tcp:listen(1234, [binary, {active, false}]) of
        {ok, ListenSocket} ->
            wait_connect(ListenSocket, 0);
        {error, Reason} ->
            io:format("start server failed: ~p~n", [Reason])
    end.

wait_connect(ListenSocket, Count) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, wait_connect, [ListenSocket, Count+1]),
    get_request(Socket, [], Count).

get_request(Socket, BinaryList, Count) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Binary} ->
            get_request(Socket, [Binary|BinaryList], Count);
        {error, closed} ->
            gen_tcp:close(Socket),
            handle(lists:reverse(BinaryList), Count)
    end.

handle(Binary, Count) ->
    {ok, Fd} = file:open("log_file_"++integer_to_list(Count), write),
    file:write(Fd, Binary),
    file:close(Fd).
