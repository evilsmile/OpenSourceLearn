-module(looper).
-compile(export_all).

%% 测试原始loop:
%% > P = spawn(looper, loop, []).
%% > P!abc.
%% > P!abcd.
%% > P!abcde.
%% > erlang:process_info(P).
loop() ->
    receive
        abc ->
            io:format("Receive abc.~n"),
            loop();
        stop ->
            io:format("stop."),
            stop
    end.


%% 加入超时清理loop2:
%% > P = spawn(looper, loop2, []).
%% > P!abc.
%% > P!abcd.
%% > P!abcde.
%% > erlang:process_info(P).
loop2() ->
    receive 
        abc ->
            io:format("Receive abc.~n"),
            loop2();
        stop ->
            io:format("stop."),
            stop
        after 15000 ->
            receive
                Any -> 
                    io:format("Receive ~p ~n" ,[Any])
            end,
                io:format("clear. ~n"),
                loop2()
    end.

%% 加入睡眠清理loop3:
%% > P = spawn(looper, loop3, []).
%% > P!abc.
%% > P!abcd.
%% > P!abcde.
%% > erlang:process_info(P).
loop3() ->
    receive
        abc ->
            io:format("Receive abc.~n"),
            timer:sleep(10000),
            io:format("sleep after receive abc done.~n"),
            loop3();
        stop ->
            io:format("stop"),
            stop
    after 25000 ->
              receive
                  Any ->
                      io:format("Receive ~p ~n" ,[Any])
              end,
              io:format("clear. ~n"),
              loop3()
    end.



process_infos() ->
    filelib:ensure_dir("./log/"),
    File = "./log/process_infos.log",
    {ok, Fd} = file:open(File, [write, raw, binary, append]),
    Fun = fun(Pi) ->
                  Info = io_lib:format("=>~p \n\n", [Pi]),
                  case filelib:is_file(File) of
                      true -> file:write(Fd, Info);
                      false ->
                          file:close(Fd),
                          {ok, NewFd} = file:open(File, [write, raw, binary, append]),
                          file:write(NewFd, Info)
                  end,
                  timer:sleep(20)
          end,
    [ Fun(erlang:process_info(P)) || P <- erlang:processes()].
