-module(revcomp_opt).
-export([main/1]).

-define(WIDTH, 60).
-define(BUFSIZE, 1024*1024).
-define(NEWLINECHAR, 10).

-record(scan_state,
        {current_state = search_header_begin, % search_header_end, search_is_over
         current_header = <<>>,
         current_body = [<<>>]}).

main([_Args]) ->
    io:setopts([binary]),
    InitState = #scan_state{},
    % fprof:trace(start, "revcomp_opt_small.trace"),
    read_and_process_file(<<>>, InitState, []),
    % fprof:trace(stop),
    halt().

read_and_process_file(Buf, State, JobList) ->
    case State#scan_state.current_state of
        search_header_begin ->
            % look for ">"
            case binary:match(Buf, <<">">>) of
                nomatch ->
                    % keep looking, add new content to body
                    NState = State#scan_state{
                               current_body = [Buf | State#scan_state.current_body]
                              },
                    get_new_chunk(NState, JobList);
                {HeaderStartPos, _Length} ->
                    % find ">", meaning a new line, and end old body.
                    % create new process to handle header/body
                    {PreviousBody, BufLeft} = split_binary(Buf, HeaderStartPos),
                    NState = State#scan_state{
                               current_state = search_header_end,
                               current_header = <<>>,
                               current_body = []
                              },
                    case State#scan_state.current_header of
                        <<>> ->
                            % first handle, go on looking for end '\n' of header
                            read_and_process_file(BufLeft, NState, JobList);
                        _ ->
                            % a new complete body, create a new process to handle.
                            NewJob = start_revcomp_job(
                                       State#scan_state.current_header,
                                       [PreviousBody | State#scan_state.current_body],
                                       self()),
                            read_and_process_file(BufLeft, NState, [NewJob | JobList])
                    end
            end;
        search_header_end ->
            % looking for end '\n' of ">" line
            case binary:match(Buf, <<"\n">>) of
                nomatch ->
                    % keep searching, append Buf to current_header if not found
                    NState = State#scan_state {
                               current_header = 
                               <<(State#scan_state.current_header)/binary, Buf/binary>>
                              },
                    get_new_chunk(NState, JobList);
                {HeaderEndPos, _Length} ->
                    % find end '\n' of header line, then construct the body
                    {PreviousHeader, BufLeft} = split_binary(Buf, HeaderEndPos),
                    NState = State#scan_state{
                               current_state = search_header_begin,
                               current_header = 
                               <<(State#scan_state.current_header)/binary, PreviousHeader/binary>>
                               },
                    read_and_process_file(BufLeft, NState, JobList)
            end;
        search_is_over ->
            % scan done.
            case State#scan_state.current_header of
                <<>> ->
                    AllJobs = JobList;
                _ ->
                    NewJob = start_revcomp_job(State#scan_state.current_header,
                                               State#scan_state.current_body,
                                               self()),
                    AllJobs = [NewJob | JobList]
            end,
            % collect handle result of worker processes
            collect_revcomp_jobs(lists:reverse(AllJobs))
    end.

get_new_chunk(State, JobList) ->
    case file:read(standard_io, ?BUFSIZE) of
        eof ->
            NState = State#scan_state{current_state = search_is_over},
            read_and_process_file(<<>>, NState, JobList);
        {ok, Chunk} ->
            read_and_process_file(Chunk, State, JobList)
    end.

collect_revcomp_jobs([]) ->
    ok;
collect_revcomp_jobs([Job | Rest]) ->
    receive 
        {Job, HeaderBuf, RevCompBodyPrint} ->
            erlang:display(Job),
            file:write(standard_io, [HeaderBuf, ?NEWLINECHAR, RevCompBodyPrint])
    end,
    collect_revcomp_jobs(Rest).

start_revcomp_job(HeaderBuf, BodyBufList, Master) ->
    spawn(fun() -> revcomp_job(HeaderBuf, BodyBufList, Master) end).

revcomp_job(HeaderBuf, BodyBufList, Master) ->
    RevCompBody = << <<(revcomp_a_chunk(ABuf))/binary>> || ABuf <- BodyBufList>>,
    RevCompBodyPrint = revcomp_chunk_printable(<<>>, RevCompBody),
    Master ! {self(), HeaderBuf, RevCompBodyPrint}.

revcomp_a_chunk(Chunk) ->
    Complement = << <<(complement(Byte))>> || <<Byte>> <= Chunk, Byte =/= ?NEWLINECHAR >>,
    % reverse
    % 计算完互补碱基之后，接下来是一个快速的 binary 翻转操作。
    % 这个翻转非常巧妙高效，首先将要翻转的 binary 以一个巨大的小尾顺序大整数读入，
    % 然后再将其以大尾顺序的方式写入一个新的 binary，
    % 那么得到的这个 binary 就是原 binary 翻转后的结果。
    % 整个翻转操作是在 ERTS 内部通过C语言实现的，效率非常高。
    ComplementBitSize = bit_size(Complement),
    <<X:ComplementBitSize/integer-little>> = Complement,
    ReversedComplement = <<X:ComplementBitSize/integer-big>>,
    ReversedComplement.

revcomp_chunk_printable(Acc, Rest) when byte_size(Rest) >= ?WIDTH ->
    <<Line:?WIDTH/binary, Rest0/binary>> = Rest,
    revcomp_chunk_printable(<<Acc/binary, Line/binary, ?NEWLINECHAR>>, Rest0);
revcomp_chunk_printable(Acc, Rest) ->
    <<Acc/binary, Rest/binary, ?NEWLINECHAR>>.

complement($A) -> $T;
complement($C) -> $G;
complement($G) -> $C;
complement($T) -> $A;
complement($U) -> $A;
complement($M) -> $K;
complement($R) -> $Y;
complement($Y) -> $R;
complement($K) -> $M;
complement($V) -> $B;
complement($H) -> $D;
complement($D) -> $H;
complement($B) -> $V;
complement($a) -> $T;
complement($c) -> $G;
complement($g) -> $C;
complement($t) -> $A;
complement($u) -> $A;
complement($m) -> $K;
complement($r) -> $Y;
complement($y) -> $R;
complement($k) -> $M;
complement($v) -> $B;
complement($h) -> $D;
complement($d) -> $H;
complement($b) -> $H;
complement($N) -> $H;
complement($S) -> $S;
complement($W) -> $W;
complement($n) -> $N;
complement($s) -> $S;
complement($w) -> $W.
