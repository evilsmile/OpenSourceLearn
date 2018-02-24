-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

start() ->
    register(freq, spawn(frequency, init, [])).

stop() ->
    call(stop).

allocate() ->
    call(allocate).

deallocate(Freq) ->
    call({deallocate, Freq}).

call(Message) ->
    Ref = make_ref(),
    freq ! {request, {Ref, self()}, Message},
    receive 
        {reply, Ref, Reply} -> 
            Reply
    end.

%%
init() ->
    process_flag(trap_exit, true),
    Frequencies = {get_frequency(), []},
    loop(Frequencies).

get_frequency() ->
    [10, 11, 12, 13, 14].

loop(Frequencies) ->
    receive 
        {request, {_, Pid} = Info, allocate} ->
            {NewFeq, Reply} = allocate(Frequencies, Pid),
            reply(Info, Reply),
            loop(NewFeq);
        {request, Info, {deallocate, Freq}} ->
            NewFeq = deallocate(Frequencies, Freq),
            reply(Info, ok),
            loop(NewFeq);
        {'EXIT', {_, Pid}, _Reason} ->
            NewFreq = exited(Frequencies, Pid),
            loop(NewFreq);
        {request, Info, stop} ->
            reply(Info, ok)
    end.

reply({Ref, Pid}, Reply) ->
    Pid ! {reply, Ref, Reply}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.

exited({Free, Allocated}, Pid) ->
    case lists:search(Pid, 2, Allocated) of 
        {value, {Freq, Pid}} ->
            NewFreq = lists:keydelete(Pid, 1, Allocated),
            {[Freq|Free], NewFreq};
        false ->
            {Free, Allocated}
    end.
