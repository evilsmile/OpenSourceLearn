-module(index).
-export([]).

-define(Punctuation, "[|,|\\.|;|:|\\t|\\n|\\(|\\)]+").

index(File) ->
    ets:new(indexTable, [ordered_set, named_table]),
    processFile(File),
    prettyIndex().

%%
processFile(File) ->
    {ok, IoDevice} = file:open(File, [read]),
    processLines(IoDevice, 1).

processFile(Line, N) ->
    Words = re:split(Line, ?Punctuation, [{return, list}]),
    processWords(Words, N);

%%
processLines(IoDevice, N) ->
    case io:get_line(IoDevice, "") of
        eof ->
            ok;
        Line ->
            processLine(Line, N),
            processLines(IoDevice, N+1)
    end.

%%
processWords(Words, N) ->
    case Words of 
        [] -> ok;
        [Word|Rest] ->
            if 
                length(Word) > 3 ->
                    Normalise = string:to_lower(Word),
                    ets:insert(indexTable, {{Normalise, N}});
                true -> ok
            end,
            processWords(Rest, N)
    end.

prettyIndex() ->
    case ets:first(indexTable) of
        '$end_of_table' ->
            ok;
        First ->
            case First of
                {Word, N} ->
                    IndexEntry = {Word, [N]}
            end,
            prettyIndexNext(First, IndexEntry)
    end.

prettyIndexNext(Entry, {Word, Lines} = IndexEntry) ->
    Next = ets:next(indexTable, Entry),
    case Next of 
        '$end_of_table' ->
            prettyEntry(IndexEntry);
        {NextWord, M} ->
            if
                NextWord == Word ->
                    prettyIndexNext(Next, {Word, [M|Lines]});
                true ->
                    prettyEntry(IndexEntry),
                    prettyIndexNext(Next, {NextWord, [M]})
            end
    end.
