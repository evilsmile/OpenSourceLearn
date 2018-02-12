-module(try_record).

-export([test/0, test/1]).

-record(info, {name, age}).

test(#info{name = Name} = I) ->
    io:format("Module:[~p] Info:[~p] Name;[~p] ~n", [?MODULE, I, Name]).

test() ->
    Info = #info{ name = <<"YY">>, age = 18},
    test(Info).
