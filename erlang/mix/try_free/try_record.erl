-module(try_record).

-export([test/0, test/1]).

-record(info, {name, age}).

test(#info{name = Name}) ->
    Name.

test() ->
    Info = #info{ name = <<"YY">>, age = 18},
    test(Info).
