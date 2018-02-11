-module(macro_test).
-export([dump/1, go/1, show/2, len/1, say/0]).

-define(Func, X).
-define(Double, *2).
-define(F, dump(X)).
-define(Eq(X, Y), X=:=Y).
%-define(P(Content), ok).
-define(PF(Content), Args), io:format(Content, Args)).
-define(P(Content), io:format(Content)).

-define(Len(Call), io:format("~p=~p ~n", [??Call, Call])).

-ifdef(me).
   -define(Who, "evil").
-else.
   -define(Who, "Zen").
-endif.

say()->
    ?P(?Who).

len(List) when is_list(List)->
    ?Len(length(List)).

dump(X)->
    ?Func?Double.

go(X)->
    ?F.

show(X,Y) when ?Eq(X, Y)->
    ?P("The are equal. ~n");

show(_X, _Y)->
    io:format("HAHA").
