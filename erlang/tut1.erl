-module(tut1).
-export([fac/1, multi/2]).

fac(1) ->
    1;  %% ';' indicates that there is more of the function fac> to come.

fac(X) ->
    fac(X-1) * X.   %% '.' saying that there no more parts of this function

multi(X, Y) ->
    X * Y.
