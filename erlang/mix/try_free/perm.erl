-module(perm).
-export([perms/1, splits/1]).

% 19> perm:perms([2,3]).
% [[2,3],[3,2]]
% 16> perm:perms([1,2,3]). 
% [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
perms([]) ->
    [[]];
perms([X|Xs]) ->
    [insert(X, As, Bs) || Ps <- perms(Xs),
                          {As, Bs} <- splits(Ps)].

% 20> perm:splits([2,3]).
% [{[],[2,3]},{[2],[3]},{[2,3],[]}]
% 21> perm:splits([3,2]).
% [{[],[3,2]},{[3],[2]},{[3,2],[]}]
% 15> perm:splits([1,2,3]).
% [{[],[1,2,3]},{[1],[2,3]},{[1,2],[3]},{[1,2,3],[]}]
splits([]) ->
    [{[], []}];
splits([X|Xs] = Ys) ->
    [{[], Ys} | [ {[X|As], Bs} || {As, Bs} <- splits(Xs)] ].

insert(X, As, Bs) ->
    lists:append([As, [X], Bs]).
