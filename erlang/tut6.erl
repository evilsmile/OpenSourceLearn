-module(tut6).
-export([list_max/1, list_min/1]).

%% < less than
%% > greater than
%% == equal
%% >= greater or equal
%% =< less or equal
%% /= not equal

%% 感觉使用erl写代码省去很多事

%% When functions have the same name but taking a different number of arguments, they are regarded as completely different functions.
%% Here list_max/1 simply assumes that the max value of the list
%% is the head of the list and calls list_max/2 with the rest of the list.
list_max([Head|Rest]) ->
    list_max(Rest, Head).

list_max([], Res) ->
    Res;

list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    list_max(Rest, Head);

list_max([Head|Rest], Result_so_far) ->
    list_max(Rest, Result_so_far).


list_min([Head | Rest]) ->
    list_min(Rest, Head).

list_min([], Res) ->
    Res;

list_min([Head | Rest], Cur_min) when Head < Cur_min ->
    list_min(Rest, Head);
list_min([Head | Rest], Cur_min) ->
    list_min(Rest, Cur_min).
