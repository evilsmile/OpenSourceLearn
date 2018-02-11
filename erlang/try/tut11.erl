-module(tut11).
-export([add_3_to_list/1, sort_list/1]).

add_3(X) -> 
    X + 3.

%% Use defined function to list:map
add_3_to_list(List) ->
    lists:map(fun add_3/1, List).


sort_list(List) ->
    New_list = lists:map(fun add_3/1, List),
    %% fun(X, Y) 这个参数决定于New_list中的元素类型。这里是数字而已
    lists:sort(fun(X, Y) -> X < Y end, New_list).

