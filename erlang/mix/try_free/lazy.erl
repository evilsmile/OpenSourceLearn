-module(lazy).
-export([next/1]).

%% 包含从零开始的整数的无穷序列。
%% 头部是第一个序列书，而尾部是一个构造，它将递归生成下一个序列数和新的尾部
next(Seq) ->
    fun() -> [Seq | next(Seq + 1)] end.
