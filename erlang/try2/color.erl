-module(color).

-export([new/4, blend/2]).

% 定义宏
-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

% new/4创建一个新的映射
new(R, B, G, A) when ?is_channel(R), ?is_channel(B), 
                     ?is_channel(G), ?is_channel(A) ->
    #{red => R, blue => B, green => G, alpha => A}.

blend(Src, Dst) ->
    blend(Src, Dst, alpha(Src, Dst)).

blend(Src, Dst, Alpha) when Alpha > 0.0 ->
    Dst#{
      red := red(Src, Dst)/Alpha,
      green := green(Src, Dst)/Alpha,
      blue := blue(Src, Dst)/Alpha,
      alpha := Alpha
     };

blend(_, Dst, _) -> 
    Dst#{
      red := 0.0,
      green := 0.0,
      blue := 0.0,
      alpha := 0.0
     }.

% 使用 := 取得键alpha相关的值作为参数的值，映射中的其它值被忽略
% 因为只需要alpha的值，所以也只会检查映射中的该键值对
alpha(#{alpha := SA}, #{alpha := DA}) ->
    SA + DA * (1.0-SA).

red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV * SA + DV * DA * (1.0 - SA).

green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
    SV * SA + DV * DA * (1.0 - SA).

blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
    SV * SA + DV * DA * (1.0 - SA).
