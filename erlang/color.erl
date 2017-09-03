-module(color).
-export([new/4, blend/2]).

% a macro 'is_channel' is defined to help with guard test. 
% This is only here for convenience and to reduce syntax culttering.
-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

% create a new map term and lets the keys 'red','green', 'blue', and 'alpha' be associated with an initial value. 
% Only the '=>' operator is allowed when crewating a new map.
new(R,G,B,A) when ?is_channel(R), ?is_channel(G), 
                     ?is_channel(B), ?is_channel(A) ->
    #{red => R, green => G, blue => B, alpha => A}.

blend(Src, Dst) ->
    blend(Src, Dst, alpha(Src, Dst)).

blend(Src, Dst, Alpha) when Alpha > 0.0 ->
    Dst#{
      red := red(Src, Dst) / Alpha,
      green := green(Src, Dst) / Alpha,
      blue := blue(Src, Dst) / Alpha,
      alpha := Alpha
     };

blend(_, Dst, _) ->
    Dst#{
      red := 0.0,
      blue := 0.0,
      green := 0.0,
      alpha := 0.0
     }.

% The value associated with key 'alpha' is fetched for both arguments using the ':=' operator. 
% The other keys in the map are ignored, olny the key 'alpha' is required and checked for.
alpha(#{alpha := SA}, #{alpha := DA}) ->
    SA + DA*(1.0 - SA).

red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).

green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).

blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).

