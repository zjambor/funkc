-module(third).
-export([count/2]).
-compile(export_all).

split_count([H | T], H) ->
    {C, Neq} = split_count(T, H),
    {C + 1, Neq};
split_count([H | T ], E) ->
    {C, Neq} = split_count(T, E),
    {C, [H | Neq]};
split_count([], E) ->
    {[], []}.

count1([]) -> [];
count1([H|T]) ->
    {C, Rem} = split_count(T, H),
    [{H, C + 1} | count1(Rem)].
%
% key-value tárolók, map-ek
% map:
% #{alma => 2, korte => 3}.
% asszociatív tároló, lusta kiértékelésű, balról jobbra asszociáció
% M = #{alma => 2, korte => 3}.
% #{korte := Value} = M.
% L = [12111 | [1311]]
% [H | T] = L.
% H.
% T.
% Value.   = 3
% update:
% M#{alma => 42}.   ilyenkor másolás történik, az eredeti map nem változik.

count2(List) -> count2(List, []).
count2([], _) -> [];
count2([H|T], Acc) ->                   %when lists:member(H, Acc) ->
    case lists:member(H, Acc) of
        true -> count2(T, Acc);
        _ ->                            %false ->
            HList = [E || E <- T, E == H],
            [{H, length(HList) + 1} | count2(T, [H | Acc])]
    end.
%   

count([], Map) ->
    Map;
count([H | T], Map) ->
    case Map of 
        #{H := C} -> count(T, Map#{H=>C+1});
        _ -> count(T, Map#{H=>1})
    end.

% maps:get(korte, M).
% third:count([3,4,5,41,8,2], #{}).
