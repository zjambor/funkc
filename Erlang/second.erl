-module(second).
-export([fact/1, prod/1, prod1/1, inc/1, inc1/1]).
-compile(export_all).

%fact(Par1, ..., Parn) when Guard -> Body;
%...
%...Bodyn

fact(0) -> 1;
fact(Num) when is_integer(Num), Num > 0 -> fact (Num - 1) * Num;
%fact(_) -> "Error: wrong type";
fact([]) -> [];
fact([Head|Tail]) -> [fact(Head) | fact(Tail)].

prod([]) -> 
    1;
prod([ Head | Tail ]) -> prod(Tail) * Head.

prod1(List) ->
    prod1(List, 1).

prod1([], Acc) ->
    Acc;
prod1([Head|Tail], Acc) ->
    prod1(Tail, Acc*Head).

%%am1 -> prod1([1,2,3,45,5,6]).

% A két végrehajtásnak más a sorrendje!

inc([]) -> [];
inc([Head|Tail]) ->
    [Head+1 | inc(Tail)].

inc1(List) ->
    inc1(List, []).

inc1([], Acc) -> Acc;
inc1([Head|Tail], Acc) ->
    inc1(Tail, Acc ++ [Head+1]).

% second:inc([1,2,3,45,5,6]).  
% [2,3,4,46,6,7]
% 7> second:inc1([1,2,3,45,5,6]).
% [7,6,46,4,3,2]

inc2(List) ->
    inc2(List, []).

inc2([], Acc) -> lists:reverse(Acc);
inc2([Head|Tail], Acc) ->
    inc2(Tail, [Head+1 | Acc]).

inc3(List) ->
    [Head+1 || Head <- List].
% listagenerátor, elemenként dolgozza fel

inc4(List) ->
    [Head+1 || Head <- List, Head < 12].

count([]) -> [];
count([Head | Tail]) ->
    HList = [E || E <- Tail, E == Head],
    RemList = [E || E <- Tail, E /= Head],
    {HList, RemList} = split(Tail, Head),
    [{Head, length(HList) + 1} | count(RemList)].

% second:count([1,2,3,45,5,6,5,1,45]).
% [{1,2},{2,1},{3,1},{45,2},{5,2},{6,1}]

%split([H | T ], E) when H == E ->
split([H | T], H) ->
    {Eq, Neq} = split(T, H),
    {[H | Eq], Neq};
split([H | T ], E) ->
    {Eq, Neq} = split(T, E),
    {Eq, [H | Neq]};
split([], E) ->
    {[], []}.

% split([H | T], H) -> mintaillesztés, az utána jövő 

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


count2(List) -> count2(List, []).
count2([], _) -> [];
count2([H|T], Acc) ->                   %when lists:member(H, Acc) ->
    case lists:member(H, Acc) of
        true -> count2(T, Acc);
        _ ->                            %false ->
            HList = [E || E <- T, E == H],
            [{H, length(HList) + 1} | count2(T, [H | Acc])]
    end.
%    count(T, Acc);
%count2([H|T], Acc) ->
%    HList = [E || E <- Tail, E == H],
%    [{H, length(HList) + 1} | count(T, [H | Acc])].
