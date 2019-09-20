-module(first).
-export([fact/1, prod/1]).

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

% pwd().
% c().  -- compile
% first:fact(10).
% first:fact([1,2,3]).
%[1,2,6]
%18> first:fact([1,2,6]).
%[1,2,720]
%19> first:prod([1,2,6]).
