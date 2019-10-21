-module(parmap).
-compile(export_all).

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

map(List, Fun) ->
    [ Fun(Head) || Head <- List].
%% spawn, !, receive ++  self()

parmap(List, Fun) ->
    [spawn(fun() -> Fun(Head) end) || Head <- List].

parmap1(List, Fun) ->
    Shell = self(),
    [spawn(fun() -> Shell ! Fun(Head) end) || Head <- List],
    [receive 
        A -> A
     end || _ <- List].


inc0(List) ->
    [ Head +1 || Head <- List].

add(X) -> X +1.

inc(List) ->
    [ add(Head) || Head <- List].

call(L) ->
    {map(L, fun add/1), %% implicit fun-exp
     map(L, fun(X) -> X +1 end), %% explicit fun-exp
     map(L, fun(X) -> add(X) end)
    }.