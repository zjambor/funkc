-module(ring).
-export([run/1, run2/1]).

run2(N) ->
    First = lists:foldl(fun(_, Acc) -> 
                          spawn(fun() -> 
                                   receive
                                     ok -> Acc ! ok
                                   end
                                end) 
                        end, self(), lists:seq(1, N)),
    First ! ok,
    receive
        ok ->
            finished
    end.

run(N) ->
    W0 = spawn(fun() -> worker(N) end),
    W0 ! {ok, self()},
    receive
        ok -> finished
    end.

worker(0) ->
    receive
        {A, Main} -> Main ! A
    end;
worker(N) ->
    Wi = spawn(fun() -> worker(N-1) end),
    receive
        A -> Wi ! A
    end.
