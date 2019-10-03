-module(parmap).

-compile(export_all).

add(X) -> X + 1.

inc(List) ->
    [add(Head) || Head <- List].

map(List, Fun) ->
    [Fun(Head) || Head <- List].

call(L) ->
    {map(L, fun add/1),  % implicit fun. exp.
    map(L, fun(X) -> X + 1 end),    % explicit fun. exp.
    map(L, fun(X) -> add(X) end)   % ugyanaz
    }.

% parmap:call([2311,2411,2511]).
% parmap:call([2311,2411,2511], fun parmap:add/1).
% "alma~n" vagy "alma\n" ~p pretty print: listáknál
