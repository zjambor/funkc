-module(chatiface).
% Client interface
-export([login/1, logout/0, msg/1]).
-define(SRV, chatserver).
-define(Node, 'srv@192.168.0.118').

login(Nick) when is_list(Nick) ->
    {?SRV, ?Node} ! {login, Nick, self()},
    receive
        ok -> ok;
        full -> deny
    after
        5000 -> server_stopped
    end;
login(_) ->
    wrong_nick.

logout() ->
    {?SRV, ?Node} ! {logout, self()}.

msg(Msg) ->
    {?SRV, ?Node} ! {msg, Msg, self()}.
