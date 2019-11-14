-module(chatclient).
-export([start/1]).

start(Nick) ->
    case chatiface:login(Nick) of
        wrong_nick ->
            nok;
        server_stopped -> nok;
        deny -> nok;
        ok -> 
            ClPid = self(),
            spawn(fun() -> io(ClPid) end),
            client()
    end.

client() ->
    receive 
        stop ->
            chatiface:logout();
        {msg, Text} ->
            chatiface:msg(Text),
            client();
        {text, Text} -> 
            io:format("~p~n", [Text]),
            client()
    end.

io(Pid) ->
    case string:strip(io:get_line("--> "), right, $\n) of
        "#q" -> Pid ! stop;
        Text -> 
            Pid ! {msg, Text},
            io(Pid)
    end.