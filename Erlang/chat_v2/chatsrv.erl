-module(chatsrv).
% Server interface
-export([start/1, stop/0, dump/0]).
%Server private
-export([init/1]).

-include("chat.hrl").

start(Max) ->
    register(?SRV, spawn(?MODULE, init, [Max])).

init(Max) ->
    process_flag(trap_exit, true),
    State = #state{max=Max}, %[{users, []}, {max, Max}], {[],Max}
    io:format("Server started~n"),
    loop(State).

loop(St=#state{clients=Cl}) -> % {Cl, Max, Notes}
    receive
        {login, Nick, Pid} when length(Cl) < St#state.max -> 
            link(Pid),
            Pid ! ok,
            loop(St#state{clients=[ {Nick, Pid} | Cl]}); %{[ {Nick, Pid} | Cl], Max, Notes}
        {login, _Nick, Pid} ->
            Pid ! full,
            loop(St);
        {logout, Pid} -> 
            NewClients = lists:keydelete(Pid, 2, Cl),
            loop(St#state{clients=NewClients});
        {'EXIT', Pid, Reason} ->
            NewClients = lists:keydelete(Pid, 2, Cl),
            loop(St#state{clients=NewClients});
        {msg, Msg, Pid} -> 
            {Nick, Pid} = lists:keyfind(Pid, 2, Cl),
            NewMsg = Nick ++ ": " ++ Msg,
            lists:map(fun({_, Pid}) -> 
                        Pid ! {text, NewMsg}
                      end, Cl),
            loop(St);
        stop -> 
            terminate(St);
        dump ->
            io:format("State: ~p~n", [St]),
            loop(St)
    end.

stop() -> 
    ?SRV ! stop.

terminate(State) ->
    io:format("Server terminated. State: ~p~n", [State]).

dump() -> 
    ?SRV ! dump.