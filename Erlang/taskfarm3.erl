-module(taskfarm3).
-export([run/2]).

%% process error handling (process links) and 
%% run-time error handling (try-catch)

run(F, L) ->
    N = 2*erlang:system_info(logical_processors),
    Disp = spawn(fun() -> dispatcher(L, N) end),
    Coll = spawn(fun() -> collector([]) end),
    Worker = fun() -> worker(Disp, Coll, F) end,
    WPids = [spawn(Worker) || _ <- lists:seq(1, N)],
    %spawn(fun() -> supervise(WPids, Worker) end),
    Coll.

worker(DPid, CPid, Fun) ->
    DPid ! {ready, self()},
    receive
        {data, Data} ->
            %CPid ! {result, Data, catch Fun(Data)},
            %case catch Fun(Data) of
            %    {'EXIT', _} -> skip;
            %    Value -> CPid ! {result, Data, Value}
            %end, 
            try Fun(Data) of
                Value -> CPid ! {result, Data, Value}
            catch
                Class:Type -> io:format("Error in execution: ~p", [{Class, Type}])
            end,
            worker(DPid, CPid, Fun);
        stop ->
            io:format("Worker terminated: ~p~n", [self()])
    end.

dispatcher([H|T], N) -> 
    receive 
        {ready, WPid} ->
            WPid ! {data, H},
            dispatcher(T, N)
    end;
dispatcher([], 0) ->
    io:format("Dispatcher terminated~n");
dispatcher([], N) ->
    receive
        {ready, WPid} ->
            WPid ! stop,
            dispatcher([], N-1)
    end.

 %   receive
 %   after
 %       5000 -> dispatcher([])
 %   end.

collector(Acc) ->
    receive
        {result, Input, Value} ->
            collector([ {Input, Value} | Acc]);
        {give_me, Pid} ->
            Pid ! {subresults, Acc},
            collector(Acc);
        {stop, Pid} ->
            Pid ! {finalresults, Acc},
            io:format("Collector terminated")
    end.

supervise(WPids, Worker) ->
    Refs = [monitor(process, W) || W <- WPids],
    sup(Worker, Refs).

sup(WorkerFun, Refs) ->
    receive
        {'DOWN', Ref, process, Pid, Reason}  when Reason /= normal ->
            case lists:member(Ref, Refs) of
                true -> 
                    {NewPid, NewRef} = spawn_monitor(WorkerFun),
                    io:format("Process terminated and restarted:~p~n ", [{Pid, NewPid}]),
                    sup(WorkerFun, [NewRef | lists:delete(Ref, Refs)] );
                false ->
                    io:format("Unexpected termination from process:~p~n ", [Pid]),
                    sup(WorkerFun, Refs)
            end
    end.