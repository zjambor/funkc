-module(taskfarm).
-export([run/2]).

%% process error handling (process links) and 
%% run-time error handling (try-catch)

run(F, L) ->
    N = 2*erlang:system_info(logical_processors),
    Disp = spawn(fun() -> dispatcher(L, N) end),
    Coll = spawn(fun() -> collector([]) end),
    Worker = fun() -> worker(Disp, Coll, F) end,
    WPids = [spawn(Worker) || _ <- lists:seq(1, N)],
    spawn(fun() -> supervise(WPids, Worker) end),
    Coll.

worker(DPid, CPid, Fun) ->
    DPid ! {ready, self()},
    receive
        {data, Data} ->
            CPid ! {result, Data, Fun(Data)},
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
    sup(Worker).

sup(WorkerFun) ->
    receive
        {'DOWN', Ref, process, Pid, Reason} 
             when Reason /= normal ->
                spawn_monitor(WorkerFun),
                sup(WorkerFun)
    end.