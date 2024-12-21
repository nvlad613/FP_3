-module(main).

-export([run/2]).

methods_spec_list() ->
    [
        {"linear", fun interp:linear/2, 2},
        {"lagrange", fun interp:lagrange/2, 4}
    ].

run(Step, Methods) ->
    RendererPid = spawn(fun() -> mio:run_renderer() end),
    ProcPidList = lists:filtermap(
        fun(Method) -> spawn_interpolator(Method, Step, RendererPid) end,
        Methods
    ),
    case ProcPidList of
        [] ->
            none;
        _ ->
            mio:run_reader(ProcPidList)
    end,
    RendererPid ! {stop},
    io:format("FINISHED!\n"),
    ok.

spawn_interpolator(MethodName, Step, RendererPid) ->
    Found = lists:filter(
        fun({Name, _, _}) -> Name =:= MethodName end,
        methods_spec_list()
    ),
    case Found of
        [{_, InterpFunc, WinSz} | _] ->
            {
                true,
                spawn(fun() ->
                    processor:run(fun(Points) -> InterpFunc(Step, Points) end, WinSz, RendererPid)
                end)
            };
        _ ->
            false
    end.
