%%%-------------------------------------------------------------------
%% @doc lab3 public API
%% @end
%%%-------------------------------------------------------------------

-module(lab3_app).

-behaviour(application).

-export([start/2, start/0, stop/1, get/2]).

start(_StartType, [Step, Methods]) ->
    S =
        case mio:get_number(get(step, Step)) of
            {err, _} -> Step;
            {ok, Numb} -> Numb
        end,
    M = get(methods, Methods),
    {ok, spawn(main, run, [S, string:split(M, ",")])}.

start() ->
    S =
        case mio:get_number(get(step, 1)) of
            {error, _} ->
                io:format("WARNING: Used default value of argument ~p = ~p~n", [step, 1]),
                1;
            {ok, Numb} ->
                Numb
        end,
    M = lab3_app:get(methods, "linear"),
    main:run(S, string:split(M, ",")).

stop(_State) ->
    ok.

get(Name, Def) ->
    case init:get_argument(Name) of
        {ok, [[L]]} ->
            L;
        _ ->
            io:format("WARNING: Used default value of argument ~p = ~p~n", [Name, Def]),
            Def
    end.
