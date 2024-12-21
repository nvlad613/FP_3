-module(mio).

-export([run_reader/1, run_renderer/0, get_number/1]).

run_reader(ConsumerList) ->
    case read_x_y() of
        {stop_sig} ->
            lists:foreach(
                fun(Consumer) -> Consumer ! {stop, none} end,
                ConsumerList
            ),
            none;
        {ok, Point} ->
            lists:foreach(
                fun(Consumer) -> Consumer ! {process, Point} end,
                ConsumerList
            ),
            run_reader(ConsumerList)
    end.

read_x_y() ->
    case io:get_line("") of
        eof ->
            {stop_sig};
        Line ->
            case try_parse_x_y(Line) of
                {err} ->
                    read_x_y();
                {ok, Point} ->
                    {ok, Point}
            end
    end.

try_parse_x_y(Line) ->
    case string:tokens(Line, " ") of
        [Arg1, Arg2] ->
            {XParsed, X} = get_number(Arg1),
            {YParsed, Y} = get_number(Arg2),
            case {XParsed, YParsed} of
                {ok, ok} -> {ok, {X, Y}};
                _ -> {err}
            end;
        _ ->
            {err}
    end.

get_number(S) when is_number(S) -> {ok, S};
get_number(S) ->
    case string:to_float(S) of
        {error, no_float} ->
            case string:to_integer(S) of
                {error, no_integer} ->
                    {error, 0};
                {I, _} ->
                    {ok, I}
            end;
        {F, _} ->
            {ok, F}
    end.

run_renderer() ->
    receive
        {process, Points} ->
            lists:foreach(
                fun({X, _}) -> io:format("~10.2f ", [X + 0.0]) end,
                Points
            ),
            io:format("~n"),
            lists:foreach(
                fun({_, Y}) -> io:format("~10.2f ", [Y + 0.0]) end,
                Points
            ),
            io:format("~n~n"),
            run_renderer();
        {stop} ->
            none
    end.
