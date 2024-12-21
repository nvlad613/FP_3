-module(processor).

-export([run/3]).

run(Func, BatchSz, Consumer) ->
    loop(queue:new(), {Func, BatchSz, Consumer}).

loop(Queue, {Func, BatchSz, Consumer}) ->
    receive
        {stop, _} ->
            none;
        {process, Data} ->
            QueueNew = update_queue(Queue, Data, BatchSz, fun(Q) ->
                Consumer ! {process, process_data(Func, Q, BatchSz)}
            end),
            loop(QueueNew, {Func, BatchSz, Consumer})
    end.

update_queue(Queue, Item, MaxSz, OnMaxSzCallback) ->
    Q = queue:in(Item, Queue),
    case queue:len(Q) of
        MaxSz ->
            OnMaxSzCallback(Q),
            {_, QNew} = queue:out(Q),
            QNew;
        _ ->
            Q
    end.

process_data(Func, Queue, BatchSz) ->
    case queue:len(Queue) of
        BatchSz ->
            Func(queue:to_list(Queue));
        _ ->
            []
    end.
