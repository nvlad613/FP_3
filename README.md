
# Лабораторная работа 3
Ненов Владислав Александрович, P34082
=====

# Цель
Получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.
В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции.

# Требования
- обязательно должна быть реализована линейная интерполяция (отрезками, [link](https://en.wikipedia.org/wiki/Linear_interpolation));
- настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:
    - какие алгоритмы использовать (в том числе два сразу);
    - частота дискретизации результирующих данных;
    - и т.п.;
- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру `x;y\n` или `x\ty\n`) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- `cat | grep 11`), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

# Реализация

## Описание
Программа по структуре предстовляет собой конвейер, состоящий из 3х основных типов звеньев:
```
                (processor1)
(reader)    ->  (processor2)    ->  ... ->  (renderer)
                (processor3)
``` 

- В качестве reader'а выступает модуль `main`, который сначала инициализирует все остальные процессы, после чего запускает функцию, которая в бесконечном цикле читает *stdin*, а полученные данные отправляет зарегистрированным процессорам.
- В качестве процессоров выступают функции интерполяции, обернутые в унифицированный интерфейс из модуля `processor`. Процессоры получают данные, отправленные ридером, обрабатывают их и отправляют дальше по конвейру. Получается, что в программе у нас всего 2 зарегистрированных процессора: интерполяция лагранжа и линейная.
- Следующим звеном у нас сразу выступает `renderer`, поскольку в лабораторной работе нет надобности обработки данных в несколько этапов. Но потенциально такая возможность имеется.
- `renderer` получает данные на вход, форматирует и выводит в *stdout*.

## Листинг кода


### Основные методы чтения и вывода данных

```erlang
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
```

### Основные методы обработки данных

```erlang
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
```


### Интерполяция 

```erlang
linear(Step, Points) when length(Points) =:= 2 ->
    [{AX, AY}, {BX, BY}] = Points,
    Gen = lists:seq(0, round((BX - AX) / Step), 1),
    {Result, _} = lists:mapfoldl(
        fun(_, X) -> {interp_linear(X, {AX, AY}, {BX, BY}), X + Step} end,
        AX,
        Gen
    ),
    Result;
linear(_, _) ->
    erlang:error({invalid_argument, "linear interpolation must receive 2 points!"}).

lagrange(Step, Points) when length(Points) =:= 4 ->
    [{BX, _} | _] = lists:reverse(Points),
    [{AX, _} | _] = Points,
    Gen = lists:seq(0, round((BX - AX) / Step), 1),
    {Result, _} = lists:mapfoldl(
        fun(_, X) -> {{X, lagrange_interp(X, Points)}, X + Step} end,
        AX,
        Gen
    ),
    Result;
lagrange(_, _) ->
    erlang:error({invalid_argument, "lagrange interpolation must receive 4 points!"}).
```
