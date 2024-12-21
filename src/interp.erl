-module(interp).

-export([linear/2, lagrange/2]).

interp_linear(X, {AX, AY}, {BX, BY}) ->
    {X, (AY * (BX - X) + BY * (X - AX)) / (BX - AX)}.

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

%
%
% Вычисление базисного полинома L_i(X) для точки Xi
lagrange_basis(X1, Xi, Points) ->
    lists:foldl(
        fun
            ({Xj, _}, Acc) when Xj =/= Xi ->
                Acc * ((X1 - Xj) / (Xi - Xj));
            (_, Acc) ->
                Acc
        end,
        1,
        Points
    ).

% Главная функция: принимает X1 и список из 4 точек
lagrange_interp(X, Points) when length(Points) =:= 4 ->
    % Вычисляем значение Y с помощью суммы Лагранжа
    lists:foldl(
        fun({Xi, Yi}, Acc) ->
            Acc + Yi * lagrange_basis(X, Xi, Points)
        end,
        0,
        Points
    ).

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
