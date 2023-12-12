-module(mirage).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    Values = lists:map(fun parse_values/1, Lines),
    {solve_part1(Values), solve_part2(Values)}.

solve_part1(Values) ->
    lists:sum(lists:map(fun predict_next/1, Values)).

solve_part2(Values) ->
    lists:sum(lists:map(fun predict_prev/1, Values)).

predict_next(Values) ->
    Diffs = build_diffs(Values, [], [lists:reverse(Values)]),
    lists:foldl(fun([X|_], Acc) -> X + Acc end, 0, Diffs).

predict_prev(Values) ->
    Diffs = lists:map(fun lists:reverse/1,
                      build_diffs(Values, [], [lists:reverse(Values)])),
    lists:foldl(fun([X|_], Acc) -> X - Acc end, 0, Diffs).

build_diffs([_], Acc, Out) ->
    case lists:all(fun(X) -> X =:= 0 end, Acc) of
        true -> [Acc|Out];
        _ ->
            RAcc = lists:reverse(Acc),
            build_diffs(RAcc, [], [Acc|Out])
    end;
build_diffs([A,B|T], Acc, Out) ->
    build_diffs([B|T], [B-A|Acc], Out).

parse_values(Line) ->
    Parts = string:split(Line, " ", all),
    lists:map(fun list_to_integer/1, Parts).
