-module(trebuchet).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    {solve_part1(Lines), solve_part2(Lines)}.

solve_part1(Lines) ->
    Nums = lists:map(fun filter_nums/1, Lines),
    lists:map(fun({X, Y}) -> io:format("~s => ~p~n", [X, Y]) end,
              aoc_util:list_merge(Lines, Nums)),
    Pairs = lists:map(fun to_pair/1, Nums),
    lists:sum(Pairs).

solve_part2(Lines) ->
    solve_part1(lists:map(fun parse_strnums/1, Lines)).

filter_nums(List) ->
    lists:filter(fun is_num/1, List).

is_num(C) -> C >= $0 andalso C =< $9.

to_pair([X]) -> list_to_integer([X, X]);
to_pair([X|T]) ->
    [Y|_] = lists:reverse(T),
    list_to_integer([X, Y]).

parse_strnums(L) -> parse_strnums(L, []).

parse_strnums([], Acc)           -> lists:reverse(Acc);
parse_strnums("one" ++ T, Acc)   -> parse_strnums("ne" ++ T,   [$1|Acc]);
parse_strnums("two" ++ T, Acc)   -> parse_strnums("wo" ++ T,   [$2|Acc]);
parse_strnums("three" ++ T, Acc) -> parse_strnums("hree" ++ T, [$3|Acc]);
parse_strnums("four" ++ T, Acc)  -> parse_strnums("our" ++ T,  [$4|Acc]);
parse_strnums("five" ++ T, Acc)  -> parse_strnums("ive" ++ T,  [$5|Acc]);
parse_strnums("six" ++ T, Acc)   -> parse_strnums("ix" ++ T,   [$6|Acc]);
parse_strnums("seven" ++ T, Acc) -> parse_strnums("even" ++ T, [$7|Acc]);
parse_strnums("eight" ++ T, Acc) -> parse_strnums("ight" ++ T, [$8|Acc]);
parse_strnums("nine" ++ T, Acc)  -> parse_strnums("ine" ++ T,  [$9|Acc]);
parse_strnums([H|T], Acc)        -> parse_strnums(T,           [H|Acc]).
