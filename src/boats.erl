-module(boats).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    Races0 = parse_races(Lines),
    Races1 = parse_bad_races(Lines),
    {solve_part1(Races0), count_wins(Races1)}.

solve_part1(Races) ->
    lists:foldl(fun(Race, Prod) -> count_wins(Race) * Prod end, 1, Races).

parse_races([Time, Dist]) ->
    TimeParts = lists:filter(fun not_empty/1, tl(string:split(Time, " ", all))),
    DistParts = lists:filter(fun not_empty/1, tl(string:split(Dist, " ", all))),
    lists:zip(lists:map(fun list_to_integer/1, TimeParts),
              lists:map(fun list_to_integer/1, DistParts)).

parse_bad_races([Time, Dist]) ->
    TimeParts = lists:filter(fun not_empty/1, tl(string:split(Time, " ", all))),
    DistParts = lists:filter(fun not_empty/1, tl(string:split(Dist, " ", all))),
    {list_to_integer(lists:flatten(TimeParts)),
     list_to_integer(lists:flatten(DistParts))}.

count_wins({Time, Dist}) ->
    length(lists:filter(fun(V) -> V * (Time - V) > Dist end,
                        lists:seq(1, Time))).

not_empty([]) -> false;
not_empty(_) -> true.
