-module(cosmic).

-export([solve/1]).

-define(EXPAND_SIZE, 2).
-define(EXPAND_SIZE_2, 1_000_000).

solve(Filename) ->
    Map = aoc_map:to_map(aoc_map:read_map(Filename, [skip_dots])),
    {UsedRows, UsedCols} = find_used(Map),
    {solve_part1(Map, UsedRows, UsedCols, ?EXPAND_SIZE),
     solve_part1(Map, UsedRows, UsedCols, ?EXPAND_SIZE_2)}.

solve_part1(Map, UsedRows, UsedCols, Expansion) ->
    Galaxies = maps:keys(Map),
    Dists = count_distances(Galaxies, UsedRows, UsedCols, Expansion, []),
    lists:sum(Dists).

count_distances([], _UsedRows, _UsedCols, _Expansion, Acc) -> Acc;
count_distances([H|T], UsedRows, UsedCols, Expansion, Acc) ->
    Dists = lists:map(fun(G) ->
        distance_between(H, G, UsedRows, UsedCols, Expansion)
    end, T),
    count_distances(T, UsedRows, UsedCols, Expansion, Dists ++ Acc).

distance_between(Start, End, UsedRows, UsedCols, Expansion) ->
    {Vertical, Horizontal} = create_ranges(Start, End),
    LengthA = calculate_length(Vertical, UsedRows, Expansion),
    LengthB = calculate_length(Horizontal, UsedCols, Expansion),
    LengthA + LengthB.

create_ranges({Y0, X0}, {Y1, X1}) ->
    {range:new(Y0, Y1), range:new(X0, X1)}.

calculate_length(Range, Exclude, Expansion) ->
    Hits = sets:fold(fun(X, Acc) ->
                             case range:contains(Range, X) of
                                 true -> Acc + 1;
                                 false -> Acc
                             end
                     end, 0, Exclude) - 1,
    (range:size(Range) - Hits) * Expansion + Hits.


find_used(Map) ->
    {Cols, Rows} = lists:unzip(maps:keys(Map)),
    {sets:from_list(Cols), sets:from_list(Rows)}.

