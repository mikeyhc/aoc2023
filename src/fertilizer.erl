-module(fertilizer).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    {Seeds, Maps} = parse_almanac(Lines),
    {solve_part1(Seeds, Maps), solve_part2(Seeds, Maps)}.

solve_part1(Seeds, Maps) ->
    [L0|Locations] = lists:foldl(fun apply_map/2, Seeds, Maps),
    lists:foldl(fun min/2, L0, Locations).

solve_part2(Seeds0, Maps0) ->
    BuildRange = fun({S, R}) -> range:new(S, S+R-1) end,
    MapToRanges = fun({D, S, R}) ->
        {BuildRange({D, R}), BuildRange({S, R})}
    end,
    Seeds = lists:map(BuildRange, aoc_util:pair_list(Seeds0)),
    Maps = lists:map(fun(M) -> lists:map(MapToRanges, M) end, Maps0),
    [L0|Locations] = lists:foldl(fun apply_single_range_map/2, Seeds, Maps),
    lists:foldl(fun(X, Y) -> min(range:start(X), Y) end, range:start(L0),
                Locations).

parse_almanac([SeedLine|Rest]) ->
    [_|Seeds] = string:split(SeedLine, " ", all),
    Maps = lists:foldl(fun parse_map/2, [], Rest),
    {lists:map(fun list_to_integer/1, Seeds), lists:reverse(Maps)}.

parse_map("", Acc) -> [[]|Acc];
parse_map([C|_], Acc) when C >= $a andalso C =< $z ->
    Acc;
parse_map(Line, [H|T]) ->
    [Dest, Src, Len] = lists:map(fun list_to_integer/1,
                                 string:split(Line, " ", all)),
    [[{Dest, Src, Len}|H]|T].

apply_map(Map, Data) ->
    SearchFn = fun(V) ->
        fun({_Dest, Src, Range}) -> V >= Src andalso V =< Src + Range - 1 end
    end,
    MapFn = fun(V) ->
        case lists:search(SearchFn(V), Map) of
            {value, {Dest, Src, _Range}} -> Dest + (V - Src);
            false -> V
        end
    end,
    lists:map(MapFn, Data).

apply_single_range_map(Map, Data) ->
    {Unmapped, Mapped} = lists:foldl(fun(M, {D, A}) ->
                                             apply_single_range_map(M, D, A)
                                     end, {Data, []}, Map),
    Unmapped ++ Mapped.

apply_single_range_map({Dst, Src}, Unmapped0, Mapped0) ->
    Fn = fun(SeedRange, {Unmapped, Mapped}) ->
        case range:intersect_slice(SeedRange, Src) of
            {InRange, OutRange} ->
                MappedIn = range:add_start(range:sub_start(InRange, Src), Dst),
                {OutRange ++ Unmapped, [MappedIn|Mapped]};
            false -> {[SeedRange|Unmapped], Mapped}
        end
    end,
    lists:foldl(Fn, {[],  Mapped0}, Unmapped0).
