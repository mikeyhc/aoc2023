-module(gears).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    {NumMap, PartMap} = build_maps(Lines),
    {solve_part1(NumMap, PartMap), solve_part2(NumMap, PartMap)}.

solve_part1(NumMap, PartMap) ->
    HasPart = lists:filter(fun(V) -> has_part(V, PartMap) end, NumMap),
    lists:sum(lists:map(fun({K, _, _, _}) -> K end, HasPart)).

solve_part2(NumMap, PartMap) ->
    Gears = get_gear_coords(PartMap),
    GearNums = lists:map(fun(V) -> find_nearby_nums(V, NumMap) end, Gears),
    ValidGears = lists:filter(fun(V) -> length(V) =:= 2 end, GearNums),
    lists:sum(lists:map(fun([{N0, _, _, _}, {N1, _, _, _}]) -> N0 * N1 end,
                        ValidGears)).

build_maps(Lines) ->
    lists:foldl(fun read_row/2, {[], #{}}, lists:enumerate(Lines)).

read_row({RowNum, Row}, Maps) -> read_row(RowNum, 1, Row, Maps).

read_row(_RowNum, _ColNum, [], Maps) -> Maps;
read_row(RowNum, ColNum, [$.|T], Maps) -> read_row(RowNum, ColNum+1, T, Maps);
read_row(RowNum, ColNum, [H|T], Maps) when H >= $0 andalso H =< $9 ->
    slurp_number(RowNum, ColNum, [H|T], [], Maps);
read_row(RowNum, ColNum, [H|T], {NumMap, PartMap}) ->
    read_row(RowNum, ColNum+1, T, {NumMap, PartMap#{{RowNum, ColNum} => H}}).

slurp_number(RowNum, ColNum, [H|T], Acc, Maps) when H >= $0 andalso H =< $9 ->
    slurp_number(RowNum, ColNum+1, T, [H|Acc], Maps);
slurp_number(RowNum, ColNum, Rest, Acc, {NumMap, PartMap}) ->
    Num = list_to_integer(lists:reverse(Acc)),
    read_row(RowNum, ColNum, Rest,
             {[{Num, RowNum, ColNum - length(Acc), ColNum - 1}|NumMap],
              PartMap}).

has_part({_Num, Row, ColA, ColB}, PartMap) ->
    ColSeq = lists:seq(ColA-1, ColB+1),
    ColLen = length(ColSeq),
    CoordsA = lists:zip(lists:duplicate(ColLen, Row - 1), ColSeq),
    CoordsB = lists:zip(lists:duplicate(ColLen, Row), ColSeq),
    CoordsC = lists:zip(lists:duplicate(ColLen, Row + 1), ColSeq),
    lists:any(fun(V) -> maps:is_key(V, PartMap) end,
              CoordsA ++ CoordsB ++ CoordsC).

get_gear_coords(PartMap) ->
    Gears = lists:filter(fun({_, V}) -> V =:= $* end, maps:to_list(PartMap)),
    lists:map(fun({K, _}) -> K end, Gears).

find_nearby_nums({GRow, GCol}, NumMap) ->
    Filter = fun({_Num, CRow, CColA, CColB}) ->
        (GRow >= CRow - 1 andalso GRow =< CRow + 1)
        andalso
        (GCol >= CColA - 1 andalso GCol =< CColB + 1)
    end,
    lists:filter(Filter, NumMap).
