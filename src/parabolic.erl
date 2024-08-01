-module(parabolic).

-export([solve/1]).

%-define(MAX_CYCLES, 1_000_000_000).
-define(MAX_CYCLES, 1000).
-define(THRESHOLD, 20).

solve(Filename) ->
    Map = aoc_map:read_map(Filename),
    {solve_part1(Map), solve_part2(Map)}.

%% part 1

solve_part1(Map0) ->
    Map1 = move_north(Map0),
    score_stones(Map1).

move_north(Map) ->
    Elements = maps:to_list(aoc_map:to_map(Map)),
    Stones = lists:filter(fun({_, V}) -> V =:= $O end, Elements),
    move_stones(Map,
                lists:sort(fun({ARow, _ACol}, {BRow, _BCol}) -> ARow < BRow end,
                           lists:map(fun({K, _}) -> K end, Stones))).

move_stones(Map, Elements) ->
    lists:foldl(fun move_stone/2, Map, Elements).

move_stone({1, _Col}, Map) -> Map;
move_stone({Row, Col}, Map0) ->
    case aoc_map:get_value(Map0, Row - 1, Col) of
        $# -> Map0;
        $O -> Map0;
        $. ->
            Map1 = aoc_map:set_value(Map0, Row - 1, Col, $O),
            Map2 = aoc_map:set_value(Map1, Row, Col, $.),
            move_stone({Row - 1, Col}, Map2)
    end.

score_stones(Map) ->
    {Rows, _Cols} = aoc_map:size(Map),
    Elements = maps:to_list(aoc_map:to_map(Map)),
    Stones = lists:filter(fun({_, V}) -> V =:= $O end, Elements),
    lists:sum(lists:map(fun({{Row, _Col}, _Val}) -> Rows - Row + 1 end,
                        Stones)).

%% part 2

solve_part2(Map) ->
    Elements = maps:to_list(aoc_map:to_map(Map)),
    Stones = lists:filter(fun({_, V}) -> V =:= $O end, Elements),
    spin(Map, lists:map(fun({K, _}) -> K end, Stones), 1, #{}).

spin(Map, _Stones, ?MAX_CYCLES, _Scores) ->
    io:format("[WARNING] actually hit MAX_CYCLES~n"),
    score_stones(Map);
spin(Map0, Stones0, Cycle, Scores0) ->
    {Map1, Stones1} = spin_stones(Map0, Stones0),
    NewScore = score_stones(Map1),
    Scores1 = maps:update_with(NewScore, fun(L) -> [Cycle|L] end,
                               [Cycle], Scores0),
    case is_final(NewScore, Scores1) of
        true -> NewScore;
        false ->
            spin(Map1, Stones1, Cycle + 1, Scores1)
    end.

is_final(Score, Scores) ->
    Cycles = maps:get(Score, Scores),
    if length(Cycles) < ?THRESHOLD -> false;
       true ->
           Zipped = lists:zip(lists:droplast(Cycles), tl(Cycles)),
           [P|Period] = lists:map(fun({X, Y}) -> X - Y end, Zipped),
           case lists:all(fun(V) -> V =:= P end, Period) of
               true -> (?MAX_CYCLES - hd(Cycles)) rem P =:= 0;
               false -> false
           end
    end.

spin_stones(Map, Stones) ->
    lists:foldl(fun(Fn, V) -> Fn(V) end,
                {Map, Stones},
                [fun push_north/1,
                 fun push_west/1,
                 fun push_south/1,
                 fun push_east/1]).

push_north({Map, Stones}) ->
    push(Map, Stones,
         fun({R, C}) -> {R - 1, C} end,
         fun({AR, _AC}, {BR, _BC}) -> AR < BR end).

push_west({Map, Stones}) ->
    push(Map, Stones,
         fun({R, C}) -> {R, C - 1} end,
         fun({_AR, AC}, {_BR, BC}) -> AC < BC end).

push_south({Map, Stones}) ->
    push(Map, Stones,
         fun({R, C}) -> {R + 1, C} end,
         fun({AR, _AC}, {BR, _BC}) -> AR > BR end).

push_east({Map, Stones}) ->
    push(Map, Stones,
         fun({R, C}) -> {R, C + 1} end,
         fun({_AR, AC}, {_BR, BC}) -> AC > BC end).

push(Map, Stones, PushFn, SortFn) ->
    Sorted = lists:sort(SortFn, Stones),
    lists:foldl(fun(Stone, Acc) -> push_stone(Stone, Acc, PushFn) end,
                {Map, []}, Sorted).

push_stone(Pos={Row, Col}, {Map0, Out}, PushFn) ->
    {NewRow, NewCol} = PushFn(Pos),
    {MaxRow, MaxCol} = aoc_map:size(Map0),
    if NewRow < 1 orelse NewCol < 1 -> {Map0, [Pos|Out]};
       NewRow > MaxRow orelse NewCol > MaxCol -> {Map0, [Pos|Out]};
       true ->
           case aoc_map:get_value(Map0, NewRow, NewCol) of
               $# -> {Map0, [Pos|Out]};
               $O -> {Map0, [Pos|Out]};
               $. ->
                   Map1 = aoc_map:set_value(Map0, NewRow, NewCol, $O),
                   Map2 = aoc_map:set_value(Map1, Row, Col, $.),
                   push_stone({NewRow, NewCol}, {Map2, Out}, PushFn)
           end
    end.
