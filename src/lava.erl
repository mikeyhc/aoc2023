-module(lava).

-export([solve/1]).

solve(Filename) ->
    Map = aoc_map:read_map(Filename),
    {solve_part1(Map), solve_part2(Map)}.

solve_part1(Map) ->
    Visited = track_light([{{1, 0}, right}], Map, #{}),
    maps:size(Visited).

solve_part2(Map) ->
    Starts = build_starts(Map),
    Fn = fun(Start, Acc) ->
                 Size = maps:size(track_light([Start], Map, #{})),
                 if Size > Acc -> Size;
                    true -> Acc
                 end
         end,
    lists:foldl(Fn, 0, Starts).

track_light([], _Map, Visited) -> Visited;
track_light([{Pos0, Dir}|Rest], Map, Visited0) ->
    Pos1 = next_pos(Pos0, Dir),
    case valid_position(Pos1, Map) of
        false -> track_light(Rest, Map, Visited0);
        true ->
            case visited(Pos1, Dir, Visited0) of
                true -> track_light(Rest, Map, Visited0);
                false ->
                    Tracks = process_square(Pos1, Dir, Map),
                    Visited1 = add_visited(Pos1, Dir, Visited0),
                    track_light(Tracks ++ Rest, Map, Visited1)
            end
    end.

next_pos({Row, Col}, Dir) ->
    case Dir of
        left -> {Row, Col - 1};
        right -> {Row, Col + 1};
        up -> {Row - 1, Col};
        down -> {Row + 1, Col}
    end.

valid_position({Row, Col}, Map) ->
    {Rows, Cols} = aoc_map:size(Map),
    Row > 0 andalso Col > 0 andalso Row =< Rows andalso Col =< Cols.

visited(Pos, Dir, Visited) ->
    Visits = maps:get(Pos, Visited, []),
    lists:member(Dir, Visits).

add_visited(Pos, Dir, Visited) ->
    Visits = maps:get(Pos, Visited, []),
    Visited#{Pos => [Dir|Visits]}.

process_square(Pos, Dir, Map) ->
    case aoc_map:get_value(Map, Pos) of
        $. -> [{Pos, Dir}];
        $/ -> [{Pos, forward_mirror(Dir)}];
        $\\ -> [{Pos, backward_mirror(Dir)}];
        $- -> horizontal_splitter(Pos, Dir);
        $| -> vertical_splitter(Pos, Dir)
    end.

forward_mirror(left) -> down;
forward_mirror(right) -> up;
forward_mirror(up) -> right;
forward_mirror(down) -> left.

backward_mirror(left) -> up;
backward_mirror(right) -> down;
backward_mirror(up) -> left;
backward_mirror(down) -> right.

horizontal_splitter(Pos, left) -> [{Pos, left}];
horizontal_splitter(Pos, right) -> [{Pos, right}];
horizontal_splitter(Pos, up) -> [{Pos, left}, {Pos, right}];
horizontal_splitter(Pos, down) -> [{Pos, left}, {Pos, right}].

vertical_splitter(Pos, left) -> [{Pos, up}, {Pos, down}];
vertical_splitter(Pos, right) -> [{Pos, up}, {Pos, down}];
vertical_splitter(Pos, up) -> [{Pos, up}];
vertical_splitter(Pos, down) -> [{Pos, down}].

build_starts(Map) ->
    {Rows, Cols} = aoc_map:size(Map),
    RowNumbers = lists:seq(1, Rows),
    ColNumbers = lists:seq(1, Cols),
    Left = lists:zip(lists:zip(RowNumbers, lists:duplicate(Rows, 0)),
                     lists:duplicate(Rows, right)),
    Right = lists:zip(lists:zip(RowNumbers, lists:duplicate(Rows, Cols + 1)),
                      lists:duplicate(Rows, left)),
    Top = lists:zip(lists:zip(lists:duplicate(Cols, 0), ColNumbers),
                    lists:duplicate(Cols, down)),
    Bottom = lists:zip(lists:zip(lists:duplicate(Cols, Rows + 1), ColNumbers),
                       lists:duplicate(Cols, up)),
    lists:flatten([Left, Right, Top, Bottom]).
