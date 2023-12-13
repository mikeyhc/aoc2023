-module(pipemaze).

-export([solve/1]).

solve(Filename) ->
    Map0 = aoc_map:to_map(aoc_map:read_map(Filename, [skip_dots])),
    {Start, _} = lists:keyfind($S, 2, maps:to_list(Map0)),
    Map1 = create_link_map(Map0),
    Steps = solve_part1(Start, Map1),
    {Steps, solve_part2(Start, Steps, Map1)}.

solve_part1(Start, Map) ->
    {Left, Right} = find_first(Start, Map),
    walk_path(Left, Start, Right, Start, Map, 1).

walk_path(Match, _LPrev, Match, _RPrev, _Map, Steps) -> Steps;
walk_path(Left, LPrev, Right, RPrev, Map, Steps) ->
    LNext = get_next(Left, LPrev, Map),
    RNext = get_next(Right, RPrev, Map),
    walk_path(LNext, Left, RNext, Right, Map, Steps+1).

solve_part2(Start, Steps, Map) ->
    Area = find_shoelace(Start, Map),
    modified_picks(Steps * 2, Area).

% use the shoelace formula to find the area
find_shoelace(Start, Map) ->
    {Left, Right} = find_first(Start, Map),
    Out1 = do_shoelace(Start, Left, Start, Start, Map, 0) * (1/2),
    Out2 = do_shoelace(Start, Right, Start, Start, Map, 0) * (1/2),
    % I cant know which one is counter-clockwise, but clockwise will be neg
    max(Out1, Out2).

do_shoelace(_Prev, End={CY, CX}, {PY, PX}, End, _Map, Size) ->
    CX*PY - PX*CY + Size;
do_shoelace(Prev, Cur={CY, CX}, LV0={PY, PX}, End, Map, Size) ->
    Next = get_next(Cur, Prev, Map),
    {V, LV1} = case is_vertex(Cur, Map) of
                   true -> {CX*PY - PX*CY, Cur};
                   false -> {0, LV0}
               end,
    do_shoelace(Cur, Next, LV1, End, Map, Size+V).

% modified version of picks theorem to get internal area
modified_picks(B, A) ->
    trunc(-(B * 0.5) + 1 + A).

%% helper methods

get_next(Cur, Prev, Map) ->
    case maps:get(Cur, Map) of
        {_, Prev, Next} -> Next;
        {_, Next, Prev} -> Next
    end.

find_first(Start={SX, SY}, Map) ->
    Possible = [{SX-1, SY}, {SX+1, SY}, {SX, SY-1}, {SX, SY+1}],
    ConnectStart = fun(Coord) ->
        case maps:get(Coord, Map, false) of
            {_, Start, _} -> true;
            {_, _, Start} -> true;
            _ -> false
        end
    end,
    [Left, Right] = lists:filter(ConnectStart, Possible),
    {Left, Right}.

is_vertex(Coord, Map) ->
    {IsVertex, _ , _} = maps:get(Coord, Map),
    IsVertex.

create_link_map(Map) ->
    maps:from_list(lists:map(fun symbol_to_link/1, maps:to_list(Map))).

symbol_to_link({C={Y, X}, $|}) -> {C, {false, {Y-1, X}, {Y+1, X}}};
symbol_to_link({C={Y, X}, $-}) -> {C, {false, {Y, X-1}, {Y, X+1}}};
symbol_to_link({C={Y, X}, $L}) -> {C, {true, {Y-1, X}, {Y, X+1}}};
symbol_to_link({C={Y, X}, $J}) -> {C, {true, {Y, X-1}, {Y-1, X}}};
symbol_to_link({C={Y, X}, $7}) -> {C, {true, {Y+1, X}, {Y, X-1}}};
symbol_to_link({C={Y, X}, $F}) -> {C, {true, {Y, X+1}, {Y+1, X}}};
symbol_to_link({C, $S}) -> {C, false}.
