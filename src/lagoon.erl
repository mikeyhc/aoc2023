-module(lagoon).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    {Part1, Part2} = parse_lines(Lines),
    {solve_part1(Part1), solve_part1(Part2)}.

solve_part1(Input) ->
    {Path, Perimeter} = walk_path(Input),
    Area = shoelace({0, 0}, Path),
    modified_picks(Area, Perimeter).

shoelace(Start, Path) ->
    trunc(do_shoelace(maps:get(Start, Path), Start, Start, Path, 0) * (1/2)).

do_shoelace(End={CY, CX}, {PY, PX}, End, _Path, Size) ->
    CX*PY - PX*CY + Size;
do_shoelace(Cur={CY, CX}, {PY, PX}, End, Path, Size) ->
    Next = maps:get(Cur, Path),
    do_shoelace(Next, Cur, End, Path, Size + (CX*PY - PX*CY)).

modified_picks(A, B) ->
    A + 1 + trunc(B / 2).

walk_path(Steps) ->
    walk_path(Steps, {0, 0}, #{}, 0).

walk_path([], _Last, Path, Perim) -> {Path, Perim};
walk_path([{Dir, N}|Rest], Last, Path, Perim) ->
    Next = aoc_2d:move_point(Last, Dir, N),
    walk_path(Rest, Next, Path#{Next => Last}, Perim + N).

parse_lines(Lines) ->
    lists:unzip(lists:map(fun parse_line/1, Lines)).

parse_line(Line) ->
    [[Dir], Dist, Color] = string:split(Line, " ", all),
    {{aoc_2d:char_dir(Dir), list_to_integer(Dist)}, parse_color(Color)}.

parse_color("(#" ++ Color) ->
    {HexDist, [DirChar|")"]} = lists:split(5, Color),
    {num_dir(DirChar), list_to_integer(HexDist, 16)}.

num_dir($0) -> right;
num_dir($1) -> down;
num_dir($2) -> left;
num_dir($3) -> up.

find_bounds(Path) ->
    lists:foldl(fun({Y,X}, {MaxY0,MaxX0}) ->
                        MaxY = max(Y, MaxY0),
                        MaxX = max(X, MaxX0),
                        {MaxY, MaxX}
                end, {0, 0}, maps:keys(Path)).

draw_path(Path, {MaxY, MaxX}) ->
    lists:foreach(fun(Y) -> draw_row(Path, Y, MaxX) end,
                  lists:seq(0, MaxY)).

draw_row(Path, Y, MaxX) ->
    Row = lists:map(fun(X) ->
                            case maps:is_key({Y, X}, Path) of
                                true -> $#;
                                false -> $.
                            end
                    end, lists:seq(0, MaxX)),
    io:format("~s~n", [Row]).
