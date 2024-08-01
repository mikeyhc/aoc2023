-module(lens).

-export([solve/1]).

solve(Filename) ->
    [Line] = aoc_util:read_lines(Filename),
    Parts = string:split(Line, ",", all),
    Commands = build_commands(Parts),
    {solve_part1(Parts), solve_part2(Commands)}.

solve_part1(Strings) ->
    lists:sum(lists:map(fun hash/1, Strings)).

hash(String) ->
    lists:foldl(fun(C, A) -> (A + C) * 17 rem 256 end, 0, String).

build_commands(Strings) ->
    lists:map(fun parse_command/1, Strings).

parse_command(String) ->
    case string:split(String, "=") of
        [Lens, [Focal]] -> {set, Lens, Focal -$0};
        [Lens] -> {remove, lists:droplast(Lens)}
    end.

solve_part2(Commands) ->
    Boxes = lists:foldl(fun run_command/2, #{}, Commands),
    score_boxes(Boxes).

run_command(Command, Boxes) ->
    Hash = hash(get_lens(Command)),
    Box0 = maps:get(Hash, Boxes, []),
    Box1 = case Command of
               {remove, Lens} ->
                   lists:keydelete(Lens, 1, Box0);
               {set, Lens, Focal} ->
                   lists:keystore(Lens, 1, Box0, {Lens, Focal})
           end,
    Boxes#{Hash => Box1}.

get_lens({remove, Lens}) -> Lens;
get_lens({set, Lens, _Focal}) -> Lens.

score_boxes(Boxes) ->
    lists:sum(lists:map(fun score_box/1, maps:to_list(Boxes))).

score_box({BoxId, Box}) ->
    lists:sum(lists:map(fun score_lens/1,
                        lists:zip3(
                          lists:duplicate(length(Box), BoxId),
                          lists:seq(1, length(Box)),
                          Box))).

score_lens({BoxId, Pos, {_Lens, Focal}}) ->
    (BoxId + 1) * Pos * Focal.
