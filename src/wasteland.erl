-module(wasteland).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    {Steps, Maps} = parse_docs(Lines),
    % {solve_part1(Steps, Maps), solve_part2(Steps, Maps)}.
    {solve_part1(Steps, Maps), solve_part2(Steps, Maps)}.

solve_part1(Steps, Maps) ->
    walk_map(Steps, Steps, "AAA", Maps, 0).

solve_part2(Steps, Maps) ->
    SpawnJobs = fun(S=[_, _, $A], Acc) ->
                        Ref = make_ref(),
                        spawn_walk(Ref, S, Steps, Maps),
                        [Ref|Acc];
                   (_, Acc) -> Acc
                end,
    Refs = lists:foldl(SpawnJobs, [], maps:keys(Maps)),
    Counts = lists:map(fun await_ref/1, Refs),
    aoc_util:lcm(Counts).

spawn_walk(Ref, Start, Steps, Maps) ->
    Self = self(),
    spawn(fun() ->
        Self ! {Ref, ghost_walk_map(Steps, Steps, Start, Maps, 0)}
    end).

await_ref(Ref) ->
    receive
        {Ref, Steps} -> Steps
    end.

walk_map(_Current, _Steps, "ZZZ", _Maps, Count) -> Count;
walk_map([], Steps, Pos, Maps, Count) ->
    walk_map(Steps, Steps, Pos, Maps, Count);
walk_map([H|T], Steps, Pos, Maps, Count) ->
    #{Pos := {Left, Right}}= Maps,
    case H of
        $L -> walk_map(T, Steps, Left, Maps, Count+1);
        $R -> walk_map(T, Steps, Right, Maps, Count+1)
    end.

ghost_walk_map(_Current, _Steps, [_, _, $Z], _Maps, Count) ->
    Count;
ghost_walk_map([], Steps, Pos, Maps, Count) ->
    ghost_walk_map(Steps, Steps, Pos, Maps, Count);
ghost_walk_map([H|T], Steps, Pos, Maps, Count) ->
    #{Pos := {Left, Right}}= Maps,
    case H of
        $L -> ghost_walk_map(T, Steps, Left, Maps, Count+1);
        $R -> ghost_walk_map(T, Steps, Right, Maps, Count+1)
    end.

parse_docs([Steps, _Empty|SMapList]) ->
    {Steps, maps:from_list(lists:map(fun parse_map/1, SMapList))}.

parse_map(SMap) ->
    Parts = string:split(SMap, " ", all),
    [Start, "=", DLeft, DRight] = Parts,
    [$(, LA, LB, LC, $,] = DLeft,
    [RA, RB, RC, $)] = DRight,
    {Start, {[LA, LB, LC], [RA, RB, RC]}}.
