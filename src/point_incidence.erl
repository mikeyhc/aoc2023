-module(point_incidence).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    Maps = build_maps(Lines),
    {solve_part1(Maps), solve_part2(Maps)}.

solve_part1(Maps) ->
    FirstFn = fun(Map) ->
                      case find_reflection(Map) of
                          false -> {false, Map};
                          T -> T
                      end
              end,
    {Hztl0, Vert0} = lists:partition(fun({Hztl, _}) -> Hztl end,
                                     lists:map(FirstFn, Maps)),
    Vert1 = lists:map(fun({_, M}) -> find_reflection(transpose(M)) end, Vert0),
    present_results(Hztl0, Vert1).

solve_part2(Maps) ->
    FirstFn = fun(Map) ->
                      case find_smudged_reflection(Map) of
                          false -> {false, Map};
                          T -> T
                      end
              end,
    {Hztl0, Vert0} = lists:partition(fun({Hztl, _}) -> Hztl end,
                                     lists:map(FirstFn, Maps)),
    Vert1 = lists:map(fun({_, M}) -> find_smudged_reflection(transpose(M)) end,
                      Vert0),
    present_results(Hztl0, Vert1).

present_results(Hztl, Vert) ->
    HztlCount = lists:sum(lists:map(fun({_, V}) -> V end, Hztl)),
    VertCount = lists:sum(lists:map(fun({_, V}) -> V end, Vert)),
    HztlCount * 100 + VertCount.

find_reflection(Map) -> find_reflection(Map, []).

find_reflection([_], _Acc) -> false;
find_reflection([First|Rest], Acc) ->
    case check_reflection([First|Acc], Rest) of
        true -> {true, length(Acc) + 1};
        false -> find_reflection(Rest, [First|Acc])
    end.

check_reflection([], _Right) -> true;
check_reflection(_Left, []) -> true;
check_reflection([LH|LT], [RH|RT]) ->
    if LH =:= RH -> check_reflection(LT, RT);
       true -> false
    end.

find_smudged_reflection(Map) -> find_smudged_reflection(Map, []).

find_smudged_reflection([_], _Acc) -> false;
find_smudged_reflection([First|Rest], Acc) ->
    case check_smudged_reflection([First|Acc], Rest) of
        true -> {true, length(Acc) + 1};
        false -> find_smudged_reflection(Rest, [First|Acc])
    end.

check_smudged_reflection(Left, Right) ->
    check_smudged_reflection(Left, Right, true).

check_smudged_reflection([], _Right, Smudge) -> not Smudge;
check_smudged_reflection(_Left, [], Smudge) -> not Smudge;
check_smudged_reflection([LH|LT], [RH|RT], Smudge) ->
    case compare_smudge(LH, RH, Smudge) of
        smudge -> check_smudged_reflection(LT, RT, false);
        true -> check_smudged_reflection(LT, RT, Smudge);
        false -> false
    end.

compare_smudge(Left, Right, Smudge) ->
    Diff = lists:filter(fun({L, R}) -> L =/= R end, lists:zip(Left, Right)),
    case length(Diff) of
        0 -> true;
        1 when Smudge -> smudge;
        _ -> false
    end.

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M)|transpose(lists:map(fun tl/1, M))].

build_maps(Lines) ->
    lists:reverse(build_maps(Lines, [])).

build_maps([], Acc) -> Acc;
build_maps(Lines, Acc) ->
    case lists:splitwith(fun(L) -> L =/= [] end, Lines) of
        {Map, []} -> [Map|Acc];
        {Map, [_|Rest]} -> build_maps(Rest, [Map|Acc])
    end.
