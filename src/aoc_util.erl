-module(aoc_util).

-export([read_lines/1, group_by/2, with_index/1]).

read_lines(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines0 = binary:split(Data, [<<"\n">>], [global]),
    [H|Lines1] = lists:reverse(Lines0),
    Lines2 = if H =:= <<>> -> lists:reverse(Lines1);
                true -> Lines0
             end,
    lists:map(fun binary_to_list/1, Lines2).

with_index(L) -> lists:zip(lists:seq(1, length(L)), L).

group_by(Pred, List) -> group_by(Pred, List, [], []).

group_by(_Pred, [], [], Final) -> lists:reverse(Final);
group_by(_Pred, [], Acc, Final) -> lists:reverse([lists:reverse(Acc)|Final]);
group_by(Pred, [H|T], Acc, Final) ->
    case Pred(H) of
        true -> group_by(Pred, T, [], [lists:reverse([H|Acc])|Final]);
        false -> group_by(Pred, T, [H|Acc], Final)
    end.
