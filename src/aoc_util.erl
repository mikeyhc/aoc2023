-module(aoc_util).

-export([read_lines/1, list_merge/2]).

read_lines(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Lines0 = binary:split(Data, [<<"\n">>], [global]),
    [_|Lines1] = lists:reverse(Lines0),
    Lines2 = lists:reverse(Lines1),
    lists:map(fun binary_to_list/1, Lines2).

list_merge(L1, L2) ->  list_merge(L1, L2, []).

list_merge([], [], Acc) -> lists:reverse(Acc);
list_merge([H1|T1], [H2|T2], Acc) ->
    list_merge(T1, T2, [{H1, H2}|Acc]).
