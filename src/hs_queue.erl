-module(hs_queue).

-export([new/0, insert/4, pop/1]).

new() -> #{}.

insert(Queue, Key, Value, Count) ->
    maps:update_with(Key, fun(Old) -> value_merge(Old, Value, Count) end,
                     new_value(Value, Count), Queue).

pop(Queue) ->
    case maps:size(Queue) of
        0 -> false;
        _ -> pop_(Queue)
    end.

pop_(Queue) ->
    KVList = maps:to_list(Queue),
    LongestKey = lists:map(fun(T={K, _V}) -> {lists:flatten(K), T} end, KVList),
    Sorted = lists:sort(fun({K0, _}, {K1, _}) -> length(K0) > length(K1) end,
                        LongestKey),
    [{_, {Key, Value}}|_] = Sorted,
    {Key, value_list(Value), maps:remove(Key, Queue)}.

%% value functions

new_value(Value, Count) -> #{Value => Count}.

value_merge(ValueMap, Value, Count) ->
    maps:update_with(Value, fun(Old) -> Old + Count end, Count, ValueMap).

value_list(Value) -> maps:to_list(Value).
