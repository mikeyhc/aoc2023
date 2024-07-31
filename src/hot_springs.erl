-module(hot_springs).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    Springs = parse_springs(Lines),
    Extended = parse_extended(Lines),
    {solve_part1(Springs), solve_part1(Extended)}.

solve_part1(Springs) ->
    lists:sum(lists:map(fun solve_line/1, Springs)).

solve_line({Map, Counts}) ->
    queue_loop(hs_queue:insert(hs_queue:new(), Map, Counts, 1),
               fun solve_springs/3, 0).

queue_loop(Queue0, Fun, Acc0) ->
    case hs_queue:pop(Queue0) of
        false -> Acc0;
        {Key, Value, Queue1} ->
            {QueueUpdates, Acc1} = Fun(Key, Value, Acc0),
            Queue2 = lists:foldl(
                       fun({K, V, C}, Q) -> hs_queue:insert(Q, K, V, C) end,
                       Queue1, QueueUpdates),
            queue_loop(Queue2, Fun, Acc1)
    end.

solve_springs(Map, Value, Acc) ->
    lists:foldl(fun(V, {Updates0, Count0}) ->
                        {Updates1, Count1} = bind_group(Map, V),
                        {Updates1 ++ Updates0, Count0 + Count1}
                end, {[], Acc}, Value).

bind_group(Map, {[], Count}) ->
    case lists:all(fun(L) -> lists:all(fun(C) -> C =:= $? end, L) end, Map) of
        true -> {[], Count};
        false -> {[], 0}
    end;
bind_group(Map, {[Match|Rest], Count}) ->
    Arragements = generate_bindings(Map, Match, []),
    Updates = lists:map(fun(M) -> {M, Rest, Count} end, Arragements),
    {Updates, 0}.

generate_bindings([], _Value, Acc) -> Acc;
generate_bindings([L=[$#|_]|Rest], Value, Acc) ->
    Len = length(L),
    if Len =:= Value -> [Rest|Acc];
       Len < Value -> Acc;
       Len > Value ->
           case lists:nth(Value + 1, L) of
               $# -> Acc;
               $? -> case lists:nthtail(Value + 1, L) of
                         [] -> [Rest|Acc];
                         Tail -> [[Tail|Rest]|Acc]
                     end
           end
    end;
generate_bindings([L=[$?|T]|Rest], Value, Acc) ->
    Len = length(L),
    if Len =:= Value ->
           case lists:member($#, T) of
               true -> [Rest|Acc];
               false -> generate_bindings(Rest, Value, [Rest|Acc])
           end;
       Len < Value ->
           case lists:member($#, T) of
               true -> Acc;
               false -> generate_bindings(Rest, Value, Acc)
           end;
       Len > Value ->
           case lists:nth(Value + 1, L) of
               $# -> generate_bindings([T|Rest], Value, Acc);
               $? ->
                   case lists:nthtail(Value + 1, L) of
                       [] -> generate_bindings([T|Rest], Value, [Rest|Acc]);
                       Tail -> generate_bindings([T|Rest], Value,
                                                 [[Tail|Rest]|Acc])
                   end
           end
    end.

parse_springs(Lines) ->
    lists:map(fun parse_line/1, Lines).

parse_extended(Lines) ->
    lists:map(fun parse_extended_line/1, Lines).

parse_line(Line) ->
    [FullMap, AllCounts] = string:split(Line, " "),
    Map = lists:filter(fun(S) -> S =/= "" end, string:split(FullMap, ".", all)),
    Counts = lists:map(fun list_to_integer/1,
                       string:split(AllCounts, ",", all)),
    {Map, Counts}.

parse_extended_line(Line) ->
    [FullMap, AllCounts] = string:split(Line, " "),
    ExtendedMap = string:join(lists:duplicate(5, FullMap), "?"),
    Map = lists:filter(fun(S) -> S =/= "" end,
                       string:split(ExtendedMap, ".", all)),
    Counts = lists:map(fun list_to_integer/1,
                       string:split(AllCounts, ",", all)),
    {Map, lists:flatten(lists:duplicate(5, Counts))}.
