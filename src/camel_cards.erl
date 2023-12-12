-module(camel_cards).

-export([solve/1]).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    Hands = parse_hands(Lines),
    {solve_part1(Hands), solve_part2(Hands)}.

solve_part1(Hands0) ->
    Hands1 = lists:map(fun add_lookup/1, Hands0),
    Hands2 = lists:map(fun add_score/1, Hands1),
    Hands3 = rank_hands(Hands2),
    lists:sum(lists:map(fun({R, B}) -> R * B end,
                        lists:zip(lists:map(fun({_, _, _, B}) -> B end, Hands3),
                                  lists:seq(1, length(Hands3))))).

solve_part2(Hands0) ->
    Hands1 = lists:map(fun add_lookup_wj/1, Hands0),
    Hands2 = lists:map(fun add_score/1, Hands1),
    Hands3 = rank_hands_wj(Hands2),
    lists:sum(lists:map(fun({R, B}) -> R * B end,
                        lists:zip(lists:map(fun({_, _, _, B}) -> B end, Hands3),
                                  lists:seq(1, length(Hands3))))).

parse_hands(Lines) ->
    MapFn = fun(Line) ->
        [Cards, Bid] = string:split(Line, " "),
        {Cards, list_to_integer(Bid)}
    end,
    lists:map(MapFn, Lines).

add_lookup({Cards, Bid}) ->
    CardLookup = make_lookup(Cards),
    {Cards, CardLookup, Bid}.

add_lookup_wj({Cards, Bid}) ->
    CardLookup = make_lookup_wj(Cards),
    {Cards, CardLookup, Bid}.

make_lookup(Cards) ->
    FoldFn = fun(C, M) -> maps:update_with(C, fun(V) -> V + 1 end, 1, M) end,
    CardCountList = maps:to_list(lists:foldl(FoldFn, #{}, Cards)),
    lists:reverse(lists:keysort(2, CardCountList)).

make_lookup_wj(Cards) ->
    FoldFn = fun(C, M) -> maps:update_with(C, fun(V) -> V + 1 end, 1, M) end,
    CardCountMap0 = lists:foldl(FoldFn, #{}, Cards),
    JokerCount = maps:get($J, CardCountMap0, 0),
    CardCountMap1 = maps:remove($J, CardCountMap0),
    case lists:reverse(lists:keysort(2, maps:to_list(CardCountMap1))) of
        [] -> [{$J, 5}];
        [{Card, Count}|T] -> [{Card, Count + JokerCount}|T]
    end.

add_score({Cards, Lookup, Bid}) ->
    Score = score_cards(Lookup),
    {Score, Cards, Lookup, Bid}.

rank_hands(Hands) ->
    lists:sort(fun hand_sort/2, Hands).

rank_hands_wj(Hands) ->
    lists:sort(fun hand_sort_wj/2, Hands).

hand_sort({S0, C0, _L0, _B0}, {S1, C1, _L1, _B1}) ->
    if S0 < S1 -> true;
       S0 > S1 -> false;
       true -> cardlist_sort(C0, C1)
    end.

hand_sort_wj({S0, C0, _L0, _B0}, {S1, C1, _L1, _B1}) ->
    if S0 < S1 -> true;
       S0 > S1 -> false;
       true -> cardlist_sort_wj(C0, C1)
    end.

cardlist_sort([], []) -> true;
cardlist_sort([C0|T0], [C1|T1]) ->
    Diff = card_score(C0) - card_score(C1),
    if Diff < 0 -> true;
       Diff > 0 -> false;
       true -> cardlist_sort(T0, T1)
    end.

cardlist_sort_wj([], []) -> true;
cardlist_sort_wj([C0|T0], [C1|T1]) ->
    Diff = card_score_wj(C0) - card_score_wj(C1),
    if Diff < 0 -> true;
       Diff > 0 -> false;
       true -> cardlist_sort_wj(T0, T1)
    end.

score_cards([{_, 5}]) -> 7;
score_cards([{_, 4}|_]) -> 6;
score_cards([{_, 3}, {_, 2}]) -> 5;
score_cards([{_, 3}|_]) -> 4;
score_cards([{_, 2}, {_, 2}|_]) -> 3;
score_cards([{_, 2}|_]) -> 2;
score_cards(_) -> 1.

card_score($A) -> 14;
card_score($K) -> 13;
card_score($Q) -> 12;
card_score($J) -> 11;
card_score($T) -> 10;
card_score(C) -> C - $1.

card_score_wj($A) -> 14;
card_score_wj($K) -> 13;
card_score_wj($Q) -> 12;
card_score_wj($J) -> 0;
card_score_wj($T) -> 10;
card_score_wj(C) -> C - $1.
