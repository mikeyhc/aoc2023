-module(scratchcards).

-export([solve/1]).

-record(card, {id, winning, numbers}).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    Cards = lists:map(fun parse_card/1, Lines),
    CardMap = maps:from_list(lists:map(fun(V) -> {V#card.id, V} end, Cards)),
    {solve_part1(Cards), solve_part2(CardMap)}.

parse_card(Line) ->
    Parts0 = string:split(Line, " ", all),
    [_, Id0|Parts1] = lists:filter(fun(X) -> X =/= "" end, Parts0),
    Id = list_to_integer(lists:droplast(Id0)),
    {SWinning, [_|SNumbers]} = lists:splitwith(fun(V) -> V =/= "|" end,
                                               Parts1),
    Winning = sets:from_list(lists:map(fun list_to_integer/1, SWinning)),
    Numbers = lists:map(fun list_to_integer/1, SNumbers),
    #card{id=Id, winning=Winning, numbers=Numbers}.

solve_part1(Cards) ->
    lists:sum(lists:map(fun score_card/1, Cards)).

solve_part2(CardMap) ->
    StartingCards = lists:map(fun({_, V}) -> {V, 1} end,
                              lists:keysort(1, maps:to_list(CardMap))),
    TotalCards = grow_cards(StartingCards, []),
    lists:sum(TotalCards).

score_card(Card) ->
    case card_wins(Card) of
        0 -> 0;
        1 -> 1;
        N -> floor(math:pow(2, N-1))
    end.

card_wins(#card{winning=Winning, numbers=Numbers}) ->
    InWinning = fun(V) -> sets:is_element(V, Winning) end,
    length(lists:filter(InWinning, Numbers)).

grow_cards([], Acc) -> Acc;
grow_cards([{Card, Count}|T], Acc) ->
    Wins = card_wins(Card),
    grow_cards(inc_tail(Wins, Count, T), [Count|Acc]).

inc_tail(_Depth, _Count, []) -> [];
inc_tail(0, _Count, L) -> L;
inc_tail(Depth, Count, [{Card, C}|T]) ->
    [{Card, C+Count}|inc_tail(Depth - 1, Count, T)].
