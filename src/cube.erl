-module(cube).

-export([solve/1]).

-define(MAX_RED, 12).
-define(MAX_GREEN, 13).
-define(MAX_BLUE, 14).

-record(draw, {red=0,
               blue=0,
               green=0}).

-record(game, {id,
               draws=[]
              }).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    Games = lists:map(fun parse_game/1, Lines),
    {solve_part1(Games), solve_part2(Games)}.

solve_part1(Games) ->
    Valid = lists:filter(fun valid_game/1, Games),
    lists:sum(lists:map(fun(X) -> X#game.id end, Valid)).

solve_part2(Games) ->
    Powers = lists:map(fun calc_power/1, Games),
    lists:sum(Powers).

parse_game(Line) ->
    Parts = string:split(Line, " ", all),
    ["Game", StrId|Rest] = Parts,
    Id = list_to_integer(lists:droplast(StrId)),
    Draws = parse_draws(Rest),
    #game{id = Id, draws=Draws}.

parse_draws(Parts) ->
    HasSemicolon = fun(X) ->
        [H|_] = lists:reverse(X),
        H =:= $;
    end,
    lists:map(fun parse_draw/1, aoc_util:group_by(HasSemicolon, Parts)).

parse_draw(DrawString) -> parse_draw(DrawString, #draw{}).

parse_draw([], Acc) -> Acc;
parse_draw([SN, Color0|T], Acc) ->
    FilterFn = fun(X) -> X >= $a andalso X =< $z end,
    Color = lists:filter(FilterFn, Color0),
    N = list_to_integer(SN),
    case Color of
        "red" -> parse_draw(T, Acc#draw{red=N});
        "blue" -> parse_draw(T, Acc#draw{blue=N});
        "green" -> parse_draw(T, Acc#draw{green=N})
    end.


valid_game(#game{draws=Draws}) ->
    lists:all(fun valid_draw/1, Draws).

valid_draw(#draw{red=R, blue=B, green=G}) ->
    R =< ?MAX_RED andalso B =< ?MAX_BLUE andalso G =< ?MAX_GREEN.

calc_power(Game) ->
    FindMax = fun(#draw{red=R1, blue=B1, green=G1},
                  #draw{red=R2, blue=B2, green=G2}) ->
        #draw{red=max(R1, R2),
              blue=max(B1, B2),
              green=max(G1, G2)
             }
    end,
    Max = lists:foldl(FindMax, #draw{}, Game#game.draws),
    Max#draw.red * Max#draw.blue * Max#draw.green.
