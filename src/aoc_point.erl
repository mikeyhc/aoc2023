-module(aoc_point).

-export([new/1, new/2, x/1, y/1]).

new(Point) -> Point.

new(X, Y) -> {X, Y}.

x({X, _}) -> X.

y({_, Y}) -> Y.
