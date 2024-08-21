-module(aoc_2d).

-export([opposite_dir/1, move_point/2, manhattan/2, dir_char/1]).


opposite_dir(none) -> none;
opposite_dir(left) -> right;
opposite_dir(right) -> left;
opposite_dir(up) -> down;
opposite_dir(down) -> up.

move_point(Point, none) -> Point;
move_point({Y, X}, left) -> {Y, X - 1};
move_point({Y, X}, right) -> {Y, X + 1};
move_point({Y, X}, up) -> {Y - 1, X};
move_point({Y, X}, down) -> {Y + 1, X}.

manhattan({Y0, X0}, {Y1, X1}) ->
    abs(X0 - X1) + abs(Y0 - Y1).

dir_char(none) -> $X;
dir_char(left) -> $<;
dir_char(right) -> $>;
dir_char(up) -> $^;
dir_char(down) -> $v.
