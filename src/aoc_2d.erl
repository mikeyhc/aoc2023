-module(aoc_2d).

-export([opposite_dir/1, move_point/2, move_point/3, manhattan/2,
         map_dir_char/1, char_dir/1]).

opposite_dir(none) -> none;
opposite_dir(left) -> right;
opposite_dir(right) -> left;
opposite_dir(up) -> down;
opposite_dir(down) -> up.

move_point(Point, Dir) ->
    move_point(Point, Dir, 1).

move_point(Point, none, _Dist) -> Point;
move_point({Y, X}, left, Dist) -> {Y, X - Dist};
move_point({Y, X}, right, Dist) -> {Y, X + Dist};
move_point({Y, X}, up, Dist) -> {Y - Dist, X};
move_point({Y, X}, down, Dist) -> {Y + Dist, X}.

manhattan({Y0, X0}, {Y1, X1}) ->
    abs(X0 - X1) + abs(Y0 - Y1).

map_dir_char(none) -> $X;
map_dir_char(left) -> $<;
map_dir_char(right) -> $>;
map_dir_char(up) -> $^;
map_dir_char(down) -> $v.

char_dir($L) -> left;
char_dir($R) -> right;
char_dir($U) -> up;
char_dir($D) -> down.
