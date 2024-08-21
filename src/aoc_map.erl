-module(aoc_map).

-export([read_map/1, read_map/2, to_map/1, set_value/4, get_value/3,
         get_value/2, has_key/2, size/1, draw_map/1, draw_map/3]).

-record(map, {rows, cols, map}).

read_map(Filename) -> read_map(Filename, []).

read_map(Filename, Opts) ->
    Trans = case lists:keyfind(transform, 1, Opts) of
                false -> fun(V) -> V end;
                {_Key, T} -> T
            end,
    Fn = case lists:member(skip_dots, Opts) of
             true ->
                 fun(RowNum) ->
                     fun({_ColNum, $.}, Acc) -> Acc;
                        ({ColNum, C}, Acc) -> Acc#{{RowNum, ColNum} => Trans(C)}
                     end
                 end;
             false ->
                 fun(RowNum) ->
                         fun({ColNum, C}, Acc) ->
                                 Acc#{{RowNum, ColNum} => Trans(C)}
                         end
                 end
         end,
    Lines = aoc_util:read_lines(Filename),
    #map{rows=length(Lines),
         cols=length(hd(Lines)),
         map=lists:foldl(fun(X, Acc) -> read_row(X, Acc, Fn) end,
                         #{}, aoc_util:with_index(Lines))
        }.

read_row({RowNum, Row}, Map, Fn) ->
    lists:foldl(Fn(RowNum), Map, aoc_util:with_index(Row)).

to_map(#map{map=Map}) -> Map.

set_value(Map=#map{map=M}, Row, Col, Val) ->
    Map#map{map=M#{{Row, Col} => Val}}.

get_value(#map{map=M}, Row, Col) ->
    maps:get({Row, Col}, M).

get_value(#map{map=M}, Pos) ->
    maps:get(Pos, M).

size(#map{rows=Rows, cols=Cols}) ->
    {Rows, Cols}.

draw_map(#map{map=Map, rows=Rows, cols=Cols}) ->
    StringMap = lists:map(fun(Row) -> build_row(Map, Row, Cols) end,
                          lists:seq(1, Rows)),
    io:format("~s~n", [string:join(StringMap, "\n")]).

build_row(Map, Row, Cols) ->
    lists:map(fun(Col) -> maps:get({Row, Col}, Map, $.) end,
              lists:seq(1, Cols)).

draw_map(#map{map=Map, rows=Rows, cols=Cols}, Path, Trans) ->
    PathMap = maps:from_list(Path),
    StringMap = lists:map(fun(Row) ->
                                  build_path_row(Map, PathMap, Trans, Row, Cols)
                          end, lists:seq(1, Rows)),
    io:format("~s~n", [string:join(StringMap, "\n")]).

build_path_row(Map, PathMap, Trans, Row, Cols) ->
    lists:map(fun(Col) ->
                      case maps:get({Row, Col}, PathMap, false) of
                          false -> Trans(maps:get({Row, Col}, Map, $.));
                          V -> aoc_2d:dir_char(V)
                      end
              end, lists:seq(1, Cols)).

has_key(#map{map=M}, Key) ->
    maps:is_key(Key, M).
