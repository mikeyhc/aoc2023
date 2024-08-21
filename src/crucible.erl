-module(crucible).

-include_lib("stdlib/include/assert.hrl").

-export([solve/1]).

-type point() :: {pos_integer(), pos_integer()}.
-type direction() :: none | left | right | up | down.
-type movement() :: {direction(), non_neg_integer()}.

-record(node_entry, {node :: point(),
                     cost :: non_neg_integer(),
                     movement :: movement(),
                     path :: [point()]}).

solve(Filename) ->
    Map = aoc_map:read_map(Filename, [{transform, fun(V) -> V - $0 end}]),
    {solve_part1(Map, 1, 3), solve_part1(Map, 4, 10)}.

solve_part1(Map, Min, Max) ->
    PQueue0 = pqueue:new(),
    Start = build_node_entry({1, 1}, 0, none, 1, []),
    PQueue1 =pqueue:push(Start, 0, PQueue0),
    Goal = aoc_map:size(Map),
    find_path(PQueue1, Goal, Map, Min, Max, #{}).

find_path(Queue0, Goal, Map, Min, Max, Visited0) ->
    Heuristic = fun(E) -> heuristic(E, Goal) end,
    Visit = fun(Entry, Queue1) ->
                    NewEntries = visit_node(Entry, Min, Max, Map),
                    ToAdd = lists:map(Heuristic, NewEntries),
                    Queue2 = pqueue:push_all(ToAdd, Queue1),
                    Visited1 = add_visited(Entry, Visited0),
                    find_path(Queue2, Goal, Map, Min, Max, Visited1)
            end,
    case pqueue:pop(Queue0) of
        false -> throw(out_of_nodes);
        {true, #node_entry{node=Goal, cost=Cost}, _Queue1} -> Cost;
        {true, Entry=#node_entry{node=Node, cost=Cost, movement=Move},
         Queue1} ->
            case maps:get(Node, Visited0, undefined) of
                undefined -> Visit(Entry, Queue1);
                Visits ->
                    case maps:get(Move, Visits, undefined) of
                        undefined -> Visit(Entry, Queue1);
                        OldCost ->
                            if Cost < OldCost -> Visit(Entry, Queue1);
                               true ->
                                   find_path(Queue1, Goal, Map, Min, Max,
                                             Visited0)
                            end
                    end
            end
    end.

visit_node(Node=#node_entry{movement=Movement}, Min, Max, Map) ->
    AllMoves = [left, right, up, down],
    ValidMoves = lists:filter(fun(D) -> valid_move(D, Movement, Min, Max) end,
                              AllMoves),
    lists:filtermap(fun(D) -> build_move(D, Node, Min, Map) end,
                    ValidMoves).

build_node_entry(Node, Cost, Direction, Count, Path) ->
    #node_entry{node=Node,
                cost=Cost,
                movement={Direction, Count},
                path=Path}.

add_visited(#node_entry{node=Node, cost=Cost, movement=Move},
            Visited) ->
    Visit = maps:get(Node, Visited, #{}),
    Visited#{Node => Visit#{Move => Cost}}.

valid_move(OldDir, {OldDir, Max}, _Min, Max) -> false;
valid_move(NewDir, {OldDir, Count}, Min, Max) ->
    ?assert(Count < Max + 1),
    if OldDir =:= none -> true;
       Count < Min -> NewDir =:= OldDir;
       true ->
           Opposite = aoc_2d:opposite_dir(OldDir),
           Opposite =/= NewDir
    end.

build_move(NewDir,
           #node_entry{node=Node, cost=Cost, path=Path, movement={Dir, Count}},
           Min,
           Map) ->
    Goal = aoc_map:size(Map),
    NextNode = aoc_2d:move_point(Node, NewDir),
    NewCount = if Dir =:= NewDir -> Count + 1;
                  true -> 1
               end,
    case aoc_map:has_key(Map, NextNode) of
        false -> false;
        true ->
            if NextNode =:= Goal andalso NewCount < Min -> false;
               true ->
                   NewCost = Cost + aoc_map:get_value(Map, NextNode),
                   Entry = build_node_entry(NextNode, NewCost, NewDir, NewCount,
                                            [{Node, Dir}|Path]),
                   {true, {Entry, NewCost}}
            end
    end.

heuristic({E=#node_entry{node=Node}, Cost}, Goal) ->
    {E, Cost + aoc_2d:manhattan(Goal, Node)}.
