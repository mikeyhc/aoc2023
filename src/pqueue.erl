-module(pqueue).

-export([new/0, push/3, push_all/2, pop/1]).

new() -> #{}.

push(Elem, Prio, Queue) ->
    maps:update_with(Prio, fun(V) -> [Elem|V] end, [Elem], Queue).

push_all(Elems, Queue) ->
    lists:foldl(fun({E, P}, Q) -> push(E, P, Q) end, Queue, Elems).

pop(Queue) ->
    Sorted = lists:sort(fun({A, _}, {B, _}) -> A < B end,
                        maps:to_list(Queue)),
    case Sorted of
        [] -> false;
        [{Prio, [First|Rest]}|_] ->
            case Rest of
                [] -> {true, First, maps:remove(Prio, Queue)};
                _ -> {true, First, Queue#{Prio => Rest}}
            end
    end.
