-module(aplenty).

-compile(export_all).

-export([solve/1]).

-define(MAX_RANGE, 4000).

-type dest() :: accept | reject | string().

-record(step, {key :: x | m | a | s,
               test :: fun((integer()) -> boolean()),
               instr :: {$> | $<, integer(), dest()} | {route, string()} |
                        accept | reject,
               dest :: string() | accept | reject
               }).

-record(workflow, {id :: string(),
                   steps :: [#step{}],
                   dependencies :: [string()]
                  }).

solve(Filename) ->
    Lines = aoc_util:read_lines(Filename),
    {Workflows, Parts} = parse_lines(Lines),
    {solve_part1(Workflows, Parts), solve_part2(Workflows)}.

solve_part1(Workflows, Parts) ->
    Filter = fun(Part) -> run_workflows(Part, Workflows) =:= accept end,
    Accepted = lists:filter(Filter, Parts),
    lists:foldl(fun(P, A) -> lists:sum(maps:values(P)) + A end, 0, Accepted).

solve_part2(Workflows) ->
    Start = maps:get("in", Workflows),
    count_accepted(Start#workflow.steps, Workflows, base_ranges(), 0).

run_workflows(Part, Workflows=#{"in" := Start}) ->
    run_workflows(Part, Start#workflow.steps, Workflows).

run_workflows(Part, [#step{key=Key, test=Test, dest=Dest}|Steps], Workflows) ->
    case Test(maps:get(Key, Part)) of
        true -> route(Part, Dest, Workflows);
        false -> run_workflows(Part, Steps, Workflows)
    end.

route(_Part, accept, _Workflows) -> accept;
route(_Part, reject, _Workflows) -> reject;
route(Part, NextId, Workflows) ->
    Next = maps:get(NextId, Workflows),
    run_workflows(Part, Next#workflow.steps, Workflows).

base_ranges() ->
    #{x => range:new(1, ?MAX_RANGE),
      m => range:new(1, ?MAX_RANGE),
      a => range:new(1, ?MAX_RANGE),
      s => range:new(1, ?MAX_RANGE)}.

count_accepted([Step|Steps], Workflows, Ranges, Count0) ->
    case Step#step.instr of
        reject -> Count0;
        accept -> range_product(Ranges) + Count0;
        {route, DestId} ->
            Dest = maps:get(DestId, Workflows),
            count_accepted(Dest#workflow.steps, Workflows, Ranges, Count0);
        {Op, Key, Val, DestId} ->
            {Ranges0, Ranges1} = split_ranges(Op, Key, Val, Ranges),
            Count1 = case DestId of
                         reject -> Count0;
                         accept -> range_product(Ranges0) + Count0;
                         _ ->
                             Dest = maps:get(DestId, Workflows),
                             count_accepted(Dest#workflow.steps, Workflows,
                                            Ranges0, Count0)
                     end,
            count_accepted(Steps, Workflows, Ranges1, Count1)
    end.

range_product(#{x := X, m := M, a := A, s := S}) ->
    range:size_inc(X) * range:size_inc(M) * range:size_inc(A) *
    range:size_inc(S).

split_ranges(Op, Key, Val, Ranges) ->
    Range = maps:get(Key, Ranges),
    {R0, R1} = case Op of
                   $< -> range:split(Range, Val);
                   $> ->
                       {A, B} = range:split(Range, Val + 1),
                       {B, A}
               end,
    {Ranges#{Key => R0}, Ranges#{Key => R1}}.

%% parsing code

parse_lines(Lines) ->
    IsBlank = fun(L) -> L =/= [] end,
    {WorkflowLines, [[]|PartLines]} = lists:splitwith(IsBlank, Lines),
    {parse_workflows(WorkflowLines), parse_parts(PartLines)}.

parse_workflows(Lines) ->
    maps:from_list(lists:map(fun parse_workflow/1, Lines)).

parse_workflow(Line) ->
    [Id, Rest0] = string:split(Line, "{"),
    Rest1 = lists:droplast(Rest0),
    StringSteps = string:split(Rest1, ",", all),
    Steps = lists:map(fun parse_step/1, StringSteps),
    Dependencies = lists:filtermap(fun build_dependency/1, Steps),
    {Id, #workflow{id=Id, steps=Steps, dependencies=Dependencies}}.

is_true(_) -> true.

parse_step("A") -> #step{key=x, test=fun is_true/1, instr=accept, dest=accept};
parse_step("R") -> #step{key=x, test=fun is_true/1, instr=reject, dest=reject};
parse_step(Step) ->
    case lists:member($:, Step) of
        false ->
            #step{key=x, test=fun is_true/1, instr={route, Step}, dest=Step};
        true ->
            [KeyChar, OpChar|Rest] = Step,
            [ValStr, DestStr] = string:split(Rest, ":"),
            Key = parse_key(KeyChar),
            Val = list_to_integer(ValStr),
            Dest = parse_dest(DestStr),
            #step{key=Key,
                  test=build_test(OpChar, Val),
                  instr={OpChar, Key, Val, Dest},
                  dest=parse_dest(Dest)}
    end.

build_dependency(#step{dest=accept}) -> false;
build_dependency(#step{dest=reject}) -> false;
build_dependency(#step{dest=Dest}) -> {true, Dest}.

parse_key($x) -> x;
parse_key($m) -> m;
parse_key($a) -> a;
parse_key($s) -> s.

build_test(OptStr, Val) ->
    case OptStr of
        $< -> fun(X) -> X < Val end;
        $> -> fun(X) -> X > Val end
    end.

parse_dest("A") -> accept;
parse_dest("R") -> reject;
parse_dest(Dest) -> Dest.

parse_parts(Parts) ->
    lists:map(fun parse_part/1, Parts).

parse_part([${|Rest0]) ->
    Rest1 = lists:droplast(Rest0),
    Parts0 = string:split(Rest1, ",", all),
    Parts1 = lists:map(fun(S) ->
                               [A, B] = string:split(S, "="),
                               {A, B}
                       end, Parts0),
    maps:from_list(lists:map(fun({[K], V}) ->
                                     {parse_key(K), list_to_integer(V)}
                             end, Parts1)).
