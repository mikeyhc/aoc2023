-module(range).

-export([new/2, intersect_slice/2, add_start/2, sub_start/2, start/1,
        contains/2, size/1]).

-record(range, {start, last}).

new(Start, End) ->
    if Start > End -> #range{start=End, last=Start};
       true -> #range{start=Start, last=End}
    end.

% returns the overlap of R0 with R1, as well as a list of the non-overlapping
% parts
intersect_slice(R0=#range{start=S0, last=E0}, R1=#range{start=S1, last=E1}) ->
    if S0 >= S1 andalso E0 =< E1 -> {R0, []}; % range0 within range1
       S0 =< S1 andalso E0 >= E1 -> %range1 within range0
           {R1, [#range{start=S0, last=S1-1},
                 #range{start=E1+1, last=E0}]};
       S0 < S1 andalso E0 > S1 -> %range0 overlaps start of range1
           {#range{start=S1, last=E0}, [#range{start=S0, last=S1-1}]};
       S0 < E1 andalso E0 > E1 -> %range0 overlaps end of range1
           {#range{start=S0, last=E1}, [#range{start=E1+1, last=E0}]};
       true -> false % no overlap
    end.

% add the start of R1 to R0
add_start(R0=#range{start=S0, last=E0}, #range{start=S1}) ->
    R0#range{start=S0+S1, last=E0+S1}.

% sub the start of R1 to R0
sub_start(R0=#range{start=S0, last=E0}, #range{start=S1}) ->
    R0#range{start=S0-S1, last=E0-S1}.

start(#range{start=S}) -> S.

contains(#range{start=S, last=E}, P) ->
    P >= S andalso P =< E.

size(#range{start=S, last=E}) -> abs(E - S).
