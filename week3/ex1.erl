-module(ex1).
-export([build_index/1]).

build_index([], Map, _) -> Map;
build_index([Words|RemainingLines], Map, LineNumber) ->
    build_index(RemainingLines, update_map(Words, Map, LineNumber), LineNumber+1).
build_index(Lines) -> build_index(Lines, #{}, 1).


update_map([], Map, _) -> Map;
update_map([Word|Remaining], Map, LineNumber) ->
    case maps:get(Word, Map) of
        {badkey, _} ->
            Value=[{LineNumber, LineNumber}],
            update_map(Remaining, maps:put(Word, Value, Map), LineNumber);
        Value ->
            NewValue=update_lines(Value, LineNumber, []),
            update_map(Remaining, maps:put(Word, NewValue, Map), LineNumber)
    end.

% when we've exhausted all ranges
update_lines([], LineNumber, Accumulated) ->
    Accumulated ++ [{LineNumber, LineNumber}];
% word is already in or near an existing range
update_lines([{Start, End}|Tail], LineNumber, Accumulated)
    when Start =< LineNumber, LineNumber =< End+1 ->
    Accumulated ++ [{Start, max(End, LineNumber)}|Tail];
% word is on some other line
update_lines([{Start, End}|Tail], LineNumber, Accumulated) ->
    update_lines(Tail, LineNumber, Accumulated++[{Start, End}]).

% TODO: split into list of words
