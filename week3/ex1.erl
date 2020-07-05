-module(ex1).
-export([build_index/1]).

build_index([], Map, _) -> Map;
build_index([Line|RemainingLines], Map, LineNumber) ->
    build_index(RemainingLines, update_map(split_text(Line), Map, LineNumber), LineNumber+1).
build_index(Lines) -> build_index(Lines, #{}, 1).

% convert sentence to list of words
split_text([], Accumulated, _) -> Accumulated;
split_text([Char|Chars], Accumulated, NextWord) ->
    case is_terminating_char(Char) of
        true -> split_text(Chars, Accumulated++[NextWord], []);
        _ -> split_text(Chars, Accumulated, NextWord++[Char])
    end.
split_text(Text) -> split_text(Text, [], []).

% This is a simplistic implementation,
% e.g. it doesn't deal with things like "doesn't" correctly
is_terminating_char(C) when C >= 65, C<65+26 -> false;
is_terminating_char(C) when C >= 97, C<97+26 -> false;
is_terminating_char(C) when C >= 48, C<48+10 -> false;
is_terminating_char(_) -> true.

update_map([], Map, _) -> Map;
update_map([Word|Remaining], Map, LineNumber) ->
    Value = maps:get(Word, Map, [{LineNumber, LineNumber}]),
    NewValue=update_lines(Value, LineNumber, []),
    update_map(Remaining, maps:put(Word, NewValue, Map), LineNumber).

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
