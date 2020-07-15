-module(ex2).
-export([filler/2]).


% "Define a function that takes an input file in Erlang
% as a string (list) of characters. and a line length len
% (a positive integer) and which returns a list of lines,
% each of which is filled to include the maximum number
% of words up to the overall length."
% I've probably made this more complicated than it needs to be
filler([], _, AccText) -> AccText;
filler(Text, MaxLength, AccText) ->
    {Taken, Rest} = take_chars(trim(Text), MaxLength),
    case Taken of
      [] ->
        {Taken2, Rest2} = take_first(trim(Rest)),
        filler(Rest2, MaxLength, AccText++[Taken2]);
      _ -> filler(Rest, MaxLength, AccText++[Taken])
    end.
filler(Text, MaxLength) ->
    filler(Text, MaxLength, []).


% Take a (complete, no broken words) string of (at most) MaxLength
% characters from given string.
% The function will return both the "taken" and remainder strings.
take_chars(Text, MaxLength, Buffer, BufferLength, Retval, RetvalLength)
  when RetvalLength == MaxLength ; BufferLength+RetvalLength > MaxLength ->
    {Retval, Buffer++Text};
% Handling for the terminating cases could be improved, there is some duplication w.r.t logic
take_chars([], MaxLength, Buffer, BufferLength, Retval, RetvalLength)
  when BufferLength+RetvalLength >= MaxLength ->
    {Retval, Buffer};
take_chars([], _MaxLength, Buffer, _BufferLength, Retval, RetvalLength) ->
    % I smell some amount of duplication here (w.r.t similar clauses below)
    case RetvalLength of
      0 -> {Buffer, []};
      _ -> {Retval++[32|Buffer], []}
    end;
take_chars([NextChar|Text], MaxLength, Buffer, BufferLength, _, RetvalLength)
  when BufferLength+RetvalLength =< MaxLength, NextChar == 32, RetvalLength == 0 ->
    take_chars(Text, MaxLength, [], 0, Buffer, BufferLength);
take_chars([NextChar|Text], MaxLength, Buffer, BufferLength, Retval, RetvalLength)
  when BufferLength+RetvalLength+1 =< MaxLength, NextChar == 32 ->
    take_chars(Text, MaxLength, [], 0, Retval++[32|Buffer], RetvalLength+BufferLength+1);
take_chars([NextChar|Text], MaxLength, Buffer, BufferLength, Retval, RetvalLength)
  when BufferLength+RetvalLength =< MaxLength ->
    take_chars(Text, MaxLength, Buffer++[NextChar], BufferLength+1, Retval, RetvalLength).
take_chars(Text, 0) ->
    {[], Text};
take_chars([NextChar|Text], MaxLength) ->
    take_chars(Text, MaxLength, [NextChar], 1, [], 0).


% In cases where nothing of MaxLength can be "taken",
% this function will return part of the string until the first space
take_first([], AccText) ->
  {AccText, []};
take_first([NextChar|Text], AccText)
  when NextChar == 32 ->
  {AccText, Text};
take_first([NextChar|Text], AccText) ->
  take_first(Text, AccText++[NextChar]).
take_first(Text) ->
  take_first(Text, []).


trim(Xs) ->
  trim_spaces(replace_newlines(Xs)).


% remove extraneous spaces from the string
% (32 == space character)
trim_spaces(Text, PreviousChar, Accumulated)
  when Text == [32]; Text == [] ->
    case PreviousChar of
        [] -> Accumulated;
        32 -> Accumulated;
        _ -> Accumulated++[PreviousChar]
    end;
trim_spaces([NextChar|Text], PreviousChar, Accumulated)
    when PreviousChar == 32, NextChar == 32 ->
    trim_spaces(Text, NextChar, Accumulated);
trim_spaces([NextChar|Text], PreviousChar, Accumulated) ->
    case PreviousChar of
        [] -> trim_spaces(Text, NextChar, Accumulated);
        _ -> trim_spaces(Text, NextChar, Accumulated++[PreviousChar])
    end.
trim_spaces([]) -> [];
trim_spaces([32|Xs]) ->
    trim_spaces(Xs);
trim_spaces([X|Xs]) ->
    trim_spaces(Xs, X, []).


replace_newlines([], AccXs) -> AccXs;
replace_newlines([10|Xs], AccXs) ->
  replace_newlines(Xs, AccXs++[32]);
replace_newlines([X|Xs], AccXs) ->
  replace_newlines(Xs, AccXs++[X]).
replace_newlines(Xs) ->
  replace_newlines(Xs, []).
