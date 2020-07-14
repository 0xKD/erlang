-module(ex2).
-export([filler/2]).


% Define a function that takes an input file in Erlang
% as a string (list) of characters. and a line length len
% (a positive integer) and which returns a list of lines,
% each of which is filled to include the maximum number
% of words up to the overall length.
% ----
% I've probably made this more complicated than it needs to be
filler([], _) -> [];
filler(Text, MaxLength) ->
    {Taken, Rest} = take_chars(trim(Text), MaxLength),
    [Taken|filler(Rest, MaxLength)].

% TODO: handle unending case where len(Rest) > MaxLength

% Take a (complete, no broken words) string of (at most) MaxLength
% characters from given string.
% The function will return both the "taken" and remainder strings.
% We do this by building a buffer, and committing buffer to final (return)
% value when a space is encountered and buffer length + return value length <= MaxLength
take_chars(Text, MaxLength, Buffer, BufferLength, Retval, RetvalLength)
  when RetvalLength == MaxLength ; BufferLength+RetvalLength > MaxLength ->
    {Retval, Buffer++Text};
% Handling for the terminating cases could be improved, there is some duplication w.r.t logic
take_chars([], MaxLength, Buffer, BufferLength, Retval, RetvalLength)
  when BufferLength+RetvalLength >= MaxLength ->
    {Retval, Buffer};
take_chars([], _MaxLength, Buffer, _BufferLength, Retval, _RetvalLength) ->
    % this section seems like it could be handled in one of the clauses below,
    % in the one which performs (Retval++[32|Buffer])
    {Retval++[32|Buffer], []};
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

% remove extraneous spaces from the string
% (32 == space character)
trim(Text, PreviousChar, Accumulated)
  when Text == [32]; Text == [] ->
    case PreviousChar of
        [] -> Accumulated;
        32 -> Accumulated;
        _ -> Accumulated++[PreviousChar]
    end;
trim([NextChar|Text], PreviousChar, Accumulated)
    when PreviousChar == 32, NextChar == 32 ->
    trim(Text, NextChar, Accumulated);
trim([NextChar|Text], PreviousChar, Accumulated) ->
    case PreviousChar of
        [] -> trim(Text, NextChar, Accumulated);
        _ -> trim(Text, NextChar, Accumulated++[PreviousChar])
    end.

trim([]) -> [];
trim([32|Xs]) ->
    trim(Xs);
trim([X|Xs]) ->
    trim(Xs, X, []).
