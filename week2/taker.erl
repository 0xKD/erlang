-module(taker).
-export([take/2, nub/1]).

-spec take(integer(), [T]) -> [T].


% take first L elements from a list
% can also use lists:split
take(L, _, R) when L =< 0 ->
	R;
take(_, [], R) ->
	R;
take(L, [X|Xs], R) ->
	take(L-1, Xs, R++[X]).
take(L, Xs) -> take(L, Xs, []).


% pick distinct elements from a list
nub([], _, R) ->
	R;
nub([X|Xs], S, R) ->
	case sets:is_element(X, S) of
		true	-> nub(Xs, S, R);
		_	-> nub(Xs, sets:add_element(X, S), R++[X])
	end.
nub(Xs) -> nub(Xs, sets:new(), []).
