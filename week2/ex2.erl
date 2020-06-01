-module(ex2).
-export([double/1,evens/1]).

% double items in the list
double([], Xs)		-> Xs;
double([X|Xs], R)	-> double(Xs, R++[X*2]).
double(Xs)		-> double(Xs, []).


% extract even numbers from a list
evens([], R)	-> R;
evens([X|Xs], R) when X rem 2 == 0 ->
	evens(Xs, R++[X]);
evens([X|Xs], R) when X rem 2 > 0 ->
	evens(Xs, R).
evens(Xs) ->
	evens(Xs, []).
