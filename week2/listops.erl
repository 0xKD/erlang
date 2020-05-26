-module(listops).
-export([sum/1,tsum/1,product/1,mx/1,all_areas/1,circles/1,circles2/1]).

% direct version
sum([])		-> 0;
sum([X|Xs])	-> X + sum(Xs).


% tail-recursive
tsum([], S) 	-> S;
tsum([X|Xs], S) -> tsum(Xs, X+S).
tsum(Xs)	-> tsum(Xs, 0).


product([], S)		-> S; % not really a fan of the default value
product([X|Xs], S)	-> product(Xs, X*S).
product(Xs)		-> product(Xs, 1).

% maximum in a list
mx([], S)	-> S;
mx([X|Xs], S)	-> mx(Xs, max(X,S)).
mx([X|Xs])	-> mx(Xs, X).


% areas
all_areas([])		-> [];
all_areas([X|Xs])	-> [ex1:area(X) | all_areas(Xs)].


% filter for circles in a list
circles([])	-> [];
circles([C={circle, {_,_}, _}|Xs]) ->
	[ C | circles(Xs)];
circles([_|Xs]) ->
	circles(Xs).


% using the "case" statement (recursion seems cleaner though)
circles2([X|Xs]) ->
	case X of 
		{circle, {_,_}, _}=C ->
			[C | circles2(Xs)];
		_ ->
			circles2(Xs)
	end.
