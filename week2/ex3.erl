-module(ex3).
-export([join/2,concat/1,member/2,perms/1,quicksort/1]).

% join two lists, like the ++ operator
join([], [])		-> [];
join([X|Xs], [])	-> [X|Xs];
join([], [X|Xs])	-> [X|Xs];
join([X|Xs], [Y|Ys])	-> [X|join(Xs, [Y|Ys])].


% flatten list of lists
concat([])	-> [];
concat([X])	-> X;
concat([X|Xs])	-> join(X, concat(Xs)).


% check if item is part of a list
member(_, [])		-> false; member(X, [X|_])	-> true;
member(X, [_|Xs])	-> member(X, Xs).


% permutations - recursion really is beautiful, it's like the problem solves itself :D
perms([X|[]])		-> [[X]];
perms([X|[Y|[]]])	-> [[X,Y],[Y,X]];
perms(Xs)		->
	concat([appender(I, perms(Is)) || [I|Is] <- picker(Xs, [])]).

% below are helper functions for perms

% return list of lists, where H is prepended to all items in Xs
% appender(1, [[2,3], [3,2]]) => [[1,2],[1,3]]
appender(H, Xs)		-> [[H]++X || X <- Xs].

% return variations of a list where each element is the head (once)
% picker([1,2,3], []) => [[1,2,3], [2,3,1], [3,2,1]]
picker([], _S)		-> [];
picker([X|Xs], S)	-> [[X|Xs++S]]++picker(Xs, [X]++S).


% this is not a "stable sort" - does not preserve ordering among values that are same
quicksort([])		-> [];
quicksort([X|Xs])	->
	quicksort([ E || E <- Xs, E =< X])++[X]++quicksort([E || E <- Xs, E > X]).
