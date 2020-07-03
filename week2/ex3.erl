-module(ex3).
-export([join/2,concat/1,member/2,perms/1,quicksort/1,mergesort/1,insort/1]).

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




% mergesort
mergesort([])		-> [];
mergesort(Xs)		->
	L=length(Xs), MP=L div 2,
	case L of
		1	-> Xs;
		_	-> merge(mergesort(splitl(Xs, MP, [])), mergesort(splitr(Xs, MP)), [])
	end.

% for the "combine" aspect of mergesort
merge([], Ys, M)	-> M++Ys;
merge(Xs, [], M)	-> M++Xs;
merge([X|Xs], [Y|Ys], M) when X=<Y ->
	merge(Xs, [Y|Ys], M++[X]);
merge([X|Xs], [Y|Ys], M) when X>Y ->
	merge([X|Xs], Ys, M++[Y]).

% take elements from left side of list considering midpoint MP
splitl(_, 0, A)		-> A;
splitl([X|Xs], MP, A)	-> splitl(Xs, MP-1, A++[X]).

% take elements from right side of list considering midpoint MP
splitr(Xs, 0)		-> Xs;
splitr([_|Xs], MP)	-> splitr(Xs, MP-1).


% insertion sort
insort([])			-> [];
insort([X|Xs])			-> insert(X, insort(Xs), []).

% insert X in the correction position in the list ([Y|Ys])
insert(X, [], A)		-> A++[X];
insert(X, [Y|Ys], A) when X=<Y	-> A++[X,Y|Ys];
insert(X, [Y|Ys], A) when X>Y	-> insert(X, Ys, A++[Y]).
