-module(funcs).
-export([is_zero/1,do_xor/2,maxThree/3,howManyEqual/3]).

is_zero(0) ->
	true;
is_zero(_) ->
	false.

% obviously this only makes sense if arguments are bools
do_xor(X,X) ->
	false;
do_xor(_,_) ->
	true.

maxThree(A,B,C) ->
	max(max(A,B),C).

equal(X,X) ->
	1;
equal(_,_) ->
	0.

howManyEqual(A,B,C) ->
	equal(A,B) + equal(B,C) + equal(A,C).
