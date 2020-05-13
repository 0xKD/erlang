-module(funcs).
-export([is_zero/1,do_xor/2]).

is_zero(0) ->
	true;
is_zero(_) ->
	false.

% obviously this only makes sense if arguments are bools
do_xor(X,X) ->
	false;
do_xor(_,_) ->
	true.
