-module(pal).
-export([palindrome/1,strip/1]).

% get rid of everything except letters (alphabets)
strip([]) ->
	[];
strip([X|Xs]) when X>=97 , X<123 ->
	[X|strip(Xs)];
strip([X|Xs]) when X>=65 , X<91 ->
	[X+32|strip(Xs)];
strip([_|Xs]) ->
	strip(Xs).

palindrome(S) ->
	C = strip(S),
	C == lists:reverse(C).
