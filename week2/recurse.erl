-module(recurse).
-export([fac/1,fib/1,cut/1]).

% example of direct recursion
fac(0) ->
	1;
% 'when N>0' is a guard
fac(N) when N>0 ->
	fac(N-1)*N.


% this gives a list, not the numbers themselves
fib(1) ->
	[0];
fib(2) ->
	[0,1];
fib(N) when N>2 ->
	INDEX=max(0, N-3),
	PREV=fib(N-1),
	PREV++[lists:sum(lists:nthtail(INDEX, PREV))].


% Q. how many pieces can you make with N cuts through a (rectangular?) sheet of paper
% in the N'th cut, you can go through N regions (at max)?
cut(0) ->
	1;
cut(N) when N>0 ->
% the below could be simplified to N + cut(N-1)
	PREV=cut(N-1),
	(N*2) + (PREV-N).
