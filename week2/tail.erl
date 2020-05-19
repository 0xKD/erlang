-module(tail).
-export([fac/1,fib/1]).


fac(0, R) ->
	R;
fac(N, R) when N>0 ->
	fac(N-1, R*N).
fac(N) ->
	fac(N, 1).


fib(P1, P2, 0) ->
	P1 + P2;
fib(P1, P2, N) ->
	fib(P2, P1+P2, N-1).
fib(1) ->
	0;
fib(N) when N>1 ->
	fib(0, 1, N-2).
