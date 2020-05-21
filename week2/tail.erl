-module(tail).
-export([fac/1,fib/1,perfect/1]).


% factorial of a number (N) => N * N-1 * N-2 ... *1
fac(0, R) ->
	R;
fac(N, R) when N>0 ->
	fac(N-1, R*N).
fac(N) ->
	fac(N, 1).


% The Fibonacci sequence is given by 0, 1, 1, 2, 3, 5, …
% where subsequent values are given by adding the two previous values in the sequence.
fib(P1, P2, 0) ->
	P1 + P2;
fib(P1, P2, N) ->
	fib(P2, P1+P2, N-1).
fib(1) ->
	0;
fib(N) when N>1 ->
	fib(0, 1, N-2).


% A positive integer is perfect when it is the sum of its divisors, e.g. 6=1+2+3, 28=1+2+4+7+14.
perfect(N, A, 0) ->
	N == A;
perfect(N, A, C) when (N rem C == 0) ->
	perfect(N, A+C, C-1);
perfect(N, A, C) ->
	perfect(N, A, C-1).

perfect(N) ->
	perfect(N, 0, round(N / 2)).
