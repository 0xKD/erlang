-module(second).
-export([combined/5]).

% compute area of triangle and rectangel and return sum
combined(A,B,C,L,W)->
	T = first:area(A,B,C),
	R = first:mult(L,W),
	T+R.
