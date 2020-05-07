% Name of the module (can refer to this from repl and other modules)
% Must match name of the file in which it resides
-module(first).

% We are exporting functions with arity (no. of arguments) specified
-export([mult/2,double/1,area/3]).

mult(X,Y) ->
	X*Y.

double(X) ->
	mult(X,2).

% compute area of triangle
area(A,B,C) ->
	S = (A+B+C)/2,
	math:sqrt(S*(S-A)*(S-B)*(S-C)).
