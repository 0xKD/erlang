-module(ex2).
-export([compose/1,twice/1,iterate/1]).

% Define a function that takes a list of functions and composes them together.
compose([Func|Funcs]) ->
    fun (X) ->
        % composition is not commutative, so using foldl v/s. foldr matters
        lists:foldl(fun (F, Prev) -> F(Prev) end, Func(X), Funcs) end.

% Using compose or otherwise, define a function twice that
% applies a function to an argument twice.
% For example, applying “multiply by 3” twice to the argument 2
% gives 18 (as applying it once gives 6 and then applying it again gives 18).
twice(F) ->
    fun(X) -> F(F(X)) end.

% Define a function iterate that takes a number N and returns
% a function that takes a function and returns that function iterated N times.
% When N is zero, it should return the identity function
% (that is, the function that returns its argument unchanged).
iterate(0) -> fun (F) -> F end;
iterate(N) ->
    fun (F) -> compose(repeat(F, N)) end.

repeat(_, 0) -> [];
repeat(F, N) -> [F|repeat(F, N-1)].
