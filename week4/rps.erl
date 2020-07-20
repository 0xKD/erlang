-module(rps).
-export([tournament/2]).


tournament(LeftMoves, RightMoves) ->
    R=lists:zipwith(fun result/2, LeftMoves, RightMoves),
    lists:sum(R).


% return 1 if X beats Y, -1 if Y beats X, 0 if its a draw
result(X,X) -> 0;
result(X,Y) ->
    case beat(X) of
        Y -> -1;
        _ -> 1
    end.


% returns the move that will defeat the input move
beat(rock) ->
    paper;
beat(paper) ->
    scissors;
beat(scissors) ->
    rock.
