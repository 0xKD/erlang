-module(strats).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,cycle/1,rand/1,better/1,equalizer/1,maximizer/1]).


% Define a function that takes three arguments: two strategies and a number N,
% and which plays the strategies against each other for N turns.
% At each stage the function should output the result of the round,
% and it should show the result of the tournament at the end.
% You could also choose to modify this so that the game is ended when one player
% is more than M points ahead of the other, for example.
play_two(_,_,PlaysL,PlaysR,0) ->
    % could also accumulate the result instead of computing at the end
    display_tournament_result(PlaysL, PlaysR);
play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
    PlayL=StrategyL(PlaysR),
    PlayR=StrategyR(PlaysL),
    Result=rps:result(PlayL, PlayR),
    display_result(Result),
    play_two(StrategyL, StrategyR, [PlayL|PlaysL], [PlayR|PlaysR], N-1).
% play one strategy against another, for N moves.
play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

display_result(0) ->
    io:format("Result: draw!~n");
display_result(1) ->
    io:format("Result: PlayerL wins!~n");
display_result(-1) ->
    io:format("Result: PlayerR wins!~n").

display_tournament_result(PlaysL, PlaysR) ->
    io:format("Tournament Result: ~p~n", [rps:tournament(PlaysL,PlaysR)]).


% interactively play against a strategy, provided as argument.
play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1
play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
        stop ->
            io:format("Stopped~n");
        _    ->
            Result = rps:result(Play,Strategy(Moves)),
            io:format("Result: ~p~n",[Result]),
            play(Strategy,[Play|Moves])
    end.


% auxiliary functions
expand(r) -> rock;
expand(p) -> paper;
expand(s) -> scissors;
expand(X) -> X.

options() -> [rock,paper,scissors].

% transform 0, 1, 2 to rock, paper, scissors and vice versa.
enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

% strategies (default)
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.

% strategies to implement (1-4)

% (1) "assume that your opponent never repeats herself:
% if you know this you can make a choice that will never lose;"
% .i.e if the last move was rock, the next move will be one of
% (scissors,paper) and we must play to win/draw against them.
no_repeat([]) ->
    rand([]); % since it doesn't matter
no_repeat([Last|_]) ->
    better(rest(Last)).

% return all options other than Choice
rest(Choice) ->
    lists:filter(fun(X) -> X =/= Choice end, options()).

% pick a move such that it beats or draws against both X and Y
% The way the game works, the third choice (apart from X,Y)
% will always lose against X or Y, so we must pick one of (X,Y) itself
better([X,Y]) ->
    case rps:beat(X) of
        Y -> Y;
        _ -> X
    end.

% (2) "make a random choice each time; you may want to use the
% random:uniform/1 function so that random:uniform(N)
% returns a random choice of 1,2, … N with equal probability each time"
% note: apparently random is deprecated, use rand instead
rand(_) ->
    enum(rand:uniform(3)-1).

% cycles through the three choices in some order;
cycle(Plays) ->
    % will cycle through (rock,paper,scissors,...) repeatedly
    enum(length(Plays) rem 3).

% (3) "apply an analysis to the previous plays and choose the least frequent,
% assuming that in the long run your opponent will play each choice equally"
% The wording on this is a bit confusing ("choose the least frequent.."),
% so I am going to focus on the latter part of the description.
% This means the next move by the opponent is such that it will tend
% to equalise the count of all moves so far. This leads to a few cases
% but is fairly straightforward to think about
equalizer([]) ->
    rand([]); % best strat
equalizer(Moves) ->
    equalize(counter(Moves)).

% build a map indicating count of all moves
% #{rock=>1, paper=>2, scissors=>0}.
counter([], Map) -> Map;
counter([Move|Moves], Acc) ->
    counter(Moves, maps:update_with(Move, fun(X) -> X+1 end, 1, Acc)).
counter(Moves) -> counter(Moves, #{}).

% given a map of count of moves, return a move that will beat
% the move that is *not* the most played one
equalize(MovesCounter) ->
    % using the solution from no_repeat, since the next move will
    % be something other than the most played move
    {MostPlayed,_}=find_most_played(MovesCounter),
    better(rest(MostPlayed)).

% return most played move and associated count
find_most_played(Move, Count, {MostPlayed,MaxCount}) ->
    case Count>MaxCount of
        true -> {Move,Count};
        _ -> {MostPlayed, MaxCount}
    end.

find_most_played(MovesCounter) ->
    maps:fold(fun find_most_played/3, {empty, -1}, MovesCounter).


% (4) "apply an analysis to the previous plays and choose the most frequent,
% assuming that in the long run your opponent is going to play that
% choice more often than the others."
maximizer([]) ->
    rand([]);
maximizer(Moves) ->
    maximize(counter(Moves)).

% given a map of count of moves,
% return move that beats the most played one
maximize(MovesCounter) ->
    {MostPlayed,_}=find_most_played(MovesCounter),
    rps:beat(MostPlayed).
