-module(ex1).
-export([area/1,perimeter/1,enclose/1,bits/1]).

area({circle, {_X,_Y}, R}) ->
	math:pi()*R*R;
area({rectangle, {_X,_Y}, H, W}) ->
	H*W;
% using heron's formula
area({triangle, {_X1,_Y1, _X2, _Y2, _X3, _Y3}, A, B, C}) ->
	P=(A+B+C)/2,
	math:sqrt(P*(P-A)*(P-B)*(P-C)).


perimeter({circle, {_X,_Y}, R}) ->
	math:pi()*2*R;
perimeter({rectangle, {_X,_Y}, H, W}) ->
	2*(H+W);
perimeter({triangle, {_X1,_Y1, _X2, _Y2, _X3, _Y3}, A, B, C}) ->
	A+B+C.


enclose({circle, {X,Y}, R}) ->
	% a circle can be enclosed in a square
	% whose side is equal to the diameter of the circle
	{rectangle, {X-R,Y-R}, 2*R, 2*R};
enclose(R={rectangle, {_X,_Y}, _H, _W}) ->
	R. % rectangle encloses itself


bits(N) when N>0 ->
	% integer_to_list(4, 2) => "100"
	bits(integer_to_list(N, 2), 0).
bits([P|Q], A) when P==48 ->
	% [P|Q] = "0100" // P=48 Q="100"
	bits(Q, A);
bits([P|Q], A) when P==49 ->
	% [P|Q] = "100" // P=49 Q="00"
	bits(Q, A+1);
bits([], A) ->
	% once there are no more items to process
	% return the accumulated value
	A.
