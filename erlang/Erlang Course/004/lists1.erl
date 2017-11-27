% The following is code made for the erlang.org course, under exercises (simple recursive programs). 

-module(lists1).
-export([max/1, min/1]).

% Tail recursive function that gives the largest value of the list, with Y as the starting comparison.
iterativemax([], Y) -> Y;
iterativemax([X|XS], Y) when X > Y ->
	iterativemax(XS, X);
iterativemax([_|XS], Y) ->
	iterativemax(XS, Y).

% Tail recursive function that gives the smallest value of the list, with Y as the starting comparison.
iterativemin([], Y) -> Y;
iterativemin([X|XS], Y) when X < Y ->
	iterativemin(XS, X);
iterativemin([_|XS], Y) ->
	iterativemin(XS, Y).

% Functions that find the largest and smallest values in a list.
% Purposefully leaves out the case of the empty list, for there is value for an empty list that can make sense.
max([X|XS]) -> iterativemax(XS, X).
min([X|XS]) -> iterativemin(XS, X).