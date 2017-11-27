% The following is code made for the erlang.org course, under exercises (simple sequential programs). 

-module(mathStuff).
-export([perimeter/1]).

% Perimiter of square, with side-length -> X.
psquare(X) ->
	4*X.

% Perimeter (a.k.a circumference) of circle, with radius -> R.
pcircle(R) ->
	6.28318530717958647693 * R.

% Perimeter of triangle, with side lengths A, B, and C.
ptriangle(A, B, C) ->
	A + B + C.

% Generalized perimeter function.
perimeter({square, X}) ->
	psquare(X);
perimeter({circle, R}) ->
	pcircle(R);
perimeter({triangle, A, B, C}) ->
	ptriangle(A, B, C).