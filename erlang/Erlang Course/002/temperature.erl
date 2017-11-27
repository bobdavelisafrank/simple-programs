% The following is code made for the erlang.org course, under exercises (simple sequential programs). 

-module(temperature).
-export([convert/1]).

% Converts a number scaled in fahrenheit into a number in celsius.
f2c(X) ->
	5*(X-32)/9.

% Convers a number scaled in celsius to a number in fahrenheit.
c2f(X) ->
	9*X/5 + 32.

% Unifies the above functions, and uses atoms for type safety.
convert({f, X}) ->
	f2c(X);
convert({c, X}) ->
	c2f(X).