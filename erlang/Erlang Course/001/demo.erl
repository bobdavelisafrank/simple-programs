% The following is code directly from the Erlang course webpage, under the section "sequential-programming".
% The purpose of this is to test the Erlang environment.

-module(demo).
-export([double/1]).

double(X) ->
	times(X, 2).

times(X, N) ->
	X * N.