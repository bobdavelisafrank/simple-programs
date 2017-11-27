% The following is code copied from the erlang.org course, under the module for concurrent programming.

-module(echo).
-export([go/0, loop/0]).

go() ->
	Pid2 = spawn(echo, loop, []),
	Pid2 ! {self(), hello},
	receive
		{Pid2, Msg} ->
			io:format("P1 ~w~n", [Msg])
	end,
	Pid2 ! stop.

loop() ->
	receive
		{From, Msg} ->
			From ! {self(), Msg},
			loop();
		stop ->
			true
	end.