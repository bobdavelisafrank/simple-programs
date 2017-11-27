% The following is code made for the erlang.org course, under exercises (interaction between processes). 
-module(communication).
-export([sendloop/1, loop/0]).

% A reply loop that, when recieving a message with a counter, sends it back with it decremented by one. When
% the counter reaches 0 on the message, the loop stops itself and its 'buddy' (which also runs the loop).
loop() ->
	receive
		{From, _, 0} ->
			From ! stop,
			true;
		{From, Msg, X} ->
			io:format("~p ~p~n", [self(), Msg]),
			From ! {self(), Msg, X-1},
			loop();
		stop ->
			true
	end.

% A function that sends a message between two processes N times.
sendloop({Msg, N}) ->
	Pid1 = spawn(communication, loop, []),
	Pid2 = spawn(communication, loop, []),
	Pid1 ! {Pid2, Msg, N}.