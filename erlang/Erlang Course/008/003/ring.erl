% The following is code made for the erlang.org course, under exercises (interaction between processes). 
-module(ring).
-export([sender/1, counter/1, start/3]).

sender({int, 0}) ->
	Pid = counter,
	sender({pid, Pid});
sender({int, N}) ->
	Pid = spawn(ring, sender({int, N-1}), []),
	sender({pid, Pid});
sender({pid, Pid}) ->
	receive
		{Msg, X} ->
			io:format("~p ~p~n", [self(), Msg]),
			Pid ! {Msg, X},
			sender({pid, Pid});
		stop ->
			Pid ! stop,
			exit(normal)
	end.

counter({int, Ring_Size}) ->
	Pid = spawn(ring, sender, [{int, Ring_Size - 1}]),
	counter({pid, Pid});
counter({pid, Pid}) ->
	receive
		{Msg, 0} ->
			io:format("~p ~p~n", [self(), Msg]),
			Pid ! stop,
			counter({pid, Pid});
		{Msg, X} ->
			io:format("~p ~p~n", [self(), Msg]),
			Pid ! {Msg, X-1},
			counter({pid, Pid});
		stop ->
			exit(normal)
	end.

start(Msg, Ring_Size, Ring_Loops) ->
	register(counter, spawn(ring, counter, [{int, Ring_Size}])),
	counter ! {Msg, Ring_Loops}.