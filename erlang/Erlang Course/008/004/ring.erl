% The following is code made for the erlang.org course, under exercises (interaction between processes). 
-module(ring).
-export([sender/1, counter/1, start/3]).

% This is a Master/Slave type ring, where there is a commanding process that determines how many times
% to send a message across a loop of processes.

% A sender is an element of the ring which just passes a message to the next process in the ring.
% The senders are recursively generated, and only hold the PID of the next process.
sender({int, 1}) ->
	% The last generated sender is a standard sender, but with the PID of the master element.
	Pid = counter,
	sender({loop, Pid});
sender({int, N}) ->
	% Any middle generated sender generates the next sender in order to know its PID.
	Pid = spawn(ring, sender, [{int, N-1}]),
	sender({loop, Pid});
sender({loop, Pid}) ->
	% The standard sender loop.
	receive
		% When receiving a number-signed message, it passes it on.
		{Msg, X} ->
			io:format("~p ~p~n", [self(), Msg]),
			Pid ! {Msg, X},
			sender({loop, Pid});
		% When it receives a plain "stop" message, it passes it on, and stops.
		stop ->
			Pid ! stop,
			exit(normal)
	end.

% The counter is the master/controller of the ring, which counts the number of times a message has been
% around the ring, and stops the ring when the number of loops is sufficient.
counter({int, 1}) ->
	% Error case scenario, for when a ring of size 1 is asked for.
	Pid = counter
	counter({loop, Pid});
counter({int, Ring_Size}) ->
	% When given an integer, it spawns a ring of size equal to that integer (the size of the ring 
	% including the counter itself).
	Pid = spawn(ring, sender, [{int, Ring_Size - 1}]),
	counter({loop, Pid});
counter({loop, Pid}) ->
	% After spawning the ring, it loops itself.
	receive
		% When it receives a message signed with a 0, it sends the stop command and awaits for 
		% it to come back.
		{_, 0} ->
			Pid ! stop,
			counter({loop, Pid});
		% When it receives a message signed with any other number, it lowers that number by one 
		% and sends the message back into the loop.
		{Msg, X} ->
			io:format("~p ~p~n", [self(), Msg]),
			Pid ! {Msg, X-1},
			counter({loop, Pid});
		% When the stop command makes its way back across the ring, the master ring exits.
		stop ->
			exit(normal)
	end.

% This starts the ring properly.
start(Msg, Ring_Size, Ring_Loops) ->
	% It registers and spawns the counter process (in order for the last ring to find it).
	register(counter, spawn(ring, counter, [{int, Ring_Size}])),
	% It then sends to the counter process a message signed with the number of loops for the message
	% to pass through it.
	counter ! {Msg, Ring_Loops}.