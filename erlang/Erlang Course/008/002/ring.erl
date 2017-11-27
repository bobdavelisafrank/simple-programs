% The following is code made for the erlang.org course, under exercises (interaction between processes). 
-module(ring).
-export([name/1, sender/0, create_senders/1, counter/0, start/3]).

% A function for producing the names of elements of the ring.
name(X) ->
	list_to_atom("server" ++ integer_to_list(X)).

% An element/process of the ring, which passes on a message to the next process in the ring.
% It knows the name of the next process by using the above naming function with information
% passed alongside the message.
sender() ->
	receive
		{Msg, Name, X} ->
			io:format("~p ~p~n", [self(), Msg]),
			name(Name+1) ! {Msg, Name+1, X},
			sender();
		{stop, Name} ->
			name(Name+1) ! {stop, Name+1},
			exit(normal)
	end.

% A unique element of the ring that manages the ring, making it loop, and controlling how many
% times the message is passed through the ring.
counter() ->
	receive
		{Msg, _, 1} ->
			io:format("~p ~p~n", [self(), Msg]),
			name(1) ! {stop, 1},
			counter();
		{Msg, _, X} ->
			io:format("~p ~p~n", [self(), Msg]),
			name(1) ! {Msg, 1, X-1},
			counter();
		{stop, _} ->
			exit(normal)
	end.

% A function that creates a ring of X sender processes.
create_senders(0) -> done;
create_senders(X) ->
	register(name(X), spawn(ring, sender, [])),
	create_senders(X-1).

% Function that starts the ring.
start(Ring_Size, Msg, Ring_Loops) ->
	create_senders(Ring_Size - 1),
	register(name(Ring_Size), spawn(ring, counter, [])),
	name(1) ! {Msg, 1, Ring_Loops}.