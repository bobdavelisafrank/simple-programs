% The following is code made for the erlang.org course, under exercises (interaction between processes).
-module(star).
-export([point/0, start/3]).

% Function that generates names for processes for recursive reference.
name(X) ->
	list_to_atom("server" ++ integer_to_list(X)).

% Function that recursively generates 'X' processes with procedural names.
generate_points(1) ->
	register(name(1), spawn(?MODULE, point, []));
generate_points(X) ->
	register(name(X), spawn(?MODULE, point, [])),
	generate_points(X-1).

% Function that recursively sends a message to the first 'X' named processes.
send_to_points(Msg, 1) ->
	name(1) ! Msg;
send_to_points(Msg, X) ->
	name(X) ! Msg,
	send_to_points(Msg, X-1).

% The function for what each named process does when receiving messages.
point() ->
	receive
		% When receiving a normal message, the process prints it.
		{message, Msg} ->
			io:format("~p ~p~n", [self(), Msg]),
			point();
		% When receiving the 'stop' command, the process terminates.
		{command, stop} ->
			exit(normal)
	end.

% A process that recursively sends a single message to all of the points several times.
spam(_, _, 0) ->
	done;
spam(Msg, Number_of_Points, Times_To_Send) ->
	send_to_points({message, Msg}, Number_of_Points),
	spam(Msg, Number_of_Points, (Times_To_Send-1)).

% Initializes the star-structured process tree and executes the 'spam' function.
start(Msg, Number_of_Points, Times_to_Send) ->
	generate_points(Number_of_Points),
	spam(Msg, Number_of_Points, Times_to_Send),
	send_to_points({command, stop}, Number_of_Points).