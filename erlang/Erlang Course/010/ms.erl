% The following is code made for the erlang.org course, under exercises (master and slaves, error handling: www.erlang.org/course/exercises).
-module(ms).
-export([slave/0, master/1, start/1, finish/0, to_slave/2, find_slave/2]).




% Functions to start the master with X slaves and to close the master when finished.
start(X) ->
	% Logging information to verify an attempt at starting the master.
	io:format("Starting master...~n", []),

	% Registers and spawns the master.
	register(master, spawn(?MODULE, master, [{init, X}])).

finish() ->
	% Sends a stop signal to the master.
	master ! stop.




% The master function, which tries to maintain X slaves.
master({init, X}) ->
	% Sets error trapping so the Master doesn't terminate if a slave terminates.
	process_flag(trap_exit, true),

	% Generates the slaves, links the master to the slaves, and stores the PIDs of the slaves into a list (referred to as the Registry).
	Registry = start_slaves(X),

	% Logging information.
	io:format("Master (PID=~p) started with ~p slaves.~n", [self(),X]),

	% Starts a receive loop, where the master passes messages to slaves and keeps them running.
	master({loop, X}, Registry).

% The master's receive loop.
master({loop, X}, Registry) ->
	% Logging information, verifying that this function was called.
	io:format("Looping master.~n", []),

	receive
		% When receiving a message designated for slave X, the master finds that slave in the register and passes on the message.
		{to_slave, Msg, Z} ->
			% Logging information to verify that the master attempted to pass the message.
			io:format("Sending message to slave ~p...~n", [Z]),

			% 'find_slave' is used to get the Pid of slave X in the registry.
			find_slave(Z, Registry) ! Msg,

			% Returns to the receive loop.
			master({loop, X}, Registry);

		% When a slave terminates, it is replaced, and the registry is updated.
		{'EXIT', Pid, _} ->
			% Logging information to verify that the master tried restarting the slave.
			io:format("Restarting slave...~n", []),

			% 'restart_slave' is used to return a new registry with the terminated slave restarted.
			New_Registry = restart_slave(Pid, Registry),

			% Returns to the receive loop.
			master({loop, X}, New_Registry);

		% Kills the slaves and exits.
		stop ->
			% Logging information to very that the master attempted to kill the slaves.
			io:format("Killing slaves...~n", []),

			% 'kill_slaves' is used to kill all of the slaves in the registry.
			kill_slaves(Registry),

			% Logging information to verify that the master has succeeded and is exiting.
			io:format("Master exiting.~n", []),

			% Sole endpoint of the receive loop, exiting the Master process.
			exit(normal)
	end.




% The slave function process, entirely a single receive loop.
slave() ->
	% Logging information to verify that this function was called.
	io:format("Looping slave (PID=~p).~n", [self()]),

	receive
		% When receiving a 'kill' message, the slave exits.
		kill ->
			% Logging information to verify that the slave received the kill message.
			io:format("Slave, PID=~p: Exiting.~n", [self()]),

			% Sole endpoint of the Slave looping function.
			exit(killed);
		% When receiving any other message, the slave pritns it.
		Msg ->
			% Signs and prints the message.
			io:format("Slave, PID=~p: ~p~n", [self(), Msg]),

			% Returns to the receive loop.
			slave()
	end.




% A function that starts 'X' slaves and returns the list (registry) of the slaves. Note: Requires at least 2 slaves.
start_slaves(X) ->
	% Starts building the registry.
	
	% Spawns the first slave of the directory.
	Pid = spawn(?MODULE, slave, []),

	% Links the slave to whichever process started this function.
	link(Pid),

	% Logging information to verify that the first slave has been initialized.
	io:format("Started slave ~p.~n", [X]),

	% Starts the primary loop of this function to build the middle part of the registry.
	start_slaves(X-1, [{X, Pid}]).

start_slaves(1, Registry) ->
	% Builds the last element of the registry. 

	% Spawns the last slave of the directory.
	Pid = spawn(?MODULE, slave, []),

	% Links the slave to whichever process started this function.
	link(Pid),

	% Logging information to verify that the last slave has been initialized.
	io:format("Started slave ~p.~n", [1]),

	% Returns the finished registry.
	[{1, Pid}|Registry];

start_slaves(X, Registry) ->
	% Builds the middle of the registry.

	% Spawns the 'X'th slave of the registry.
	Pid = spawn(?MODULE, slave, []),

	% Links the slave to whichever process started this function.
	link(Pid),

	% Logging information to verify that the 'X'th slave has been initialized.
	io:format("Started slave ~p.~n", [X]),

	% Continues building the registry.
	start_slaves(X-1, [{X, Pid}|Registry]).




% A function that restarts a slave and returns an updated list of slaves.
restart_slave(Pid, [{X, Pid}|Registry]) ->
	% Case for when the slave is the head of the list.

	io:format("Found slave ~p.~n", [X]),

	% It unlinks the slave.
	unlink(Pid),

	% A new slave is started and linked.
	New_Pid = spawn(?MODULE, slave, []),
	link(New_Pid),

	% Logging information for verifying that the slave was restarted.
	io:format("Restarted slave ~p.~n", [X]),

	% Returns the new registry.
	[{X, New_Pid}|Registry];

restart_slave(Pid, [X|XS]) ->
	% When the slave isn't found, it starts searching the tail of the list.
	[X|restart_slave(Pid, XS)].




% Finds the Pid of slave number X in a registry.
find_slave(X, [{X, Pid}|_]) ->
	% Case for when the slave is the head of the list. 

	% Logging information for verifying that the slave was found.
	io:format("Found slave ~p.~n", [X]),

	% Returns the Pid of the slave.
	Pid;

find_slave(X, [_|YS]) ->
	% When the slave isn't found, it starts searching the tail of the list.
	find_slave(X, YS).




% Kills all of the slaves in a registry.
kill_slaves([]) ->
	% If the registry is empty, then it does nothing.
	done;

kill_slaves([{X, Pid}|Registry]) ->
	% Kills the head of the registry.

	% Logging information for verifying that the kill signal was attempted to be sent to the slave.
	io:format("Sending kill signal to slave ~p...~n", [X]),

	% Sends the kill signal to the slave.
	Pid ! kill,

	% Calls itself with the arguments being the tail of the registry.
	kill_slaves(Registry).




% Sends a message to slave X (through the master).
to_slave(Msg, X) ->
	master ! {to_slave, Msg, X}.

