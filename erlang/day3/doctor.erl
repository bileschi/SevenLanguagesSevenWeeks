-module(doctor).
-export([loop/0]).

loop() ->
	process_flag(trap_exit, true),
	receive
		% protect ->
		% 	io:format("Creating two guardians for doctor~n"),
		%	Guard1 = spawn(fun doc_guard:loop/0).
		%	Guard2 = spawn(fun doc_guard:loop/0).
		%	Guard1 ! {register_doctor, self()}
		%	Guard2 ! {register_doctor, self()}
		%	Guard1 ! {register_guard, Guard2}
		%	Guard2 ! {register_guard, Guard1}
		%	loop();
		new ->
			io:format("Creating and monitoring process.~n"),
			register(revolver, spawn_link(fun roulette:loop/0)),
			loop();

		{'EXIT', From, Reason} ->
			io:format("The shooter ~p died with reason ~p.", [From, Reason]),
			self() ! new,
			loop()
		end.
