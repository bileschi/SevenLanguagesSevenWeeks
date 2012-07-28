-module(doc_guard).
-export([loop/0]).

loop() ->
	process_flag(trap_exit, true),
	receive
		{register_doctor, DocPID} ->
			io:format("Protecting the Doctor~n");
			% todo.  
		{register_guard, GuardPID} ->
			io:format("Protecting the other guard~n");
		{'EXIT', From, Reason} ->
			io:format("The shooter ~p died with reason ~p.", [From, Reason]),
			self() ! new,
			loop()
		end.
