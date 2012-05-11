-module(translator_monitor).
-export([loop/0]).

loop() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Creating and monitoring translator.~n"),
			register(translator, spawn_link(fun translate:loop/0)),
			loop();

		{'EXIT', From, Reason} ->
			io:format("Translator ~p died with reason ~p.", [From, Reason]),
			self() ! new,
			loop()
		end.
