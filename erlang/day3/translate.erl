-module(translate).
-export([loop/0]).

loop() ->
	receive
		"casa" ->
			io:format("house~n"),
			loop();
		"blanca" ->
			io:format("white~n"),
			loop();
		"die" -> 
			io:format("Dios Mio!  Ack...~n"),
			exit({translate, died, at, erlang:time()});

		_ ->
			io:format("I don't understand.~n"),
			loop()
end.
