-module(roulette).
-export([loop/0]).

% send a number 0 - 6
loop() ->
	receive
		3 -> io:format("bang.~n"), exit({roulette, die, at, erlang:time()});
		_ -> io:format("click.~n"), loop()
end.

% ask if process is alive with:
% erlang:is_process_alive(Gun).
