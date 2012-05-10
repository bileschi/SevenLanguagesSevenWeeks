-module(count10).
-export([count/0]).

count() -> count(0).
count(N) when N < 10 -> io:format("~w~n",[N+1]), count(N+1);
count(N) -> ok.


