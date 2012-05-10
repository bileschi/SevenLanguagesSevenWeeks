-module(wc).
-export([count_words/1]).

count_words(S) -> count_words(string:tokens(S, " "), 0).

count_words([], N) -> N;
count_words([H|T], N) -> count_words(T, N+1).
