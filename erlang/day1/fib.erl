-module(fib).
-export([another_fib/1, another_fact/1]).

another_fact(0) -> 1;
another_fact(1) -> 1;
another_fact(N) -> N * another_fact(N-1).

another_fib(0) -> 1;
another_fib(1) -> 1;
another_fib(N) -> another_fib(N-1) + another_fib(N-2).