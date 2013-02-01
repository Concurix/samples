%% Recursive implementation of FIB
%%
%% The standard implementation of a recursive implementation of FIB has runtime O(2^N)
%%
%% The memoized version of recursive FIB is O(N)
%%
%% [fib:fibRecur(N) || N <- lists:seq(0, 20)].



-module(fib).
-export([fibRecur/1, start/0]).

start() ->
    fibRecur(20).

%%
%% The classic recursive version
%%

fibRecur(0) -> 0;

fibRecur(1) -> 1;

fibRecur(N) -> fibRecur(N - 1) + fibRecur(N - 2).

