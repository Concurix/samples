-module(factorial).
-compile(export_all).

f(0) -> 1;
f(N) -> N * f(N-1).

start() -> f(1997).
