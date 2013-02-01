%% The Computer Language Benchmarks Game
%% http://shootout.alioth.debian.org/
%% Contributed by Fredrik Svahn based on Per Gustafsson's mandelbrot program

-module(mandelbrot).

-export([start/0, main/1, row/3]).
-define(LIM_SQR, 4.0).
-define(ITER, 50).
-define(SR, -1.5).
-define(SI, -1).

start() -> main(64).

main(N) ->
    case file:open("/tmp/output.ppm", [write]) of
        {ok, F} ->
            Start = now(),
            io:put_chars(F, ["P4\n", integer_to_list(N), " ", integer_to_list(N), "\n"]),
    
            %% Spawn one process per row

            Row = fun(Y)-> spawn(mandelbrot, row, [F, Y, N]) end,
            Pids = lists:map(Row, lists:seq(0, N - 1)),

            %%Pass token around to make sure printouts are in the right order
            hd(Pids) ! tl(Pids) ++ [ self() ],
            receive _Token -> file:close(F) end,
            Stop = now(),
            Time = timer:now_diff(Stop, Start),
            %%io:format("Elapsed time (micros): ~p\n", [Time]),
            Time;
        {error, Reason} ->
            io:format("Couldn't open output.png: ~p\n", [Reason]),
            0
    end.

%Iterate over a row, collect bits, bytes and finally print the row
row(F, Y, N) ->
  row(F, 0, ?SI + Y * 2 / N, N, 0, [], 7).

row(F, X, _, N, Bits, Bytes, BitC) when X =:= N-1 ->
    receive Pids ->
	    put_chars(F, Bits, Bytes, BitC),
	    hd(Pids) ! tl(Pids)
    end;

row(F, X, Y2, N, Bits, Bytes, 0) ->
    row(F, X+1, Y2, N, 0, [Bits bsl 1 + m(?ITER, ?SR+X*2/N, Y2) | Bytes], 7);

row(F, X, Y2, N, Bits, Bytes, BitC) ->
    row(F, X+1, Y2, N, Bits bsl 1 + m(?ITER, ?SR+X*2/N, Y2), Bytes, BitC-1).

%Mandelbrot algorithm
m(Iter, CR,CI) -> m(Iter - 1, CR, CI, CR, CI).

m(Iter, R, I, CR, CI) ->
    case R*R+I*I > ?LIM_SQR of 
	false when Iter > 0 -> m(Iter-1, R*R-I*I+CR, 2*R*I+CI, CR, CI);
	false -> 1;
	true -> 0
    end.

put_chars(F, _, Bytes, 7)-> io:put_chars(F, lists:reverse(Bytes));
put_chars(F, Bits, Bytes, C) -> io:put_chars(F, lists:reverse([Bits bsl (C+1) | Bytes])).
