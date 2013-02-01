%% The Computer Language Benchmarks Game
%% http://shootout.alioth.debian.org/
%% Contributed by Fredrik Svahn based on Per Gustafsson's mandelbrot program

-module(webLoad).
-export([start/0, mandel_rpc/0]).

-define(LIM_SQR,   4.0).

-define(ITER,     50).

-define(SR,        1.5).
-define(SI,        1.0).

start() -> 
    Mandel = spawn(webLoad, mandel_rpc, []),

    Mandel ! { mandel,   2},
    Mandel ! { mandel,   4},
    Mandel ! { mandel,   8},
    Mandel ! { mandel,  16},
    Mandel ! { mandel,  32},
    Mandel ! { mandel,  64},
    Mandel ! { cr },

    Mandel ! { mandel,   2},
    Mandel ! { mandel,   4},
    Mandel ! { mandel,   8},
    Mandel ! { mandel,  16},
    Mandel ! { mandel,  32},
    Mandel ! { mandel,  64},
    Mandel ! { cr },

    Mandel ! { mandel,   2},
    Mandel ! { mandel,   4},
    Mandel ! { mandel,   8},
    Mandel ! { mandel,  16},
    Mandel ! { mandel,  32},
    Mandel ! { mandel,  64},
    Mandel ! { cr },

    Mandel ! { mandel,   2},
    Mandel ! { mandel,   4},
    Mandel ! { mandel,   8},
    Mandel ! { mandel,  16},
    Mandel ! { mandel,  32},
    Mandel ! { mandel,  64},
    Mandel ! { cr },

    Mandel ! stop,
    0.

mandel_rpc() ->
    receive
      { mandel, N } -> 

        Start   = now(),

        Excess  = mandel(N),

        Stop    = now(),
        RunTime = timer:now_diff(Stop, Start),

        %%io:format("Mandel(~4.B) -> ~8.B.  App time (us) ~8.B. ~n", [N, Excess, RunTime]),

        mandel_rpc();

      { cr } -> 
        io:format("~n"),
        mandel_rpc();

       stop       ->
        void
    end.

mandel(N) ->
    pixel(N, 0, 0, 0).

pixel(N, _X,  Y, Excess) when Y =:= N ->
    Excess;

pixel(N,  X,  Y, Excess) when X =:= N ->
    pixel(N, 0, Y + 1, Excess);

pixel(N,  X,  Y, Excess) ->
    Bit = bit(X, Y, N),
    case Bit of
      0 -> pixel(N, X + 1, Y, Excess + 1);
      1 -> pixel(N, X + 1, Y, Excess - 1)
    end.

% Compute the magnitude at (X, Y)
bit(X, Y, N) ->
    mloop(?ITER, (2.0 * X) / N - 1.5, (2.0 * Y) / N - 1.0).
    
mloop(Iter, CR, CI) -> 
    mloop(Iter, CR, CI, 0.0, 0.0).

mloop(Iter, CR, CI, ZR, ZI) ->
    case ZR * ZR + ZI * ZI > ?LIM_SQR of 
	false when Iter > 0 -> mloop(Iter - 1, CR, CI, ZR * ZR - ZI * ZI + CR, 2 * ZR * ZI + CI);
	false               -> 1;
	true                -> 0
    end.
