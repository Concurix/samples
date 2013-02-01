-module(jsx_stress).
-export([start/0, decode/1]).

start() ->
	main(100, "../samples/sample.json").
	
main(N, File) ->
	{ok, Json} = file:read_file(File),
	stress_decode(N, Json).
	
stress_decode(0, _Json) ->
	ok;
stress_decode(N, Json) ->
	spawn(jsx, decode, [Json]),
	stress_decode(N-1, Json).
	
%%
%% change the spawn above to jsx_stress to call this version for more detailed
%% timing.
decode(Json) ->
	Start = now(),
	jsx:decode(Json),
	Finish = now(),
	Delta = timer:now_diff(Start, Finish),
	io:format("json decode in ~p ~n", [Delta]).
	