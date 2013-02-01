-module(img_stress).
-export([start/0, main/2, process_image/4]).
-define(TIMEOUT, 100000).
-define(ITER, 1). % was 10

start() ->
	main(2, "../samples/crab.png").  % was 50

main(N, File) ->
	case ets:info(img_tab) of 
		undefined -> ok;
		_X -> ets:delete(img_tab)
	end,
	T = ets:new(img_tab, [public, {heir, whereis(init), concurix}]),
	{ok, Bin} = file:read_file(File),
	spawn_loop(N, N, Bin, self(), T).
		
spawn_loop(N, 0, _Bin, Pid, T) ->
	count_loop(N, 0, Pid, T);
spawn_loop(N, Counter, Bin, Pid, T) ->
	Pid2 = spawn(img_stress, process_image, [Bin, ?ITER, Pid, T]),
	timer:kill_after(?TIMEOUT, Pid2),
	spawn_loop(N, Counter-1, Bin, Pid, T).

count_loop(0, Count, _Pid, T) ->
	%%io:format("exiting pid ~p ~n", [Pid]),
	ets:delete(T),
	Count;
count_loop(N, Count, Pid, T) ->
	receive
		Msg -> NewCount = Count + Msg
	end,
	count_loop(N-1, NewCount, Pid, T).
	
process_image(_Bin, 0, Pid, _T) ->
	Pid ! ?ITER;
process_image(Bin, Iter, Pid, T) ->	
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	Lat = {random:uniform(25) + 1, random:uniform(60)},
	Long = {random:uniform(57) + 67, random:uniform(60)},
	{ok, Img} = erl_img:load(Bin),
	{ok, Bin2} = erl_img:to_binary(Img),
	ets:insert(T, {{Lat, Long}, Bin2} ),
	process_image(Bin, Iter -1, Pid, T).
