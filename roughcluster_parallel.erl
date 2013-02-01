% Proceedings of the International MultilConference of Engineers and Computer Scientists 2011 
% Vol II, IMECS 2011, March 16018, 2011, Hong Kong
% Parallelized Rough K-means Clustering with Erlang Programming



-module(roughcluster_parallel).
-import(lists, [seq/2,sum/1,flatten/1,split/2,nth/2]). 
-import(io, [format/1,format/2]). 
-import(random, [uniform/1]).
-compile(export_all).

% use start().

% running a clustering on a Dim dimensional data of size Count of randomly distributed of range [0, Max], 
% returned C is the center of N clusters
% example: start(10000, 1000, 3, 7)  - 3 dimensional data set of size 10k into 7 clusters

start() -> start(10000, 1000, 2, 3).

start(Count, Max, Dim, N) ->
	DataSet = genData(Count, Max, Dim),
	rough_cluster(DataSet, 0.2,0.7,0.3,N).
	
	
mysplit(DataSet,_Num,NumPart,Result) when length(DataSet) =< NumPart ->
	lists:reverse([DataSet|Result]);
mysplit(DataSet,0,_NumPart,Result) ->
	lists:reverse([DataSet|Result]);
mysplit(DataSet,Num,NumPart,Result) ->
	{Part,Rest} = lists:split(NumPart,DataSet),
mysplit(Rest,Num-1,NumPart,[Part|Result]).

rough_cluster(DataSet,T,Wl,Wu,N) ->
	Means = [E || E <- lists:zip(lists:seq(1,N), get_centroid(DataSet,N))],
	DataSetList = mysplit(DataSet,8,(length(DataSet) div 8),[]),
	{_Time,_Result} = timer:tc(?MODULE,rough_cluster,	[DataSetList,N,T,Means,Wl,Wu,100,0]).
	
rough_cluster(_,_,_,Means,_,_,0,_) -> 
	Means;
rough_cluster(DataSet,N,T,Means,Wl,Wu,I,Count) ->
	Closest = determine_closest2(DataSet,Means),
	Approx = determine_approximation(Closest,Means,T, [{K,{[],[]}} || K <- lists:seq(1,N)]),
	NewMeans = calculate_means_rough(Approx,N,Wl,Wu,[]),
	AllTrue = lists:all(fun(X)->lists:member(X,Means) end,
						NewMeans),
	if AllTrue == true -> 
		NewMeans;
	true -> 
		rough_cluster(DataSet,N,T,NewMeans, Wl,Wu,I-1,Count+1)
	end.

genData(0, _ ) -> 
	[];
genData(Count, Max) -> 
	[[uniform(Max), uniform(Max), uniform(Max)]] ++ genData(Count-1, Max).   

genData(0, _, _ ) -> [];
genData(Count, Max, Dim) ->
	[genDataDim(Max, Dim) ] ++ genData(Count-1, Max, Dim).

genDataDim(_Max, 0) -> [];
genDataDim(Max, Dim) ->
	[uniform(Max) | genDataDim(Max, Dim -1)].



go() ->
	{_,N} = io:read("enter number of clusters:> "),
	{ok,DataSet} = file:consult("sample4.txt"),
	rough_cluster(DataSet,0.2,0.7,0.3,N).

get_centroid(DataSet,Cluster) -> 
	get_centroid(DataSet,Cluster,[]).
get_centroid(_DataSet,0,R) -> 
	lists:reverse(R);
get_centroid([],_,R) -> 
	lists:reverse(R);
get_centroid([H|T],N,R) ->
	case lists:member(H,R) of
		true -> get_centroid(T,N,R);
		_ -> get_centroid(T,N-1,[H|R])
	end.

random_assign_approximation([],_,Result) -> 
	Result;
random_assign_approximation([H|T],K,Result) ->
	N = random:uniform(K),
	{LowerApp,UpperApp} = proplists:get_value(N,Result),
	NewResult = [{N,{[H|LowerApp],UpperApp}} | proplists:delete(N,Result)],
	random_assign_approximation(T,K,NewResult).

calculate_means([]) -> 
	0;
calculate_means(H) ->
	[HR|_T] = H,
	Dim = length(HR),
	GroupByDim = [[lists:nth(N,L) || L <- H] || N <- lists:seq(1,Dim)],
	[lists:sum(G)/length(G) || G <- GroupByDim].

calculate_means_rough(_,0,_,_,Result) -> 
	Result;
calculate_means_rough(Cluster,K,Wl,Wb,Result) ->
	{LowerApp, UpperApp} = proplists:get_value(K,Cluster),
	Mk = case UpperApp of
		[] -> calculate_means(LowerApp);
		_Else ->
		LMeans = [Wl*M || M <- calculate_means(LowerApp)],
		UMeans = [Wb*M || M <- calculate_means(UpperApp)],
		[L+U || {L,U} <- lists:zip(LMeans,UMeans)]
	end,
	NewMean = [{K,Mk}|Result],
	calculate_means_rough(Cluster,K-1,Wl,Wb,NewMean).

euclidean_distance(X1,X2) -> math:pow((X1-X2),2).

distance_cluster(Xn,Mk) ->
	{K,M} = Mk,
	{K,math:sqrt(lists:sum(lists:map(fun({X1,X2}) ->
	euclidean_distance(X1,X2) end, lists:zip(Xn,M))))}.

min_distance(Xn,Means) ->
	Dist = [distance_cluster(Xn,Mk) || Mk <- Means],
	[FirstDist |_Rest] = Dist,
	{H,_} = lists:foldl(fun({K1,M1},{K2,M2}) ->
			if M1 < M2 -> 
					{K1,M1};
				true -> 
					{K2,M2}
				end
			end,
			FirstDist,Dist),
	H.

determine_closest([],_Means,Result) -> 
	Result;
determine_closest([H|T],Means,Result) ->
	Min = min_distance(H,Means),
	{Lower,Upper} = proplists:get_value(Min,Result,{[],[]}),
	NewResult = [{Min,{[H|Lower],Upper}} | proplists:delete(Min,Result)],
	determine_closest(T,Means,NewResult).
determine_closest2(DataSetList,Means) ->
	lists:foreach(fun(H) ->
			spawn(?MODULE,determine_closest_process, [self(),H,Means])
			end, 
			DataSetList),
	determine_closest_response(length(DataSetList), length(Means),[]).

determine_closest_process(Parent,DataList,Means) ->
	Parent ! determine_closest3(DataList,Means,[]).
determine_closest3([],_Means,Result) -> 
	Result;
determine_closest3([H|T],Means,Result) ->
	Min = min_distance(H,Means),
	determine_closest3(T,Means,[{Min,H}|Result]).
determine_closest_response(0,LMean,Result) ->
	FlattenResult = lists:flatten(Result),
	lists:map(fun(M) ->
			Lowers = proplists:get_all_values(M,FlattenResult),
			{M,{Lowers,[]}}
			end,
			lists:seq(1,LMean));
determine_closest_response(N,LMean,Result) ->
	receive
		R ->
			determine_closest_response(N-1,LMean,[R|Result])
	end.
determine_set_T(H,Mh,Means,Epsilon) ->
	lists:filter(fun(Mk) ->
			{_,Dist1} = distance_cluster(H,Mk),
			{_,Dist2} = distance_cluster(H,Mh),
			((Dist1 - Dist2) =< Epsilon)
			end,
			Means).
determine_approximation([],_,_,Result) -> 
	Result;
determine_approximation([H|T],Means,Epsilon,Result) ->
	{K,Data} = H,
	{Lower,_} = Data,
	Mh = proplists:get_value(K,Means),
	OtherMeans = proplists:delete(K,Means),
	NewResult = determine_set_T(Lower,{K,Mh}, OtherMeans,Epsilon,Result),
	determine_approximation(T,Means,Epsilon,NewResult).
determine_set_T([],_,_,_,Result) -> 
	Result;
determine_set_T([H|T],Mh,Means,Epsilon,Result) ->
	{K,_} = Mh,
	SetT = determine_set_T(H,Mh,Means,Epsilon),
	NewResult = 
			if SetT == [] ->
				{Lower,Upper}= proplists:get_value(K,Result,{[],[]}),
				[{K,{[H|Lower],Upper}} | proplists:delete(K,Result)];
			true ->
				upper_assign_approximation(H,[Mh|SetT],Result)
			end,
	determine_set_T(T,Mh,Means,Epsilon,NewResult).

upper_assign_approximation(_Data,[],Result) -> 
	Result;
upper_assign_approximation(Data,[Mt|T],Result) ->
	{K,_} = Mt,
	{Lower,Upper}= proplists:get_value(K,Result,{[],[]}),
	NewResult = [{K,{Lower,[Data|Upper]}} | proplists:delete(K,Result)],
	upper_assign_approximation(Data,T,NewResult).