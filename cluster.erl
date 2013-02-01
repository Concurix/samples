% INTERNATIONAL JOURNAL OF MATHEMATICS AND COMPUTERS IN SIMULATION 
%  Issue 4, Volume 4, 2010

%------------k-means clustering ---------
% data file "points.dat" must exist in working directory 
% example of data file: 
%    [2,7].     [3,6].     [1,6].     [3,7].     [2,6]. 
%    [21,25]. [16,29]. [29,25]. [18,23]. [16,33]. 
% Then test a program with these commands: 
%    c(cluster).             %% compile a program 
%    cluster:go().           %% then run 

-module(cluster). 
-export([go/0, clustering/3]). 
go() -> 
	{_, DataList} = file:consult("points.dat"), 
	file:close("points.dat"), 
	kMeans(DataList).       
% ------------------------
% start k-means clustering 
% ------------------------
kMeans(PL) -> 
	{_,N} = io:read('enter number of clusters:> '),  
			% for this example input "2"  
			% then select initial centroids                                          
	CL = take(N, PL),    
	io:format("~n AllPoints = ~w ~n",[PL]), 
	io:format("~n Initial Centroid = ~w~n",[CL]),          
			% report data and initial centroids 
			% start clustering process with  
			% cluster number 1 
			% then move on to cluster number 2  
			% and so on           
                    
	{TT,{Centroid,DataGroup}} = timer:tc(cluster, clustering,[1,CL,PL]), 
	T = TT/1000000,          
			% record running time and report time 
			% in a unit of seconds                            
	io:format("~n~n__Time for k-means is ~w  second",[T]), 
	io:format("~n~n__Calculated Centroid=~w~n~n", [Centroid]), 
	printCluster(1, N, DataGroup). 

% .................................... 
% supporting clauses for kMeans 
% 
%  These clauses take first  distinct-n element of list    

take(0,_) -> []; 
take(N,[H|T]) -> [H|take(N-1,T)].     

% to print cluster nicely 
printCluster(_,_,[]) -> 
	end_of_clustering; 
printCluster(_,0,_) -> 
	end_of_clustering; 
printCluster(I,N, [H|T]) -> 
	{Centroid, ClusterMember} = H, 
	io:format("~n__Cluster:~w  Mean point = ~w~n",[I,Centroid]), 
	io:format("               Cluster member is ~w~n",[ClusterMember]), 
	printCluster(I+1,N-1,T). 

% --------------------------
% repetitive data clustering 
% --------------------------
clustering(N,CL,PL)-> 
	L1 = lists:map( fun(A) -> nearCentroid(A,CL)  
					end,  
					PL),   
	L2 = transform(CL,L1), 
	NewCentroid = lists:map(fun({_,GL}) -> findMeans(GL)
					end,  
					L2),   
	if NewCentroid==CL ->
			io:format("~nNo cluster changes~n"), 
			io:format("From Loop1->stop at Loop~w~n",[N]), 
			{NewCentroid,L2};      
				% return new centroids and  
				% cluster members as a list L2                
		N>=90 ->
				% max iterations=90 
			io:format("Force to stop at Loop ~w~n",[N]),                
			io:format("Centroid = ~w",[NewCentroid]),
			{NewCentroid,L2};               
				% return new centroids and  
				% cluster members as a list L2   
        true ->
				% default case 
			io:format("~nLoop=~w~n",[N]), 
			io:format("~nNewCentroid=~w ~n",[NewCentroid]), 
			clustering(N + 1, NewCentroid, PL)    
         end.  
				% end if and end clustering function 
        
% transform a format "Point-CentroidList"  
% to "Centroid-PointList" 
% example, 
%         transform([[1]],[{[2],[1]},{[3],[1]}]).  
%              -->  [{[1],[[2],[3]]} ] 
transform([], _) -> 
	[]; 
transform([C|TC], PC) -> 
	[ {C, t1(C, PC)} | transform(TC, PC)]. 

t1(_, []) -> [] ;           
t1(C1, [H|T]) -> 
	{P,C} = H,  
	if  C1==C -> 
			[ P| t1(C1, T) ];
		C1=/=C -> t1(C1, T)
	end.  

% -----------------------------
% Given a data point and a centroidList, 
%     the clause nearCentroid computes a nearest 
%     centroid and then returns 
%     a tuple of Point-Centroid 
% example: 
%     nearCentroid( [1], [[2],[3],[45],[1]] ).  
%                    ---> [ [1], [1] ] 
nearCentroid(Point, CentroidL)-> 
     LenList = lists:zip(
					lists:map(fun(A) -> distance(Point,A)  
						end, 
						CentroidL),           
				CentroidL), 
	[ {_, Centroid} | _ ] = lists:keysort(1,LenList), 
	{Point, Centroid}.                 
			% return this tuple to caller  
   
% --------------------------
% compute Euclidean distance 
% --------------------------
distance([], []) -> 0;   
distance([X1|T1], [X2|T2]) -> 
            math:sqrt((X2-X1)*(X2-X1) + distance(T1,T2) ). 
% ----------------------------------
% calculate mean point (or centroid) 
% ----------------------------------
% example, 
%      findMeans([[1,2], [3,4]]). --> [2.0,3.0] 
findMeans(PointL) -> 
        [H|_] = PointL, 
        Len = length(H), 
         AllDim = lists:reverse( allDim(Len,PointL) ),
         lists:map(fun(A)-> mymean(A) end, AllDim ). 
  
allDim(0, _) -> [];
allDim(D, L) -> [ eachDimList(D,L) | allDim(D-1,L) ].
eachDimList(_, []) -> [];
eachDimList(N, [H|T]) -> 
          [ lists:nth(N, H) | eachDimList(N, T) ].
mymean(L) -> lists:sum(L) / length(L).    

% ---------- End of Serial k-means program -----------
% 
% ----------------------------
% Running example: 
% ----------------------- 
% 1> c(cluster).   
%    {ok,cluster}
% 2> cluster:go().
%     enter number of clusters:> 2.
 %     AllPoints = [[2,7],[3,6],[1,6],[3,7],[2,6],[1,5],[3,5], 
%                         [2,5],[2,6],[1,6],[21,25],[16,29], 
%                         [29,25], [18,23],[16,33],[25,32], 
%                         [20,24],[27,21],[16,21],[19,34]] 
 %     Initial Centroid = [[2,7],[3,6]]
%      Loop = 1
%      NewCentroid = [ [1.75,6.0], 
%                                [17.75,23.166666666666668]]
%      Loop = 2
%      NewCentroid = [ [2.0,5.9], [20.7,26.7] ]
%      No cluster changes
%      From Loop1->stop at Loop3 
%      __Time for k-means is 0.031 second 
%      __Calculated Centroid=[[2.0,5.9],[20.7,26.7]]
%      __Cluster:1  Mean point = [2.0,5.9]
%              Cluster member is [ [2,7],[3,6],[1,6],[3,7], 
%                                             [2,6],[1,5],[3,5], 
%                                             [2,5],[2,6],[1,6]]
%       __Cluster:2  Mean point = [20.7,26.7]
%               Cluster member is [ [21,25],[16,29],[29,25], 
%                                              [18,23],[16,33],[25,32], 
%                                              [20,24],[27,21],[16,21], 
%                                              [19,34]]
%       end_of_clustering
