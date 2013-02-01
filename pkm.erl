% INTERNATIONAL JOURNAL OF MATHEMATICS AND COMPUTERS IN SIMULATION 
%  Issue 4, Volume 4, 2010

% ------- A parallel k-means program --------- 
%  Compile program with a command 
% 
%              c(pkm,[export_all]). 
% 
%  To unbinding variables from the previous run 
%   use a command 
% 
%               f(Var)     % means clear Var 
%   
%  Start experimentation by calling a function 
%   genData to generate 8000 synthetic data points 
% 
%               f(), NumDat = 8000,
%               D = pkm:genData(NumDat,10000). 
% 
%  Then identify number of clusters 
% 
%               f(NumCent), f(CL), 
%               NumCent = 4,
%               CL = lists:sublist(D, NumCent).  
% 
%  Start parallelization by identifying  
%   number of data partitions  
% 
%                f(NumPar), f(DL), NumPar=8, 
%                DL = pkm:mysplit(length(D) div NumPar, 
%                                             D, NumPar). 
% 
%  Record running time with the command 
%               {TReal,RealCen} = timer:tc(pkm, 
%                                           start,[DL,CL,length(DL)]). 
% Then record the running time of approximate parallel 
% k-means (in this example apply 50%  
% data sampling scheme) 
% 
%                f(RDL), 
%                RDL=pkm:mrand(DL,50), 
%                {TRand,RandCen} = timer:tc(pkm, 
%                                       start,[RDL,CL,length(RDL)]). 
% 
%  Calculate time difference between the parallel  
%   k-means and the approximate (50% data points) 
%   parallel k-means with a command 
% 
%          pkm:mydiff({TReal,TRand},{RealCen,RandCen}). 
% 
%  To show different time of different percentages  
%  from the same Centroid use the following commands  
% 
%           f(RDL), f(Rand), 
%           RDL = pkm:mrand(DL,40), 
%           Rand = pkm:start(RDL,CL,length(RDL)), 
%           f(RealDL), f(Real), 
%           RealDL = pkm:mrand(DL,100), 
%           Real = pkm:start(RealDL,CL,length(RealDL)). 
%           f(RDL), f(Rand), f(TimeR),  
%           f(TimeReal), f(Per),  
%           Per = 40, RDL = pkm:mrand(DL,Per), 
%           {TimeR,Rand} = timer:tc(pkm, 
%                                   start,[RDL,CL,length(RDL)]), 
%           f(RealDL), f(Real),  
%           RealDL = pkm:mrand(DL,100), 
%           {TimeReal,Real} = timer:tc(pkm, 
%                              start,[RealDL,CL,length(RealDL)]). 
% 
% To compute percentage of time difference, 
% use a command 
% 
%     io:format("___For ~w Percent, diff.time = 
%                        ~w sec,length=~w", 
%                         [Per, (TimeReal-TimeR) /1000000,
%                         lists:sum(pkm:diffCent(Real,Rand))]).  
% 
%  All of the commands in clustering experimentation 
%  are also included in the test function 
% 
-module(pkm). 
-import(lists, [seq/2,sum/1,flatten/1,split/2,nth/2]). 
-import(io, [format/1,format/2]). 
-import(random, [uniform/1]). 

%---- for clustering experimentation ---------
test(_NRand) -> 
              NumDat = 8000,   
D = pkm:genData(NumDat,10000), 
NumCent = 4,
              CL = lists:sublist(D, NumCent),  
NumPar=8, 
DL=pkm:mysplit(length(D) div NumPar, 
                                         D, NumPar), 
              {TReal, RealCen} = timer:tc(pkm,  
                                         start, [DL,CL, length(DL)]), 
              RDL = pkm:mrand(DL,50), 
{TRand,RandCen} = timer:tc(pkm,  
                                    start, [RDL,CL, length(RDL)]), 
pkm:mydiff({TReal, TRand},  
                                  {RealCen, RandCen}). 
% ---- spawn a new process 
%           and start the newly created process 
%           with a function c(Pid) 
myspawn(0) -> [] ; 
myspawn(N) -> 
           [spawn(?MODULE, c, [self()]) | myspawn(N-1) ]. 
% random sampling without replacement 
% 
myrand(_, 0) -> [];
myrand(L, Count) -> 
           E = nth((uniform(length(L))), L), 
L1= L -- [E], 
          [E | myrand(L1, Count-1)].  
                               %  for 100 percent sampling 
mrand(L, 100) -> L;
                               % random in each partition 
mrand([], _) -> [];
mrand([HL|TL], X ) ->  
          [myrand(HL, trunc(length(HL)/(100/X) )) |  
                          mrand(TL,X)]. 
mysend(LoopN, [CidH|CT], Cent, [DataH|DT]) ->   
          CidH ! {LoopN, Cent, DataH}, 
mysend(LoopN, CT, Cent, DT); 
mysend( _, [], _ ,_) -> true. 
% Compute difference between centroids 
% 
diffCent( [H1|T1], [H2|T2]) -> 
          [ abs(H1-H2) | diffCent(T1,T2) ];
diffCent( [], _ )->[]. 
mystop( [CH|CT] ) ->  
                CH ! stop, 
mystop(CT); 
mystop([]) -> true. 
myrec( _, 0) -> [];
myrec(LoopN, Count) -> 
      receive  
          {LoopN, L} -> [L | myrec(LoopN,Count-1) ];
          Another -> self() ! Another   % send to myself 
      end. 
% generate 2 dimensional data points 
%  example:  [{2,76},...] 
%  
genData(0, _ ) -> [];
genData(Count, Max) -> 
           [ {uniform(Max), uniform(Max)} | 
                 genData(Count-1, Max)]. 
mysplit(_, _, 0) -> [];    
mysplit(Len, L, Count) -> 
            {H, T} = split(Len, L),  
            [ H | mysplit(Len, T, Count-1) ].
start( DataL, Cent, NumPar) -> 
          CidL = myspawn(NumPar), 
           LastC = myloop(CidL,Cent,DataL,NumPar,1), 
           format("~nCentroid=~w",[LastC]), 
          LastC. 

myloop(CidL, Cent, DataL, NumPar, Count) -> 
         mysend(Count, CidL, Cent, DataL),   
L = flatten(myrec(Count, NumPar)),  
C_= calNewCent(Cent, L), 
format("~w.", [Count]), 
if  Count >100 -> mystop(CidL), 
                                     C_ ;  
              Cent/= C_ -> myloop(CidL, C_, DataL,  
                                            NumPar, Count+1); 
              true -> mystop(CidL), 
                         C_    
         end. 
c(Sid) -> 
       receive 
            stop -> true; 
           {LoopN, Cent, Data} -> L = locate(Data,Cent), 
                                               Sid ! {LoopN,L}, 
                                               c(Sid) 
       end. 
calNewCent(Cent, RetL) -> 
          LL = group(Cent, RetL), 
         avgL(LL). 
%---- supplementary functions------
% 
mydiff( {TReal,TRand}, {RealCen,RandCen} ) -> 
          { (TReal-TRand)/1000000, 
            mdiff(RealCen, RandCen) / length(RandCen) }.
mdiff( [ {X,Y}|T1], [ {X1,Y1}|T2] ) ->  
             distance({X,Y}, {X1,Y1}) + mdiff(T1,T2);  
mdiff([], _ ) -> 0.
group([H|T] , RetL) -> 
            [ [X || {X,M} <- RetL , M==H ] | group(T, RetL)]; 
group([],_) -> [].
avgL( [HL|TL] ) ->  
           N = length(HL), 
[ {sumX(HL) / N, sumY(HL) / N} | avgL(TL)]; 
avgL([]) -> [].
sumX( [ {X, _} | T] ) -> X + sumX(T); 
sumX([]) -> 0. 
sumY( [ {_,Y} | T] ) -> Y + sumY(T); 
sumY([]) -> 0. 
locate( [H|T], C) -> 
            NearC = near(H,C), 
           [ {H, NearC} |locate(T, C) ];
locate( [], _ ) -> [].
near(H, C) -> 
             mynear(H, C, {0,1000000000} ).
mynear(D, [H|T], {MinC, Min}) -> 
            Min_= distance(D, H),
            if Min>Min_ -> mynear(D, T, {H, Min_} );
                        true  -> mynear(D, T, {MinC, Min} ) 
             end ; 
mynear(_ , [], {MinC, _ } ) -> MinC.   
distance( {X, Y}, {X1, Y1}) -> 
             math:sqrt( (X-X1)*(X-X1) + (Y-Y1)*(Y-Y1) ).