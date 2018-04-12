%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. kwi 2018 11:38
%%%-------------------------------------------------------------------
-module(qsort).
-author("Kamil").

%% API
-export([qs/1,randomElems/3,compareSpeeds/3,digitize/1,sumDigits/1]).

qs([]) -> [];
qs([A]) -> [A];
qs([Pivot|Tail]) -> qs(lessThan(Tail,Pivot))++ [Pivot] ++ qs(grtEqThan(Tail,Pivot )).

lessThan([], _) -> [];
lessThan(List, Arg) -> lists:filter(fun (X) -> X<Arg end,List).

grtEqThan([], _) -> [];
grtEqThan(List, Arg) -> lists:filter(fun (X) -> X>=Arg end, List).

randomElems(N,Min,Max) ->   lists:map(fun (X) -> rand:uniform(Max-Min+1)+Min-1 end,lists:seq(0,N)).

compareSpeeds(List,Fun1, Fun2) -> {Time1,_} = timer:tc(Fun1,[List]),
  {Time2,_} = timer:tc(Fun2,[List]),
  {Time1,Time2}.

digitize(A) -> lists:map(fun (X) -> X-48 end,integer_to_list(A)).

sumDigits(A) -> lists:foldl(fun (X,Y) -> X+Y end,0,digitize(A)).