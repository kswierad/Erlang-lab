%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mar 2018 21:06
%%%-------------------------------------------------------------------
-module(onp).
-author("Kamil").

%% API
-export([onp/1,process/2,bin_to_num/1]).

bin_to_num(Expr) ->
  case string:to_float(Expr) of
    {error, no_float} -> list_to_integer(Expr);
    {F, _Rest} -> F
  end.

process(["+"|T],[V1|[V2|ST]]) ->
  process(T,[V2+V1|ST]);
process(["*"|T],[V1|[V2|ST]]) ->
  process(T,[V2*V1|ST]);
process(["/"|T],[V1|[V2|ST]]) ->
  process(T,[V2/V1|ST]);
process(["-"|T],[V1|[V2|ST]]) ->
  process(T,[V2-V1|ST]);
process(["sqrt"|T],[V1|ST]) ->
  process(T,[math:sqrt(V1)|ST]);
process(["pow"|T],[V1|[V2|ST]]) ->
  process(T,[math:pow(V1,V2)|ST]);
process(["cos"|T],[V1|ST]) ->
  process(T,[math:cos(V1)|ST]);
process(["sin"|T],[V1|ST]) ->
  process(T,[math:sin(V1)|ST]);
process([H|T],Stack) ->
  process(T,[bin_to_num(H)]++Stack);
process([],[Val]) ->
  Val.

onp(Expr) ->

  process(string:tokens(Expr," "),[]).