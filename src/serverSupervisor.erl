%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. kwi 2018 23:03
%%%-------------------------------------------------------------------
-module(serverSupervisor).
-author("Kamil").

-behaviour(supervisor).
-export([start_link/1, init/1]).
start_link(InitValue) ->
  supervisor:start_link({local, supervisor},
    ?MODULE, InitValue).

init(InitValue) ->
  {ok, {
    {one_for_all, 2, 3},
    [ {pollutionServer,
      {pollution_gen_server, start_link, [InitValue]},
      permanent, brutal_kill, worker, [pollution_gen_server]}
    ]}
  }.
