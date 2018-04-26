%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. kwi 2018 12:18
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3 ]).
-export([addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,getDailyMean/2,getMinMaxValue/2,crash/0]).

start_link(InitialValue) ->
  gen_server:start_link(
    {local,pollutionServer},
    pollution_gen_server,
    InitialValue, []).
init( _) ->
  {ok, pollution:createMonitor()}.


%% user interface
addStation(Name, Coords) ->
  gen_server:call(pollutionServer, {addStation, Name, Coords}).
addValue(Station, Type, Date, Value) ->
  gen_server:call(pollutionServer, {addValue, Station, Type, Date, Value}).
%% callbacks
handle_call({addStation, Name, Coords},_From, Monitor) ->
  case pollution:addStation(Name, Coords, Monitor) of
    {error, Msg} -> {reply, Msg, Monitor};
    Result -> {reply, "Added Station", Result}
  end;
handle_call({addValue,Station, Type, Date, Value}, _From, Monitor) ->
  case pollution:addValue(Station, Date, Type, Value, Monitor) of
    {error, Msg} -> {reply, Msg, Monitor};
    Result -> {reply, "Added Value", Result}
  end.




