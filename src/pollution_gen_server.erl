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
-export([start_link/1, init/1, handle_call/3, handle_cast/2 ]).
-export([addStation/2,addValue/4,stop/0,removeValue/3,getOneValue/3,getStationMean/2,getDailyMean/2,getMinMaxValue/2,crash/0]).

start_link(InitialValue) ->
  gen_server:start_link(
    {local,pollutionServer},
    pollution_gen_server,
    InitialValue, []).
init( _InitialValue) ->
  {ok, pollution:createMonitor()}.


%% user interface
addStation(Name, Coords) ->
  gen_server:call(pollutionServer, {addStation, Name, Coords}).
addValue(Station, Type, Date, Value) ->
  gen_server:call(pollutionServer, {addValue, Station, Type, Date, Value}).
removeValue(Station, Datetime, Type) ->
  gen_server:call(pollutionServer,{removeValue,Station, Datetime, Type}).
getOneValue(Station, Datetime, Type) ->
  gen_server:call(pollutionServer,{getOneValue, Station, Datetime, Type}).
getStationMean(Station, Type) ->
  gen_server:call(pollutionServer,{getStationMean, Station, Type}).
getDailyMean(Datetime, Type) ->
  gen_server:call(pollutionServer,{getDailyMean, Datetime, Type}).
getMinMaxValue(Station, Type) ->
  gen_server:call(pollutionServer,{getMinMaxValue, Station, Type}).
crash() ->
  gen_server:call(pollutionServer, crash).
stop() ->
  gen_server:cast(pollutionServer, stop).
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
  end;

handle_call({removeValue, Station, Datetime, Type}, _From, Monitor) ->
  P = pollution:removeValue(Station,Datetime,Type,Monitor),
  case P of
    {error, ErrMsg} -> {reply, ErrMsg, Monitor};
    Result -> {reply, "Removed Value", Result}
  end;
handle_call({getOneValue, Station, Datetime, Type},_From, Monitor) ->
  P = pollution:getOneValue(Station,Datetime,Type,Monitor),
  case P of
    {error, ErrMsg} -> {reply, ErrMsg, Monitor};
    Result -> {reply, Result, Monitor}
  end;
handle_call({getStationMean, Station, Type},_From, Monitor) ->
  P = pollution:getStationMean(Station,Type,Monitor),
  case P of
    {error, ErrMsg} -> {reply, ErrMsg, Monitor};
    Result -> {reply, Result, Monitor}
  end;
handle_call({getDailyMean, Date, Type}, _From, Monitor) ->
  P = pollution:getDailyMean(Date,Type,Monitor),
  case P of
    {error, ErrMsg} -> {reply, ErrMsg, Monitor};
    Result -> {reply, Result, Monitor}
  end;
handle_call({getMinMaxValue, Station, Type}, _From, Monitor) ->
  P = pollution:getMinMaxValue(Station,Type,Monitor),
  case P of
    {error, ErrMsg} -> {reply, ErrMsg, Monitor};
    Result -> {reply, Result, Monitor}
  end;
handle_call(crash, _From, _Monitor) -> 2/0.

handle_cast(stop, Value) ->
  {stop, normal, Value}.






