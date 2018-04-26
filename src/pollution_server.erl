%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2018 11:57
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Kamil").

%% API
-export([start/0,stop/0]).
-export([init/0]).
-export([addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,getDailyMean/2,getMinMaxValue/2,crash/0]).

start() -> register(pollutionServer, spawn_link(pollution_server,init,[])).

stop() -> pollutionServer ! stop.

init() ->
  loop(pollution:createMonitor()).

loop(Monitor) ->
  receive
    {request, Pid, addStation, {Name, Coords}} ->
      P = pollution:addStation(Name,Coords,Monitor),
      case P of
        {error, ErrMsg} -> Pid ! {reply, ErrMsg}, loop(Monitor);
        _ -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, addValue, {Station, Datetime, Type, Value}} ->
      P = pollution:addValue(Station,Datetime,Type,Value,Monitor),
      case P of
        {error, ErrMsg} -> Pid ! {reply, ErrMsg}, loop(Monitor);
        _ -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, removeValue, {Station, Datetime, Type}} ->
      P = pollution:removeValue(Station,Datetime,Type,Monitor),
      case P of
        {error, ErrMsg} -> Pid ! {reply, ErrMsg}, loop(Monitor);
        _ -> Pid ! {reply, ok}, loop(P)
      end;
    {request, Pid, getOneValue, {Station, Datetime, Type}} ->
      P = pollution:getOneValue(Station,Datetime,Type,Monitor),
      case P of
        {error, ErrMsg} -> Pid ! {reply, ErrMsg}, loop(Monitor);
        _ -> Pid ! {reply, P}, loop(Monitor)
      end;
    {request, Pid, getStationMean, {Station, Type}} ->
      P = pollution:getStationMean(Station,Type,Monitor),
      case P of
        {error, ErrMsg} -> Pid ! {reply, ErrMsg}, loop(Monitor);
        _ -> Pid ! {reply, P}, loop(Monitor)
      end;
    {request, Pid, getDailyMean, {Datetime, Type}} ->
      P = pollution:getDailyMean(Datetime,Type,Monitor),
      case P of
        {error, ErrMsg} -> Pid ! {reply, ErrMsg}, loop(Monitor);
        _ -> Pid ! {reply, P}, loop(Monitor)
      end;
    {request, Pid, getMinMaxValue, {Station, Type}} ->
      P = pollution:getMinMaxValue(Station,Type,Monitor),
      case P of
        {error, ErrMsg} -> Pid ! {reply, ErrMsg}, loop(Monitor);
        _ -> Pid ! {reply, P}, loop(Monitor)
      end;
    stop -> ok;
    crash -> 2/0

  end.

%%Client

call(Message, Parameters) ->
  pollutionServer ! {request, self(), Message, Parameters},
  receive
    {reply, Reply} -> Reply
  end.
addStation(Name, Coords) -> call(addStation,{Name, Coords}).
addValue(Station, Datetime,Type,Value) -> call(addValue, {Station, Datetime, Type, Value}).
removeValue(Station, Datetime, Type) -> call(removeValue, {Station, Datetime, Type}).
getOneValue(Station, Datetime, Type) -> call(getOneValue, {Station, Datetime, Type}).
getStationMean(Station, Type) -> call(getStationMean, {Station, Type}).
getDailyMean(Datetime, Type) -> call(getDailyMean, {Datetime, Type}).
getMinMaxValue(Station, Type) -> call(getMinMaxValue, {Station, Type}).
crash() -> pollutionServer ! crash.
