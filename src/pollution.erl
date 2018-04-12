%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. kwi 2018 12:34
%%%-------------------------------------------------------------------
-module(pollution).
-author("Kamil").

%% API
-export([createMonitor/0,addStation/3,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,getMinMaxValue/3]).

%%structures
-record (measure, {date, type = "", value = 0}).
-record (station, {name = "Station", coords = {0, 0}, measurements = []}).

createMonitor() -> [].




%%addStation adds a station with a given name and coordinates, doesn't allow for duplicates, returns updated monitor.
addStation(StationName, Coords, Monitor) ->
  case findStation(StationName, Monitor) of
    false -> case findStation(Coords, Monitor) of
               false -> [#station{name = StationName, coords = Coords}| Monitor];
               _ -> {error, "Error: a station exists with same coordinates."}
             end;
    _ -> {error, "Error: a station with same name exists."}
  end.

findStation(_, []) -> false;
findStation(Station, [#station{name = Station} = S | _]) -> S;
findStation(Station, [#station{coords = Station} = S | _]) -> S;
findStation(Station, [_ | T]) -> findStation(Station, T).

%%addValue adds a a value to a station(being given coords or a station, date, type, value), returns updated monitor.
addValue(_,_,_,_,[]) -> [];
addValue(Station, {{Year, Month, Day},{Hour, Minutes, Seconds}}, Type, Value, Monitor) ->
  case findStation(Station, Monitor) of
    false -> {error, "Error: given station doesn't exist."};
    _ -> addIt(Station, {{Year, Month, Day} ,{Hour, Minutes, Seconds}}, Type, Value, Monitor)
  end.

addIt(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, [#station{name = Station, measurements = M} = S | T]) ->
  case getMeasure({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, M) of
    false -> [S#station{measurements = [#measure{date = {{Year, Month, Day}, {Hour, Minutes, Seconds}}, type = Type, value = Value} | M] }|T];
    _ -> io:format("Error: given measurement already exist."),
      [S|T]
  end;
addIt(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, [#station{coords = Station, measurements = M} = S | T]) ->
  case getMeasure({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, M) of
    false -> [S#station{measurements = [#measure{date = {{Year, Month, Day}, {Hour, Minutes, Seconds}}, type = Type, value = Value} | M]} | T];
    _ -> io:format("Error: given measurement already exist."),
      [S|T]
  end;
addIt(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, [S | T]) ->
  [S|addIt(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value, T)].


getMeasure(_,_, [] ) -> false;
getMeasure({{Year, Month, Day} , {Hour, Minutes, Seconds}}, Type,
    [#measure{date ={{Year, Month, Day} , {Hour, Minutes, Seconds}}, type = Type, value = V }| _]) -> V;
getMeasure({{Year, Month, Day} , {Hour, Minutes, Seconds}}, Type, [_|T]) ->
  getMeasure({{Year, Month, Day} , {Hour, Minutes, Seconds}}, Type, T).

%%removeValue removes measure from a station (coords/name, date, type), returns updated monitor;

removeValue(_,_,_,[]) -> [];
removeValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Monitor) ->
  case findStation(Station, Monitor) of
    false -> {error, "Error: given station doesn't exist."};
    _ -> removeIt(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Monitor)
  end.


removeIt(Station, {{Year, Month, Day} ,{ Hour, Minutes, Seconds}}, Type, [#station{name = Station, measurements = M } = S | T]) ->
  case getMeasure({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, M) of
    false -> io:format("Error: given measurement doesn't exist."),
      [S|T];
    _ -> [S#station{measurements = remIt({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type,M) }|T]
  end;
removeIt(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, [#station{coords = Station, measurements = M} = S | T]) ->
  case getMeasure({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, M) of
    false -> [S#station{measurements = remIt({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type,M)} | T];
    _ -> io:format("Error: given measurement already exist."),
      [S|T]
  end;
removeIt(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, [S | T]) ->
  [S|removeIt(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, T)].

remIt({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type,
    [#measure{date = {{Year, Month, Day}, {Hour, Minutes, Seconds}}, type = Type} | T]) -> T;
remIt({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, [_|T]) -> remIt({{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, T).


%%getOneValue returns value of measurement of given type from given date and station

getOneValue(_, _, _, []) -> {error, "Error: No such station."};
getOneValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}},Type, Monitor) ->
  case findStation(Station, Monitor) of
    false -> {error, "Error: No such station."};
    #station{measurements = M} -> getMeasure({{Year, Month, Day} , {Hour, Minutes, Seconds}}, Type, M)
  end.


%%getStationMean returns  mean value of parameter at a given station
getStationMean(Station,Type, Monitor) ->
  case findStation(Station, Monitor) of
    false -> {error, "Error: No such station."};
    #station{measurements = M} -> getMeanValue(lists:filter(fun (#measure{type = X}) ->X==Type  end, M),0,0)
  end.

getMeanValue([],_,0) -> 0;
getMeanValue([],Acc,Num) -> Acc/Num;
getMeanValue([#measure{value = Value}|T],Acc,Num) ->getMeanValue(T,Acc+Value,Num+1).

%%getDailyMean return mean value of a parameter of a given type on a given day on all stations;

getDailyMean(_,_,[]) -> 0;
getDailyMean({Year, Month, Day},Type, Monitor) ->
  getMeanValue(getSmth(Type,{Year, Month, Day}, Monitor),0,0).

unfold([]) -> [];
unfold([[]|T])->unfold(T);
unfold([[H|RT]|T])-> [H|unfold([RT|T])].

getSmth(Type,{Year, Month, Day}, Monitor) ->
  lists:filter(fun (#measure{type = Type1,date = {Year1,Month1,Day1,_}}) ->
    ((((Type1==Type) and (Year1==Year)) and (Month1==Month)) and (Day1==Day)) end,unfold(lists:map(fun (#station{measurements = M}) ->M  end, Monitor))).

goThroughStations(_,_,[],_,0) -> 0;
goThroughStations(_,_,[],Acc,Num) -> Acc/Num;
goThroughStations(Type,{Year,Month,Day},[#station{measurements = M}|T],Acc,Num) ->
  {Accum,Number} = goThroughMeasurements(Type,{Year,Month,Day},M,Acc,Num),
  goThroughStations(Type,{Year,Month,Day},T,Acc+Accum,Num+Number).


goThroughMeasurements(_,_,[],Acc,Num) -> {Acc,Num};
goThroughMeasurements(Type, {Year,Month,Day}, [#measure{type = Type, date = {{Year,Month,Day},_}, value = Val} | T], Acc, Num) ->
  goThroughMeasurements(Type, {Year,Month,Day}, T, Acc+Val, Num+1);
goThroughMeasurements(Type, {Year,Month,Day}, [_|T],Acc,Num) -> goThroughMeasurements(Type, {Year,Month,Day}, T,Acc,Num).

%%getMinMaxValue returns minimum and maximum value of a parameter of a given type on a given station (by coordinates).
getMinMaxValue(_,_,[])-> {error, "No such station"};
getMinMaxValue(Type, Station, Monitor) ->
  case findStation(Station,Monitor) of
    false -> {error, "No such station"};
    #station{measurements = M} -> findMinMax(lists:map(fun (#measure{value = Val}) -> Val end, lists:filter(fun (#measure{type = X}) ->X==Type  end, M)),{inf,'-inf'})
  end.

findMinMax([],{inf,'-inf'}) -> {error,"Error: no measurements."};
findMinMax([],{Min,Max}) -> {Min,Max};
findMinMax([H|T],{inf,'-inf'}) -> findMinMax(T,{H,H});
findMinMax([H|T],{Min,Max}) ->
  case H > Max of
    true -> findMinMax(T,{Min,H});
    false -> case H < Min of
               true -> findMinMax(T,{H,Max});
               false -> findMinMax(T,{Min,Max})
             end
  end.