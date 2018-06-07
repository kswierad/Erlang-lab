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
addValue(Station, {{Year, Month, Day},{Hour, Minutes, Seconds}}, Type, Value, Monitor) ->
  P = findStation(Station, Monitor),
  case P of
    false -> {error, "Error: given station doesn't exist."};
    #station{measurements = M} -> case getMeasure({{Year, Month, Day},{Hour, Minutes, Seconds}}, Type, M) of
                                    false -> [P#station{measurements = [#measure{date = {{Year, Month, Day},{Hour, Minutes, Seconds}},type = Type, value = Value}|M]}|lists:delete(P,Monitor)];
                                    _ -> {error, "Error: given measurement already exist."}
                                  end
  end.

getMeasure(_,_, [] ) -> false;
getMeasure({{Year, Month, Day} , {Hour, Minutes, Seconds}}, Type,
    [#measure{date ={{Year, Month, Day} , {Hour, Minutes, Seconds}}, type = Type} = M| _]) -> M;
getMeasure({{Year, Month, Day} , {Hour, Minutes, Seconds}}, Type, [_|T]) ->
  getMeasure({{Year, Month, Day} , {Hour, Minutes, Seconds}}, Type, T).

%%removeValue removes measure from a station (coords/name, date, type), returns updated monitor;

removeValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Monitor) ->
  case findStation(Station, Monitor) of
    false -> {error, "Error: given station doesn't exist."};
    S =#station{measurements = MeasureList} -> case getMeasure({{Year, Month, Day},{Hour, Minutes, Seconds}}, Type, MeasureList) of
                                        false -> {error, "Error: given measurement doesn't exist."};
                                        Measure -> [S#station{measurements = lists:delete(Measure,MeasureList)}| lists:delete(S,Monitor)]
                                      end
  end.

%%getOneValue returns value of measurement of given type from given date and station

getOneValue(_, _, _, []) -> {error, "Error: No such station."};
getOneValue(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}},Type, Monitor) ->
  case findStation(Station, Monitor) of
    false -> {error, "Error: No such station."};
    #station{measurements = M} -> case getMeasure({{Year, Month, Day} , {Hour, Minutes, Seconds}}, Type, M) of
                                    false -> {error, "Error: given measurement doesn't exist."};
                                    #measure{value = Value} -> Value
                                  end
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
  getMeanValue(getMeasurementList(Type,{Year, Month, Day}, Monitor),0,0).

unfold([]) -> [];
unfold([[]|T])->unfold(T);
unfold([[H|RT]|T])-> [H|unfold([RT|T])].

getMeasurementList(Type,{Year, Month, Day}, Monitor) ->
  lists:filter(fun (#measure{type = Type1,date = {{Year1,Month1,Day1},_}}) ->
    ((((Type1==Type) and (Year1==Year)) and (Month1==Month)) and (Day1==Day)) end,unfold(lists:map(fun (#station{measurements = M}) ->M  end, Monitor))).

%%getMinMaxValue returns minimum and maximum value of a parameter of a given type on a given station (by coordinates).

getMinMaxValue(_,_,[])-> {error, "No such station"};
getMinMaxValue(Station, Type, Monitor) ->
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