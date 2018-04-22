%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2018 04:21
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Kamil").
-include_lib("eunit/include/eunit.hrl").

-record (measure, {date, type = "", value = 0}).
-record (station, {name = "Station", coords = {0, 0}, measurements = []}).

createMonitor_test() ->
  ?assertEqual([], pollution:createMonitor()).

addStation_test() ->
  Monitor = pollution:createMonitor(),
  Actual = pollution:addStation("X", {50, 18}, Monitor),
  Expected = [#station{name = "X",coords = {50,18},measurements = []}],
  ?assertEqual(Expected, Actual).

addValue_test() ->
  Monitor = pollution:createMonitor(),
  Monitor2 = pollution:addStation("X", {50, 18}, Monitor),
  Actual = pollution:addValue({50, 18}, {{2018, 4, 7}, {19, 00, 00}}, "PM10", 50, Monitor2),
  Expected =  [#station{name = "X",coords = {50, 18},measurements = [#measure{date = {{2018, 4, 7}, {19, 00, 00}},type =  "PM10",value =  50}]}],
  ?assertEqual(Expected, Actual).

removeValue_test() ->
  Monitor = pollution:createMonitor(),
  Monitor2 = pollution:addStation("X", {50, 18}, Monitor),
  Monitor3 = pollution:addValue({50, 18}, {{2018, 4, 7}, {19, 00, 00}}, "PM10", 50, Monitor2),
  Actual = pollution:removeValue("X", {{2018, 4, 7}, {19, 00, 00}}, "PM10", Monitor3),
  Expected = [#station{name = "X",coords = {50, 18},measurements = []}],
  ?assertEqual(Expected, Actual).

getOneValue_test() ->
  Monitor = pollution:createMonitor(),
  Monitor2 = pollution:addStation("X", {50, 18}, Monitor),
  Monitor3 = pollution:addValue({50, 18}, {{2018, 4, 7}, {19, 00, 00}}, "PM10", 50, Monitor2),
  Actual = pollution:getOneValue("X", {{2018, 4, 7}, {19, 00, 00}}, "PM10", Monitor3),
  Expected = 50,
  ?assertEqual(Expected, Actual).

getStationMean_test() ->
  Monitor = pollution:createMonitor(),
  Monitor2 = pollution:addStation("X", {50, 18}, Monitor),
  Monitor3 = pollution:addValue({50, 18}, {{2018, 4, 7}, {19, 00, 00}}, "PM10", 50, Monitor2),
  Monitor4 = pollution:addValue({50, 18}, {{2018, 4, 7}, {20, 00, 00}}, "PM10", 60, Monitor3),
  Actual = pollution:getStationMean("X", "PM10", Monitor4),
  Expected = 55.0,
  ?assertEqual(Expected, Actual).

getDailyMean_test() ->
  Monitor = pollution:createMonitor(),
  Monitor2 = pollution:addStation("X", {50, 18}, Monitor),
  Monitor3 = pollution:addStation("Y", {51, 19}, Monitor2),
  Monitor4 = pollution:addValue("X", {{2018, 4, 7}, {19, 00, 00}}, "PM10", 50, Monitor3),
  Monitor5 = pollution:addValue("X", {{2018, 4, 1}, {20, 00, 00}}, "PM10", 60, Monitor4),
  Monitor6 = pollution:addValue("Y", {{2018, 4, 7}, {16, 00, 00}}, "PM10", 10, Monitor5),
  Monitor7 = pollution:addValue("Y", {{2018, 4, 7}, {16, 00, 00}}, "TEMP", -30, Monitor6),
  Actual = pollution:getDailyMean({2018, 4, 7}, "PM10", Monitor7),
  Expected = 30.0,
  ?assertEqual(Expected, Actual).

getMinMaxValue_test() ->
  Monitor = pollution:createMonitor(),
  Monitor2 = pollution:addStation("X", {50, 18}, Monitor),
  Monitor3 = pollution:addValue("X", {{2018, 4, 6}, {19, 00, 00}}, "PM10", 50, Monitor2),
  Monitor4 = pollution:addValue("X", {{2018, 4, 7}, {19, 00, 00}}, "PM10", 60, Monitor3),
  Monitor5 = pollution:addValue("X", {{2018, 4, 7}, {16, 00, 00}}, "PM10", 10, Monitor4),
  Monitor6 = pollution:addValue("X", {{2018, 4, 8}, {16, 00, 00}}, "PM10", 20, Monitor5),
  Actual = pollution:getMinMaxValue("PM10",{50,18}, Monitor6),
  Expected = {10,60},
  ?assertEqual(Expected, Actual).
