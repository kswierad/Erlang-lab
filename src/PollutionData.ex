defmodule DataPollution do

  def importLinesFromCSV() do
    File.read!("pollution.csv") |> String.split("\r\n")
  end

  def cutDate(line) do
    [day,month,year] = String.split(line,"-") |> Enum.map(fn (x) -> elem(Integer.parse(x),0) end)
    {year,month,day}
  end

  def cutTime(line) do
    [hour,minute] = String.split(line,":") |> Enum.map(fn (x) -> elem(Integer.parse(x),0) end)
    {hour,minute,0 }
  end

  def parseLine(line) do
    [date,time,langitude,longitude,val] = String.split(line,",")
    map = %{:datetime => {cutDate(date),cutTime(time)},
      :location => {elem(Float.parse(langitude),0),elem(Float.parse(longitude),0)},
      :pollutionLevel => elem(Integer.parse(val),0)}
  end

  def stationName({lat,long}) do
      string = "station_#{lat}_#{long}"
  end

  def fillServer() do
    importLinesFromCSV() |> Enum.map(&DataPollution.parseLine/1) |> Enum.map(fn (map) ->
                                                                                      :pollution_gen_server.AddStation(,map[:location]) end)
  end
end