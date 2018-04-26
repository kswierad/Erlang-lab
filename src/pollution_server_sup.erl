%%%-------------------------------------------------------------------
%%% @author Kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. kwi 2018 11:34
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("Kamil").

%% API
-export([start_spv/0]).

start_spv() ->  spawn(fun supervisor/0).
supervisor() -> process_flag(trap_exit, true),
  loop().

loop() ->pollution_server:start(),
    receive
      {'EXIT', _, _} -> io:format("Wylecialem"), loop();
      stop -> terminate()
    end.

terminate() -> ok.