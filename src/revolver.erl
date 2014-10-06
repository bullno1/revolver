-module(revolver).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
	revolver_sup:start_link().

stop(_State) -> ok.
