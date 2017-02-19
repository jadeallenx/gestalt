-module(gestalt_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _Args) ->
    gestalt_sup:start_link().

stop(_Args) -> ok.
