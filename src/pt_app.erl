%%%-------------------------------------------------------------------
%% @doc pt public API
%% @end
%%%-------------------------------------------------------------------

-module(pt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pt_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
