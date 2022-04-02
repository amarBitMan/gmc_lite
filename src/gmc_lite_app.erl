%%%-------------------------------------------------------------------
%% @doc gmc_lite public API
%% @end
%%%-------------------------------------------------------------------

-module(gmc_lite_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = gmc_lite_web:start(),
    gmc_lite_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
