%%%-------------------------------------------------------------------
%% @doc barcode_change public API
%% @end
%%%-------------------------------------------------------------------

-module(barcode_change_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = barcode_change_web:start(),
    barcode_change_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
