%%%-------------------------------------------------------------------
%% @doc opentelemetry_experimental public API
%% @end
%%%-------------------------------------------------------------------

-module(opentelemetry_experimental_app).

-behaviour(application).

-export([start/2,
         prep_stop/1,
         stop/1]).

start(_StartType, _StartArgs) ->
    Opts = otel_configuration:merge_with_os(
             application:get_all_env(opentelemetry_experimental)),

    opentelemetry_experimental_sup:start_link(Opts).

%% called before the supervision tree is shutdown.
prep_stop(_State) ->
    %% on application stop set meter to the noop implementation.
    %% This is to ensure no crashes if the sdk isn't the last
    %% thing to shutdown or if the opentelemetry application crashed.
    opentelemetry_experimental:set_default_meter({otel_meter_noop, []}),
    ok.

stop(_State) ->
    ok.

%% internal functions
