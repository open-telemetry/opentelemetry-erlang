%%%-------------------------------------------------------------------
%% @doc opentelemetry_experimental public API
%% @end
%%%-------------------------------------------------------------------

-module(opentelemetry_experimental_app).

-behaviour(application).

-export([start/2,
         prep_stop/1,
         stop/1]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

start(_StartType, _StartArgs) ->
    Config = otel_configuration:merge_with_os(
               application:get_all_env(opentelemetry_experimental)),

    {ok, Pid} = opentelemetry_experimental_sup:start_link(Config),

    {ok, _} = opentelemetry_experimental:start_meter_provider(?GLOBAL_METER_PROVIDER_NAME, Config),

    {ok, Pid}.

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
