%%%-------------------------------------------------------------------
%% @doc opentelemetry_experimental public API
%% @end
%%%-------------------------------------------------------------------

-module(opentelemetry_experimental_app).

-behaviour(application).

-export([start/2,
         stop/1]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

start(_StartType, _StartArgs) ->
    Config = otel_configuration:merge_with_os(
               application:get_all_env(opentelemetry_experimental)),

    SupResult = opentelemetry_experimental_sup:start_link(Config),
    case Config of
        #{sdk_disabled := true} ->
          %% skip the rest if the SDK is disabled
          SupResult;
        _ ->
            Resource = otel_resource_detector:get_resource(),
            {ok, _} = otel_meter_provider_sup:start(?GLOBAL_METER_PROVIDER_NAME, Resource, Config),

            SupResult
    end.

stop(_State) ->
    ok.

%% internal functions
